# calculate historical mean temp over some years

calc_hsbs_years_vars <- function(hs_years, calc_bs_var, data, var, crop_season_extended_subset){
  
  apply(hs_years[,c("YEAR.START", "YEAR.END")], 1, 
        function(x) calc_bs_var(
          x[1], x[2], data, var, crop_season_extended_subset
        )
  )
  
}

# populate with 1980-1995 and 2000-2015
hs_years = data.frame(
  YEAR.START = c(seq(as.Date("1980-01-16"), as.Date("2015-01-16"), 'years')), 
  YEAR.END = c(seq(as.Date("1980-12-16"), as.Date("2015-12-16"), 'years')))

# # group by year, extract only years in periods 1980-1995, 2000-2015 (remove 1995-2000)

get_hs_vars <- function(data1, data2){
  library(dplyr)
  
  lapply(1:length(data1), function(i) {
    cbind(data1[[i]], data2[[i]][5]) %>%
      rename(bs_gs_tmp = 5, bs_gs_pre = 6) 
  }) %>% 
    data.table::rbindlist() %>% 
    filter(year %in% c(1980:1995, 2000:2015)) %>% 
    # group and average over periods, except for 2000-2015 which will be represented as a time series
    mutate(period = ifelse(year %in% c(1980:1995), "1980_1995", year)) %>% 
    group_by(lon, lat, period, crop_pooled) %>% 
    summarise(mean_bs_gs_tmp = mean(bs_gs_tmp, na.rm=T),
              mean_bs_gs_pre = mean(bs_gs_pre, na.rm=T)) #%>%
  
  
}

calc_hs_annual_change <- function(hs_tmp_pre){
  library(dplyr)
  # need to create column that holds only the mean tmp and pre from 1980-1995
  # mutate column mean_bs_gs_tmp_1980_1995 that is equal to
  # value in column mean_bs_gs_tmp at value in column period == "1980_1995"
  mean_bs_gs_tmp_1980_1995 <- hs_tmp_pre %>% 
    group_by(lon, lat, period, crop_pooled) %>%
    filter(period=="1980_1995") %>% 
    dplyr::select(mean_bs_gs_tmp) %>% 
    rename(mean_bs_gs_tmp_1980_1995 = mean_bs_gs_tmp)
  
  mean_bs_gs_pre_1980_1995  <- hs_tmp_pre %>% 
    group_by(lon, lat, period, crop_pooled) %>% 
    filter(period=="1980_1995")  %>%
    dplyr::select(mean_bs_gs_pre) %>% 
    rename(mean_bs_gs_pre_1980_1995 = mean_bs_gs_pre)
  
  hs_tmp_pre %>% 
    left_join(mean_bs_gs_tmp_1980_1995, 
              by = c("lon", "lat", "crop_pooled")) %>% 
    left_join(mean_bs_gs_pre_1980_1995, 
              by = c("lon", "lat", "crop_pooled")) %>% 
    mutate(Temp.Change = mean_bs_gs_tmp - mean_bs_gs_tmp_1980_1995,
           Precipitation.change = mean_bs_gs_pre - mean_bs_gs_pre_1980_1995) %>%
    rename(
      bs_gs_tmp = mean_bs_gs_tmp_1980_1995,
      bs_gs_pre = mean_bs_gs_pre_1980_1995
    ) %>%
    ungroup() %>%
    group_by(crop_pooled) %>%
    group_split()
}



calculate_hs_co2 <- function(hs_co2){
  library(dplyr)
  
  hs_co2 <- hs_co2 %>% 
    filter(year %in% c(1980:1995, 2000:2015)) %>% 
    mutate(period = ifelse(year %in% c(1980:1995), "1980_1995", year)) %>% 
    group_by(period) %>% 
    summarise(CO2 = mean(mean, na.rm=T)) 
  
  hs_bs_co2 <- hs_co2 %>% 
    filter(period=="1980_1995") %>% 
    dplyr::select(CO2) %>% 
    rename(co2_1980_1995 = CO2)
  
  hs_co2 %>% mutate(co2_1980_1995 = hs_bs_co2$co2_1980_1995,
                    CO2_change = CO2-co2_1980_1995,
                    f_CO2_C3 = CO2_change/(CO2_change + 100), # allows for declining marginal effect of CO2, from Moore
                    f_CO2_C4 = CO2_change/(CO2_change + 50))
  
}

create_hs_prediction_data <- function(hs_vars, hs_co2_change){
  lapply(1:4, function(crop){
    hs_vars[[crop]] %>% 
      mutate(f_CO2 = ifelse(C3==1, hs_co2_change$f_CO2_C3, hs_co2_change$f_CO2_C4)) %>% 
      dplyr::filter(complete.cases(.))# %>% 
    # ungroup() %>% 
    # group_by(period) %>% 
    # group_split(period)
  })
}

predict_level_5_hs <- function(fit, data, ncores){
  future::plan(future::multisession, workers = ncores)
  # may need to change these to 1:5?
  p <- future.apply::future_lapply(1:5, function(i) { # spec
    gammit::predict_gamm(fit[[1]][[i]], # 4 of these with 25 fits each
                         data[[1]],
                         re_form = c("s(Country2_fact)"),
                         keep_prediction_data = TRUE,
                         newdata.guaranteed = TRUE,
                         se.fit = TRUE) %>% 
      mutate(model_spec=attr(fit[[1]][[i]], "model_specs"),
             m=attr(fit[[1]][[i]], "imputation")) 
    
    
  })
  rbindlist(p)
}

pool_across_m_hs <- function(predictions, model_specs){
  
  imp <- 5 # note this cannot be same as idcol name! otherwise where m is computed will not work/collapse
  
  # read in level 5 prediction data
  t <- predictions %>%
    rbindlist() %>% 
    group_by(lon, lat, crop_pooled, model_spec, period) %>% # to identify iteration
    summarise(fit_bar = mean(prediction.fit, na.rm=TRUE), # pooled mean predictions
              v_w     = mean(prediction.se.fit.x^2, na.rm=TRUE), # Rubin's Rules for pooling within-m variance
              v_b     = sum(prediction.fit - fit_bar)^2 / (imp - 1), # pooling between-m variance
              v_p     = v_w + v_b * (1 + (1 / imp)), # overall variance
              se_p    = sqrt(v_p)) %>% # standard error
    # use the p_suffix to indicate these are pooled
    mutate(lwr_p = fit_bar - se_p * 1.96, # assuming normally distributed imputations
           upr_p = fit_bar + se_p * 1.96)
  
  df <- data.frame(model_spec=model_specs,
                   model_specs=c("gam_RS",
                                 "gam_RI",
                                 "glm_RS",
                                 "glm_RI",
                                 "lm"))
  t %>% left_join(df, by=c("model_spec")) %>%
    dplyr::select(!model_spec)
}



# average across 2000-2015 ------------------------------------------------


get_hs_avg_vars <- function(data1, data2){
  library(dplyr)
  
  lapply(1:length(data1), function(i) {
    cbind(data1[[i]], data2[[i]][5]) %>%
      rename(bs_gs_tmp = 5, bs_gs_pre = 6) 
  }) %>% 
    data.table::rbindlist() %>% 
    filter(year %in% c(1980:1995, 2000:2015)) %>% 
    # group and average over periods, except for 2000-2015 which will be represented as a time series
    mutate(period = case_when(year %in% c(1980:1995) ~ "1980_1995",
                              year %in% c(2000:2015) ~ "2000_2015")) %>% 
    group_by(lon, lat, period, crop_pooled) %>% 
    summarise(mean_bs_gs_tmp = mean(bs_gs_tmp, na.rm=T),
              mean_bs_gs_pre = mean(bs_gs_pre, na.rm=T)) %>% 
    tidyr::pivot_wider(names_from = period,
                       values_from = c(mean_bs_gs_pre,
                                       mean_bs_gs_tmp),
                       names_expand=TRUE) %>% 
    mutate(Temp.Change = mean_bs_gs_tmp_2000_2015 - mean_bs_gs_tmp_1980_1995,
           Precipitation.change = mean_bs_gs_pre_2000_2015 - mean_bs_gs_pre_1980_1995) %>%
    rename(
      bs_gs_tmp = mean_bs_gs_tmp_1980_1995,
      bs_gs_pre = mean_bs_gs_pre_1980_1995
    ) %>%
    ungroup() %>%
    group_by(crop_pooled) %>%
    group_split()
  
  
}


create_bs_vars <- function(data1, data2){
  
  bs_vars <- lapply(1:4, function(i) {
    data1[[i]] %>% 
      left_join(data2, by=c("lon","lat")) %>%
      rename(Country2_fact = ISO_A2,
             Country_name = ADMIN) %>%
      mutate(Country2_fact=na_if(Country2_fact, "-99"),
             #Country_int=ifelse(Country_int=="<NA>",NA,Country_int),
             #Country_name=ifelse(Country_name=="<NA>",NA,Country_name),
             adapt_dummy=0,
             adapt_dummy=as.factor(adapt_dummy),
             C3=0,
             C4=0) %>% 
      rename(Baseline_tmp = bs_gs_tmp,
             Baseline_pre = bs_gs_pre,
             Baseline_yield = bs_yield) %>% 
      data.table::as.data.table()
  })
  
  bs_vars[[1]] <- bs_vars[[1]] %>% mutate(C4=1)
  bs_vars[[2]] <- bs_vars[[2]] %>% mutate(C3=1)
  bs_vars[[3]] <- bs_vars[[3]] %>% mutate(C3=1)
  bs_vars[[4]] <- bs_vars[[4]] %>% mutate(C3=1)
  
  bs_vars
  
}


calculate_hs_avg_co2 <- function(hs_co2){
  library(dplyr)
  
  hs_co2 <- hs_co2 %>% 
    filter(year %in% c(1980:1995, 2000:2015)) %>% 
    mutate(period = case_when(year %in% c(1980:1995) ~ "1980_1995", 
                              year %in% c(2000:2015) ~ "2000_2015")) %>% 
    group_by(period) %>% 
    summarise(CO2 = mean(mean, na.rm=T)) 
  
  
  hs_co2 %>% tidyr::pivot_wider(names_from=period,
                                values_from=CO2) %>% 
    mutate(CO2_change = `2000_2015` - `1980_1995`,
           f_CO2_C3 = CO2_change/(CO2_change + 100), # allows for declining marginal effect of CO2, from Moore
           f_CO2_C4 = CO2_change/(CO2_change + 50))
  
}


create_hs_prediction_data <- function(hs_vars, hs_co2_change){
  lapply(1:4, function(crop){
    hs_vars[[crop]] %>% 
      mutate(f_CO2 = ifelse(C3==1, hs_co2_change$f_CO2_C3, hs_co2_change$f_CO2_C4)) %>% 
      dplyr::filter(complete.cases(.))# %>% 
    # ungroup() %>% 
    # group_by(period) %>% 
    # group_split(period)
  })
}

predict_level_5_avg_hs <- function(fit, data, ncores){
  future::plan(future::multisession, workers = ncores)
  # may need to change these to 1:5?
  p <- future.apply::future_lapply(1:25, function(i) { # spec
    gammit::predict_gamm(fit[[1]][[i]], # 4 of these with 25 fits each
                         data[[1]],
                         re_form = c("s(Country2_fact)"),
                         keep_prediction_data = TRUE,
                         newdata.guaranteed = TRUE,
                         se.fit = TRUE) %>% 
      mutate(model_spec=attr(fit[[1]][[i]], "model_specs"),
             m=attr(fit[[1]][[i]], "imputation")) 
    
    
  })
  rbindlist(p)
}


pool_across_m_hs_avg <- function(predictions, model_specs){
  
  imp <- 5 # note this cannot be same as idcol name! otherwise where m is computed will not work/collapse
  
  # read in level 5 prediction data
  t <- predictions %>%
    # rbindlist() %>% 
    group_by(lon, lat, crop_pooled, model_spec) %>% # to identify iteration
    summarise(fit_bar = mean(prediction.fit, na.rm=TRUE), # pooled mean predictions
              v_w     = mean(prediction.se.fit.x^2, na.rm=TRUE), # Rubin's Rules for pooling within-m variance
              v_b     = sum(prediction.fit - fit_bar)^2 / (imp - 1), # pooling between-m variance
              v_p     = v_w + v_b * (1 + (1 / imp)), # overall variance
              se_p    = sqrt(v_p)) %>% # standard error
    # use the p_suffix to indicate these are pooled
    mutate(lwr_p = fit_bar - se_p * 1.96, # assuming normally distributed imputations
           upr_p = fit_bar + se_p * 1.96)
  
  t
  # 
  # df <- data.frame(model_spec=model_specs,
  #                  model_specs=c("gam_RS",
  #                                "gam_RI",
  #                                "glm_RS",
  #                                "glm_RI",
  #                                "lm"))
  # t %>% left_join(df, by="model_spec") %>%
  #   dplyr::select(!model_spec)
}


rasterise_hs_predictions <- function(predictions){ # predictions_nested - this produces a raster stack?
  lapply(1:5, function(model){ 
    lapply(1:4, function(crop){  
      
      x <- predictions[[model]][[crop]]
      r <- x[c("lon","lat","fit_bar")]
      
      
      r <- raster::rasterFromXYZ(r)
      crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
      
      r
    })
    
    # these don't span entire globe? ymin -55 only not -90!!?
    # terra::rast(r_2021_2040, "xyz") throws an error
    #https://future.futureverse.org/articles/future-4-non-exportable-objects.html#packages-that-rely-on-external-pointers
  }) 
  
  
}


plot_hs_predictions <- function(predictions, returnStack, World, model_spec_alphabetical, crops, path){
  lapply(1:5, function(model){
    p <- lapply(1:4, function(crop){
      
      tmap::tmap_mode("plot")
      
      
      tmap::tm_shape(predictions[[model]][[crop]]) + 
        tmap::tm_raster("fit_bar", title="Pooled fit (%)",
                        style = "cont",
                        breaks=seq(-100,100,10), 
                        # ^ this is chosen manually to disregard skewing/scaling effects of Siwa desert outliers
                        # remove range restriction when we remove outliers in the prediction data
                        palette = rev(terrain.colors(100))) +
        tmap::tm_shape(World) +
        tmap::tm_borders("grey", lwd =1) + 
        tmap::tm_layout(panel.labels = crops[[crop]])
      
    })
    
    plot <- tmap::tmap_arrange(p[[1]], p[[2]], p[[3]], p[[4]])
    
    
    # define filename
    outfile <- sprintf(path, 
                       model_spec_alphabetical[[model]])
    
    # save
    tmap::tmap_save(plot, filename=outfile, height=4, width=10, asp=0)
    
    # show predictions again
    if(isTRUE(returnStack)) {
      plot
    } else {
      outfile
    }
    
  })
}

# extract GDHY time series data



get_gdhy_yield_data <- function(crops_lwr){
  
  lapply(1:4, function(j){
    
    lapply(1:35, function(i){
      
      library(dplyr)
      
      crop_yields_ts <- list.files(here::here("data", "GDHY data", crops_lwr[[j]]), pattern = "^.*\\.(nc4)$")
      
      data <- ncdf4::nc_open(here::here("data", "GDHY data", crops_lwr[[j]], crop_yields_ts[[i]]))
      
      lon <- ncdf4::ncvar_get(data, "lon")
      lat <- ncdf4::ncvar_get(data, "lat")
      yields <- ncdf4::ncvar_get(data, "var")
      
      fillvalue <- ncdf4::ncatt_get(data, "var", "_FillValue")
      
      yields[yields == fillvalue$value] <- NA
      
      # set dimension names and values to corresponding lon and lat values
      dimnames(yields) <- list(lon = lon, lat = lat)
      
      # transform into dataframe
      colnames(yields) <- c(seq(-89.75, 89.75, 0.5)) # lat
      rownames(yields) <- c(seq(0.25, 359.75, 0.5)) # lon, this needs to be converted to -180 to 180 # -179.75, 179.75, 0.5 # 0.25, 359.75
      
      yields <- reshape2::melt(yields)
      
      yields %>% rename(bs_yield=value) %>% 
        mutate(lon = ifelse(lon > 180, -(360-lon), lon)) %>% 
        data.table::as.data.table()
      
    }
    )
  })
}


plot_gdhy_yields <- function(predictions, returnStack, World, crops, path){
    p <- lapply(1:4, function(crop){
      
      tmap::tmap_mode("plot")
      
      
      tmap::tm_shape(predictions[[crop]]) + 
        tmap::tm_raster("Yield.Change", title="Yield Change (%)",
                        style = "cont",
                        breaks=seq(-100,100,10), 
                        # ^ this is chosen manually to disregard skewing/scaling effects of Siwa desert outliers
                        # remove range restriction when we remove outliers in the prediction data
                        palette = rev(terrain.colors(100))) +
        tmap::tm_shape(World) +
        tmap::tm_borders("grey", lwd =1) + 
        tmap::tm_layout(panel.labels = crops[[crop]])
      
    })
    
    plot <- tmap::tmap_arrange(p[[1]], p[[2]], p[[3]], p[[4]])
    
    
    # define filename
    outfile <- sprintf(path)
    
    # save
    tmap::tmap_save(plot, filename=outfile, height=4, width=10, asp=0)
    
    # show predictions again
    if(isTRUE(returnStack)) {
      plot
    } else {
      outfile
    }
  
}

  