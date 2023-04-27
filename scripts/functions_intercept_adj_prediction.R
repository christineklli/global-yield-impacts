
# create prediction data for zero change variables
# zero temperature, precipitation and CO2 change

create_int_adj_prediction_data <- function(prediction_data, time_periods, GCMs){
  
  # create dataframe with same variables as prediction_data_complete_cases
  maize <- prediction_data[[1]][[1]][[1]]
  rice <- prediction_data[[2]][[1]][[1]]
  soy <- prediction_data[[3]][[1]][[1]]
  wheat <- prediction_data[[4]][[1]][[1]]
  
  l <- list(maize, rice, soy, wheat)
  
  # void change vars
  l_nested <- lapply(1:4, function(i){
    df <- l[[i]] %>% 
      mutate(Temp.Change=0,
             Precipitation.change=0,
             f_CO2=0)   
    # rep by gcm
    df_a <- purrr::map(seq_len(24), ~df) %>% 
      bind_rows(.id="gcm_no")
    
    # rep by time_period
    df_b <- purrr::map(seq_len(4), ~df_a) %>% 
      bind_rows(.id="time_period_no")
    
    time_period_concord <- data.frame(time_period_no=seq(1,4,1), 
                                      time_period=time_periods) %>% 
      mutate(time_period_no=as.character(time_period_no))
    
    gcm_concord <- data.frame(gcm_no=seq(1,24,1),
                              gcm=GCMs) %>% 
      mutate(gcm_no=as.character(gcm_no))
    
    df_b <- df_b %>% 
      filter(gcm_no!=3) %>% # remove third GCM
      left_join(time_period_concord, by="time_period_no") %>% 
      left_join(gcm_concord, by="gcm_no") %>% 
      dplyr::select(!c("time_period_no","gcm_no"))
    
    # split by timeperiod so that we have 4 nested lists of 4 each (i.e. 16 lists)
    split(df_b, df_b$time_period)
  })
  l_nested
  
}


adjust_prediction_level_5 <- function(prediction_og, prediction_adj){
  prediction_og %>% 
    left_join(
      prediction_adj, by=c(
        "lon","lat", "crop_pooled", "time_period","gcm","model_spec","m", 
        "Baseline_tmp", "Baseline_pre", "Baseline_yield", "C3", "C4", "adapt_dummy", "Country2_fact")
      # there will be duplicated variables for Temp.Change; Precipitation.change, f_CO2 as intended
    ) %>% 
    mutate(prediction.fit=prediction.fit.x-prediction.fit.y)
  #^ same prediction.fit so as to be able to use pooling functions as they are
}


pool_across_m_int_adj <- function(predictions){
  
  imp <- 5 # note this cannot be same as idcol name! otherwise where m is computed will not work/collapse
  
  # read in level 5 prediction data
  predictions %>%
    
    group_by(lon, lat, crop_pooled, model_spec, time_period, gcm) %>% # to identify iteration
    summarise(fit_bar = mean(prediction.fit, na.rm=TRUE), # pooled mean predictions
              v_w     = mean(prediction.se.fit.x^2, na.rm=TRUE), # Rubin's Rules for pooling within-m variance
              v_b     = sum(prediction.fit - fit_bar)^2 / (imp - 1), # pooling between-m variance
              v_p     = v_w + v_b * (1 + (1 / imp)), # overall variance
              se_p    = sqrt(v_p)) %>% # standard error
    # use the p_suffix to indicate these are pooled
    mutate(lwr_p = fit_bar - se_p * 1.96, # assuming normally distributed imputations
           upr_p = fit_bar + se_p * 1.96)
}

create_global_predictions_tbl <- function(predictions, 
                                          crop_production_raster_agg, 
                                          worldmap_clean, 
                                          model_spec_alphabetical, 
                                          crops, 
                                          path){
  df <- lapply(1:5, function(model){ #
    lapply(1:4, function(crop){ # time period
      
      prediction <- exactextractr::exact_extract(predictions[[model]][[crop]], # this is a stack
                                                 worldmap_clean, 
                                                 'weighted_mean', 
                                                 weights = crop_production_raster_agg[[crop]]) 
      
      # calculate area of each country
      
      worldmap_clean@data$area_sqkm <- raster::area(worldmap_clean)/1000000
      
      tbl <- data.frame(ADMIN=worldmap_clean@data$ADMIN,
                        ISO_A2=worldmap_clean@data$ISO_A2,
                        area_sqkm=worldmap_clean@data$area_sqkm,
                        prediction)
      
      # calculate global area-weighted mean prediction
      
      tbl %>% 
        summarise(
        wt.mean.2021.2040 = weighted.mean(weighted_mean.X2021.2040, area_sqkm, na.rm=T),
        wt.mean.2041.2060 = weighted.mean(weighted_mean.X2041.2060, area_sqkm, na.rm=T),
        wt.mean.2061.2080 = weighted.mean(weighted_mean.X2061.2080, area_sqkm, na.rm=T),
        wt.mean.2081.2100 = weighted.mean(weighted_mean.X2081.2100, area_sqkm, na.rm=T),
        model = model_spec_alphabetical[[model]],
        crop_name = crops[[crop]]
      ) 
      
    }) 
  }) 
  
  lapply(1:5, function(model){
    rbindlist(df[[model]])
    }) %>% 
    rbindlist() %>% 
    readr::write_csv(sprintf(path))
}


rasterise_conf_gridded <- function(predictions, time_periods, var){ # predictions_nested - this produces a raster stack?
  lapply(1:5, function(model){ 
    lapply(1:4, function(crop){  
      
      x <- predictions[[model]][[crop]]
      
      x_list <- lapply(1:4, function(i){
        
        r <- x %>% filter(time_period==time_periods[[i]]) %>% 
          dplyr::select(c("lon","lat",!!sym(var))) # quasiquotation can only be used in tidyverse functions
        
        r <- raster::rasterFromXYZ(r)
        crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
        names(r) <- time_periods[[i]]
        r
      })
      
      raster::stack(x_list)
      
    })
    
   
  }) 
  
  
}