
manipulate_crop_season <- function(data, raster){
  
  list <- lapply(1:15, function(i){
    matrix <- raster[[i]][] # turn raster of full 259200 grid cells for 15 crop varieties into matrix
    matrix[,1] <- unique(data$crop)[i] # crop name
    matrix <- as.data.frame(matrix)
    matrix$lon <- data$lon[1:259200] # full list replicated x 7 crops
    matrix$lat <- data$lat[1:259200]
    matrix
  })
  
  df <- rbindlist(list)
  
  cols <- c("n_growing_months", "jan_growing", "feb_growing", "mar_growing", "apr_growing", "may_growing", "jun_growing", "jul_growing", "aug_growing", "sept_growing", "oct_growing", "nov_growing", "dec_growing")
  
  df[,(cols):=lapply(.SD, as.numeric), .SDcols=cols]
  
  df %>% 
    filter(crop %in% c("Rice", "Rice (Rainfed)", "Rice (Irrigated)", 
                       "Wheat", "Wheat.Winter", "Wheat (Rainfed)", "Wheat (Irrigated)", 
                       "Maize", 
                       "Soybeans"))
  
  
}

# read in CRU temp data for 2011-2020
# can use this function for tmp and precip

create_cru_prediction_data <- function(file, var_name){
  
  data <- ncdf4::nc_open(here("data", "CRU data", file)) 
  
  var <- ncdf4::ncvar_get(data, attributes(data$var)$names[1])
  lon <- ncdf4::ncvar_get(data,attributes(data$dim)$names[1])
  lat <- ncdf4::ncvar_get(data, attributes(data$dim)$names[2])
  time <- ncdf4::ncvar_get(data, attributes(data$dim)$names[3])
  atts <- ncdf4::ncatt_get(data, 0)
  
  dimnames(var) <- list(lon = lon, 
                         lat = lat, 
                         time = time)
  
  df <- reshape2::melt(var, value.name = var_name) 
  df$date <- as.Date(df$time, origin = "1900-1-1")
  df %>% dplyr::select(!time)
  
}

# calculate mean annual temp and precip for 2015 

mean_cru_year_var <- function(data, var, var_2015){
  
  data %>% 
  as.data.table() %>% 
  filter(date >="2015-01-16" & date <= "2015-12-16") %>% 
  group_by(lon,lat) %>% 
  summarise(var_2015 = mean(.data[[var]], na.rm=TRUE)) %>% 
  ungroup()
  
}

# calculate baseline average growing season temp and precip for each year of 2015-2020 from CRU data 

calc_bs_var <- function(YEAR.START, YEAR.END, data, var, crop_season_extended_subset) { # note that i relates to tmp, pre data
  
  YEAR <- data %>% 
    dplyr::filter(date >= as.Date(YEAR.START) & date <= as.Date(YEAR.END)) # this changes dynamically
  
  # need to reshape wide by month
  YEAR_WIDE <- tidyr::spread(YEAR, date, eval(rlang::parse_expr(var)))
  colnames(YEAR_WIDE) <- paste(var, colnames(YEAR_WIDE), sep = "_")
  YEAR_WIDE <- YEAR_WIDE %>% dplyr::rename(lon = 1, lat = 2)
  
  # merge by lon lat
  CROP_YEAR <- merge(crop_season_extended_subset, YEAR_WIDE, 
                     by = c("lon", "lat"),
                     all.x = T)
  
  # convert climate vars to months (rename colnames) (jan_var, ... , dec_var)
  
  CROP_YEAR <- CROP_YEAR %>% 
    rename(jan_var = 19,
           feb_var = 20,
           mar_var = 21,
           apr_var = 22,
           may_var = 23,
           jun_var = 24,
           jul_var = 25,
           aug_var = 26,
           sept_var = 27,
           oct_var = 28,
           nov_var = 29,
           dec_var = 30)
  
  # created weighted average
  CROP_YEAR <- CROP_YEAR %>% 
    mutate(avg_growing_var = (jan_var*jan_growing + 
                                feb_var*feb_growing +
                                mar_var*mar_growing +
                                apr_var*apr_growing +
                                may_var*may_growing +
                                jun_var*jun_growing +
                                jul_var*jul_growing +
                                aug_var*aug_growing +
                                sept_var*sept_growing +
                                oct_var*oct_growing +
                                nov_var*nov_growing +
                                dec_var*dec_growing)/n_growing_months)
  
  AVG_YEAR <- CROP_YEAR %>% 
    dplyr::select(lon, lat, crop, avg_growing_var) %>% 
    mutate(year = year(YEAR.START)) %>%  
    
    # now average over sub-crop variety baseline temperatures
    # for each dataframe, mutate new variable with major crop variety level, group_by major crop level and take average baseline temperatures over the sub-crop-variety temperatures
    mutate(crop_pooled = case_when(crop %in% c("Wheat", "Wheat.Winter", "Wheat (Rainfed)", "Wheat (Irrigated)") ~ "Wheat",
                                   crop %in% c("Rice", "Rice (Irrigated)", "Rice (Rainfed)") ~ "Rice",
                                   crop == "Maize" ~ "Maize",
                                   crop %in% c("Soybean", "Soybeans") ~ "Soybean"),
           #year = as.factor(year)
    ) %>%
    group_by(lon, lat, crop_pooled, year) %>% 
    summarise(bs_gs_var = mean(avg_growing_var, na.rm = TRUE)) %>%  
    ungroup() %>% 
    mutate_all(~ifelse(is.nan(.), NA, .)) 
  
}

# argument
bs_years = data.frame(YEAR.START = c("2015-01-16", "2016-01-16", "2017-01-16", "2018-01-16", "2019-01-16", "2020-01-16"), 
                      YEAR.END = c("2015-12-16", "2016-12-16", "2017-12-16", "2018-12-16", "2019-12-16", "2020-12-16"))


# calculate baseline growing season temperature averaged over 2015-2020

calc_bs_years_vars <- function(bs_years, calc_bs_var, data, var, crop_season_extended_subset){
  
  apply(bs_years[,c("YEAR.START", "YEAR.END")], 1, function(x) calc_bs_var(x[1], x[2], data, var, crop_season_extended_subset))
  
}


# cbind precip into the same dataframe as temp for each of 2015-2020 (6) tables in the list
# note cbind here instead of join by is ok because temp and precip data have exactly the same format

combine_bs_years_tmp_pre <- function(data1, data2){
  
  lapply(1:6, function(i) {
  cbind(data1[[i]], data2[[i]][5]) %>%
    rename(bs_gs_tmp = 5, bs_gs_pre = 6) %>% 
    dplyr::select(!c("year")) %>% 
    as.data.table()}) 
  
}

# focus on 2015 - extract first element dataframe

extract_bs_2015_tmp_pre <- function(data){
  # extract 2015, first element in list
  data[[1]] %>% 
   # group_split into list by crop_pooled 
  group_split(crop_pooled)
}

# rasterise and plot bs 2015 tmp and pre by crop

plot_bs_2015_tmp_pre <- function(data, var, World, title, palette, outfile){
 
  x <- lapply(1:4, function(i){
    x <- data[[i]] %>% 
      dplyr::select(c(lon, lat, eval(var)))
    
    rasterFromXYZ(x)
  })
  
  y <- stack(x)
 
  plot <- tmap::tm_shape(y) +
    tmap::tm_raster(palette=palette, 
            midpoint=NA,
            title= title) +
    tmap::tm_facets() +
    tmap::tm_shape(World) +
    tmap::tm_borders("grey", lwd =1) +
    tmap::tm_layout(legend.outside=TRUE,
              legend.outside.position = c("right"),
              panel.labels=c("Maize","Rice","Soybean","Wheat"))
  
 tmap::tmap_save(plot, filename=outfile, height=4, width=7, asp=0)
  plot
  
}



# rasterise and stack monfreda yields data

rasterise_crop_yield_monf <- function(files){
  
  crop_yield_monf_raster <- lapply(files, raster::raster)
  
  r <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, crs=4326, res=0.5)
  res.factor <- raster::res(r)/raster::res(crop_yield_monf_raster[[1]])
  
  lapply(1:4, function(i){
    raster::aggregate(crop_yield_monf_raster[[i]], fact = res.factor, fun = mean)
  }
  )
}

make_crop_yield_monf_dt <- function(raster){
   lapply(1:4, function(i) {
  raster::as.data.frame(raster[[i]], xy=TRUE) %>% 
    rename(lon=x, lat=y, bs_yield=3) %>% 
       mutate(lon=round(lon,2), lat=round(lat,2)) %>% 
    as.data.table})
 }

# merge tmp, precip and yield data for baseline year 2015

merge_cru_monf_prediction_data <- function(data1, data2){
  lapply(1:4, function(i){
  data1[[i]] %>% 
    left_join(data2[[i]], 
              by=c("lon","lat"))
})
}

# add country and adapt dummy 

coords2admin <-  function(points){  
  
  countriesSP <- getMap(resolution='low')
 
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  indices$ADMIN  

  
}

coords2iso <-  function(points){  
  
  countriesSP <- getMap(resolution='low')

  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  indices$ISO_A2 # returns the ISO2 code 
  
}

identify_points <- function(data){
  data[[1]] %>% 
    dplyr::select(lon, lat)
}

combine_coords <- function(data1,data2,data3){
  
  cbind(data1,data2,data3) %>%  
  rename(ADMIN = 3, ISO_A2 = 4)
  
}

# create bs_2015_vars

create_bs_2015_vars <- function(data1, data2){
  
  bs_2015_vars <- lapply(1:4, function(i) {
    data1[[i]] %>% 
      left_join(data2, by=c("lon","lat")) %>%
      rename(Country2_fact = ISO_A2,
             Country_name = ADMIN) %>%
      mutate(Country2_fact=na_if(Country2_fact, "-99"),
             adapt_dummy=0,
             adapt_dummy=as.factor(adapt_dummy),
             C3=0,
             C4=0) %>% 
      rename(Baseline_tmp = bs_gs_tmp,
             Baseline_pre = bs_gs_pre,
             Baseline_yield = bs_yield) %>% 
      as.data.table
  })
  
  bs_2015_vars[[1]] <- bs_2015_vars[[1]] %>% mutate(C4=1)
  bs_2015_vars[[2]] <- bs_2015_vars[[2]] %>% mutate(C3=1)
  bs_2015_vars[[3]] <- bs_2015_vars[[3]] %>% mutate(C3=1)
  bs_2015_vars[[4]] <- bs_2015_vars[[4]] %>% mutate(C3=1)
  
  bs_2015_vars
  
}



create_cmip6_pre_df <- function(time_periods){
  
  lapply(1:4, function(i){
    cmip6_files <- list.files(path = here("data","CMIP6 data",time_periods[[i]]), pattern = "^.*\\.(tif|TIF|Tif)$")
    cmip6_data <- here("data", "CMIP6 data", time_periods[[i]], cmip6_files)
    # each GCM is one rasterlayer - and each rasterlayer has 19 bands - select 1st and 12th band for each rasterlayer
    cmip6_pr_annual <- lapply(1:24, function(i){
      raster(cmip6_data[i],band=12)
    })
    # transform to annual average mean by dividing by 12
    cmip6_pre_pre <- lapply(1:24, function(i){
      calc(cmip6_pr_annual[[i]], fun=function(x){x/12})
    })
    # aggregate spatial resolution to 0.5x0.5 degrees 
    r_object <- raster(res = c(0.5, 0.5), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    res.factor <- raster::res(r_object)/raster::res(cmip6_pre_pre[[1]]) # 0.5 0.5 / 0.083 0.083 
    cmip6_pre <- lapply(1:24, function(i){raster::aggregate(cmip6_pre_pre[[i]], fact = res.factor, fun = mean)}) # note this reduces some of the maximum precipitation values seen in finer spatial resolution
    # convert to dataframe 
    cmip6_pre_df <- lapply(1:24, function(i){
      raster::as.data.frame(cmip6_pre[[i]],xy=TRUE) %>% 
        as.data.table() %>% 
        rename(lon=x, lat=y, pre=layer) %>% 
        mutate(lon=round(lon,2), lat=round(lat,2))
    })
  })
}


create_cmip6_tmp_df <- function(time_periods){
  
  lapply(1:4, function(i){
    cmip6_files <- list.files(path = here("data","CMIP6 data",time_periods[[i]]), pattern = "^.*\\.(tif|TIF|Tif)$")
    cmip6_data <- here("data", "CMIP6 data", time_periods[[i]], cmip6_files)
    cmip6_tmp <- lapply(1:24, function(i){
      raster(cmip6_data[i],band=1)
    })
    # aggregate spatial resolution to 0.5x0.5 degrees 
    r_object <- raster(res = c(0.5, 0.5), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    res.factor <- raster::res(r_object)/raster::res(cmip6_tmp[[1]]) # 0.5 0.5 / 0.083 0.083 
    cmip6_tmp <- lapply(1:24, function(i){raster::aggregate(cmip6_tmp[[i]], fact = res.factor, fun = mean)}) # note this reduces some of the maximum precipitation values seen in finer spatial resolution
    # convert to dataframe 
    cmip6_tmp_df <- lapply(1:24, function(i){
      raster::as.data.frame(cmip6_tmp[[i]],xy=TRUE) %>% 
        as.data.table() %>% 
        rename(lon=x, lat=y, tmp=3) %>% 
        mutate(lon=round(lon,2), lat=round(lat,2))
    })
  })
}

# CO2 data from cmip5

get_CO2_data <- function(file){
  
  nc_CO2_rcp85 <- nc_open(here("data", "CMIP5 data", file))
  
  nc_CO2_var_rcp85 <- ncvar_get(nc_CO2_rcp85, attributes(nc_CO2_rcp85$var)$names[1])
  
  CO2_data <- data.frame(year = c(1860:2100), CO2 = nc_CO2_var_rcp85)
  
}

# extract CO2

extract_CO2_2015 <- function(data){
  
  data %>% filter(year==2015) %>% dplyr::select(CO2)
  
}

# Calculate CO2 change and F(CO2)

create_CO2_change <- function(CO2_data, CO2_2015, period){
    CO2_change <- lapply(1:4, function(i){ 
      CO2_data %>% 
        filter(year >= period$start_year[[i]] & year <= period$end_year[[i]]) %>% 
        summarise(CO2 = mean(CO2)) %>% 
        mutate(CO2_change = CO2 - CO2_2015$CO2,
               f_CO2_C3 = CO2_change/(CO2_change + 100), # allows for declining marginal effect of CO2, from Moore
               f_CO2_C4 = CO2_change/(CO2_change + 50))
    })
}

# create mean annual temp change and pre change for each future time period

create_change_vars <- function(cmip6_tmp_df, cmip6_pre_df, tmp_mean_2015, pre_mean_2015){
  
 lapply(1:4, function(j){ # i = time period
  lapply(1:24, function(i, j){ # j = GCM 
    cmip6_tmp_df[[j]][[i]] %>% 
      left_join(cmip6_pre_df[[j]][[i]], by = c("lon", "lat")) %>% 
      left_join(tmp_mean_2015, by = c("lon", "lat")) %>% 
      rename(tmp_2015=var_2015) %>% 
      left_join(pre_mean_2015, by = c("lon", "lat")) %>% 
      rename(pre_2015=var_2015) %>% 
      mutate(Temp.Change = tmp - tmp_2015, # in degrees
             Precipitation.change = (pre - pre_2015)/pre_2015*100) # in percentage terms, but same as yield change in that x100
  }, j)
  })
}

# create final prediction dataset

create_prediction_data <- function(bs_2015_vars, change_vars, CO2_change){
  
  lapply(1:4, function(k){ # crops = k - highest level in three-level list; 1:4
    lapply(1:4, function(j, k){ # time period = j; 1:4
      lapply(1:24, function(i,j,k){ # GCM = i; 1:24
        bs_2015_vars[[k]] %>% 
          left_join(
            dplyr::select(change_vars[[j]][[i]], Temp.Change, Precipitation.change, lon, lat), 
            by=c("lon","lat")) %>% 
          dplyr::select(!c("Country_name")) %>% 
          mutate(f_CO2 = ifelse(C3==1, CO2_change[[j]]$f_CO2_C3, CO2_change[[j]]$f_CO2_C4)) # time period j matches CO2_change
      }, j, k) 
  }, k)}) 
  
}


# clean
clean_prediction_data <- function(data){
  lapply(1:4, function(k){ # crops = k - highest level in three-level list; 1:4
    lapply(1:4, function(j, k){ # time period = j; 1:4
      lapply(1:24, function(i,j,k){
  data[[k]][[j]][[i]] %>% 
    filter(complete.cases(.)) 
      }, j, k) 
    }, k)}) 
  
}

# store and write prediction data to local storage

write_prediction_data <- function(data, outdir, crops, time_period, GCM) {# 
  
  paths <- sapply(1:4, function(k){
    sapply(1:4, function(j){
      sapply(1:24, function(i){
        filename <-  sprintf("%s/prediction_data/%s/%s/%s.RData", 
                             outdir, crops[k], time_period[j], GCM[i])
        data.complete <- data[[k]][[j]][[i]] %>% filter(complete.cases(.)) 
        
        
        # Create directory if it does not exists
        if(!dir.exists(dirname(filename))) { 
          dir.create(path = dirname(filename), recursive = TRUE)
        }
        
        saveRDS(data.complete, filename)
        filename
      })
    })
  }) %>% unlist()
  
  
  message("Prediction data complete")
  paths
}
