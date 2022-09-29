# https://github.com/ropensci/targets/discussions/850
# modularise target pipeline


# Read in and clean scraped CGIAR data ----------------------------------------------

get_data_from_csv <- function(file) {
  read_csv(file)
}

remove_na <- function(data) {
  data <- data %>% 
  replace_with_na_all(
    condition = ~.x %in% c("NA", "N/A", -9999, -9998)
  )
}


# Wrangle Monfreda crop production data -------------------------

# rasterise gridded crop production data
rasterise_files <- function(files){
  lapply(1:4, function(i){ 
    raster(files[i])})
}

# rasterise and extract country specific crop production data into dataframe

extract_country_crop_production <- function(files, map_boundaries){
  lapply(1:4, function(i){ 
     crop_production <- raster(files[i])
  crop_production_country <- raster::extract(crop_production, 
                                             map_boundaries, # or make this an explicit argument
                                             fun = sum, 
                                             na.rm = TRUE) # 243 countries
  crop_country_df <- as.data.frame(crop_production_country)
  crop_country_df$Y <- crop_country_df$V1
  # mutate new variable for ranking countries by production
  crop_country_df$rank_Y <- rank(desc(crop_country_df$Y), ties.method = "first")
  crop_country_df
  })

}

add_crop_production_country_name <- function(data, map_boundaries){
  lapply(1:4, function(i){
    data[[i]] <- data[[i]] %>% mutate(Country=map_boundaries@data$ISO_A2)   
    data[[i]]
  })
}

# wrangle CGIAR multiple country estimates by choosing country with highest production of the crop
unwrap_multicountry <- function(data){
  df <- data %>% 
    as.data.frame() %>% 
    filter(Crop %in% c("Maize", "Maize (Monsoon)", "Maize (Winter)",
                       "Rice", "Rice (Irrigated)", "Rice (Rainfed)",
                       "Wheat", "Wheat (Durum)", "Wheat (Irrigated)", 
                       "Wheat (Rainfed)", "Wheat (Spring)", "Wheat (Winter)",
                       "Soybean")) %>% 
    filter(!is.na(Country)) %>% 
    group_by(Country, Crop) %>% 
    summarise(unique_countries = n_distinct(Country)) %>%
    mutate(c_occur = stringr::str_count(Country, '[A-Z]')/2) %>% 
    filter(c_occur != 1) %>% 
    dplyr::select(Country, Crop) %>% 
    as.data.frame()
} 

unlist_multicountry <- function(data){
  df <- data %>% 
    mutate(index = 1:nrow(data)) %>% # 1:72
    relocate(index, Country, Crop) 
  
  df$Country <- str_replace_all(df$Country, ";", ",")
  
  df
}

make_long_multicountry <- function(data, s){
  df <- data.frame(
    index = rep(data$index, sapply(s, length)), 
    crop = rep(data$Crop, sapply(s, length)),
    Country = unlist(s))
  
  df$Country <- gsub(" ", "", 
                    df$Country, 
                    fixed = TRUE)
  
  df
  
}

join_multicountry <- function(data, crop_production_names){
  lapply(1:4, function(i){
  df <- left_join(
  data, 
  crop_production_names[[i]], 
  by = c("Country"))
  
  })
}


rank_country_by_crop <- function(data, crop_production_names){ # may need to make this 1:4 function as list
  lapply(1:4, function(i){
     df <- data[[i]] %>% 
  group_by(index, crop) %>% 
  # currently index describes the 66 (study, crop) groups
  summarise(min_Y = min(rank_Y, na.rm = TRUE)) %>% 
    left_join(dplyr::select(crop_production_names[[i]], Country, rank_Y), 
              by = c("min_Y" = "rank_Y"))  %>% 
    dplyr::select(!c("min_Y")) 
  })
}

# instead of keeping by column in previous code, keep by list index number
# but in a single data frame

country_crop_ranked_df <- function(y){
  data.frame(index=y[[1]]$index,
             crop=y[[1]]$crop) %>% 
    mutate(Country1 = case_when(
      crop == "Maize" ~ y[[1]]$Country,
      crop == "Rice" ~ y[[2]]$Country,
      crop == "Soybean" ~ y[[3]]$Country,
      crop == "Wheat" ~ y[[4]]$Country,
      crop == "Rice (Irrigated)" ~ y[[2]]$Country,
      crop == "Rice (Rainfed)" ~ y[[2]]$Country,
      crop == "Wheat (Irrigated)" ~ y[[4]]$Country,
      crop == "Wheat (Rainfed)" ~ y[[4]]$Country,
      crop == "Wheat (Durum)" ~ y[[4]]$Country,
      crop == "Wheat (Spring)" ~ y[[4]]$Country
    ))
}

join_country_crop_ranked <- function(data, a){
  data %>% 
  left_join(dplyr::select(a, index, Country1), by = "index")
}


# wrangle CGIAR data ------------------------------------------------------

clean_cgiar_data <- function(data){
  
  data %>% 
    filter(Crop %in% c("Maize", "Maize (Monsoon)", "Maize (Winter)",
                       "Rice", "Rice (Irrigated)", "Rice (Rainfed)",
                       "Wheat", "Wheat (Durum)", "Wheat (Irrigated)", "Wheat (Rainfed)", "Wheat (Spring)", "Wheat (Winter)",
                       "Soybean"),
           !is.na(Country)) %>% 
    mutate(CO2.Change = CO2.Projected - CO2.Baseline,
           Photosynthetic_pathway = case_when(
          startsWith(Crop, "Maize") ~ "C4",
          startsWith(Crop, "Rice") ~ "C3",
          startsWith(Crop, "Wheat") ~ "C3",
          startsWith(Crop, "Soybean") ~ "C3"
    )) %>% 
    mutate(
        Country=ifelse(Reference=="Moriondo et al (2010)", str_replace_all(Country, ";",","), Country)
      )
  }
  
join_cgiar_data_country_rank <- function(data, multi_index){
  
  data %>% 
    left_join(multi_index, by=c("Country","Crop")) %>% 
    mutate(Country1=as.character(Country1),
           Country2 = case_when( 
             # assign multi-country entries to country with highest crop production 
             is.na(Country1) ~ Country, 
             !is.na(Country1) ~ Country1)) %>%
    dplyr::select(!Country1) %>% 
    mutate(C = 1) %>% 
    pivot_wider(names_from = Photosynthetic_pathway,
                values_from = C,
                values_fill = 0) %>% 
    mutate(f_CO2 = case_when(
      C3 == 1 ~ CO2.Change/(CO2.Change + 100),
      C4 == 1 ~ CO2.Change/(CO2.Change + 50)),
      adapt_dummy = as.factor(
        if_else(
        Adaptation %in% c("No", "NA", "No "), 0, 1)))

}


# read in and wrangle Sacks data ------------------------------------------

nc_folder_path <- function(folder){
  list.files(here("data", folder), 
                    pattern = "^.*\\.(nc|NC|Nc|Nc)$")
}

read_sacks <- function(files){
  
  lapply(1:7, function(i){
    # may need to change this in future: depending on number of nc files saved in folder
    # open a connection to the ith nc file
    nc_crop <- nc_open(here("data", "Sacks data", files[i]))
    # store values from variables and attributes
    nc_crop_index <- ncvar_get(nc_crop, attributes(nc_crop$var)$names[1])
    nc_crop_filled_index <- ncvar_get(nc_crop, attributes(nc_crop$var)$names[2])
    nc_crop_plant <- ncvar_get(nc_crop, attributes(nc_crop$var)$names[3])
    nc_crop_harvest <- ncvar_get(nc_crop, attributes(nc_crop$var)$names[7])
    nc_crop_lon <- ncvar_get(nc_crop,attributes(nc_crop$dim)$names[1])
    nc_crop_lat <- ncvar_get(nc_crop, attributes(nc_crop$dim)$names[2])
    nc_crop_atts <- ncatt_get(nc_crop, 0)
    # close the connection
    nc_close(nc_crop)
    # set the dimension names and values to the appropriate lon and lat values
    dimnames(nc_crop_index) <- list(lon = nc_crop_lon, lat = nc_crop_lat)
    # store in long format - meaning all location and crops will be in long format
    crop_calendar_df <- reshape::melt(nc_crop_index, value.name = "index") 
    # append other variable columns to the data frame
    crop_calendar_df$filled_index <- reshape::melt(nc_crop_filled_index)
    crop_calendar_df$filled_index <- crop_calendar_df$filled_index$value
    crop_calendar_df$plant <- reshape::melt(nc_crop_plant)
    crop_calendar_df$plant <- crop_calendar_df$plant$value
    crop_calendar_df$harvest <- reshape::melt(nc_crop_harvest)
    crop_calendar_df$harvest <- crop_calendar_df$harvest$value
    # note crop file names are long, need to chop these 
    # but also later harmonise across datasets
    crop_calendar_df$crop <- files[i]
    crop_calendar_df$crop <- substr(crop_calendar_df$crop, 1, 
                                 nchar(crop_calendar_df$crop)-22)
    
    crop_calendar_df
    
  }
  )
}

rbind_crop_list <- function(list){
  rbindlist(list, idcol=NULL)
}

ready_crop_calendar_for_joining <- function(data){
  data %>% 
    mutate(plant.date = as.Date(plant, origin = "2000-01-01"),
           harvest.date = as.Date(harvest, origin = "2000-01-01"),
           lon = round(lon, digits = 2),
           lat = round(lat, digits = 2),
           plant_month = lubridate::month(plant.date),
           harvest_month = month(harvest.date))%>% 
    dplyr::select(!c("filled_index", "plant", "harvest", "plant.date", "harvest.date", "value")) %>% 
    relocate(c("lon", "lat", "crop", "plant_month", "harvest_month"))
}


# read in and wrangle MIRCA data ------------------------------------------

read_mirca_30mn <- function(file){
  read.table(here(
  "data",
  "MIRCA data",
  file), 
  header = TRUE
)}

wrangle_mirca_data <- function(data){
  data %>% 
    filter(crop %in% c(1,2,3,8,27,28,29,34)) %>% 
    dplyr::select(!c("cell_ID", "row", "column", "subcrop")) %>% 
  # recode crop codes to character names
    mutate(crop = replace(crop, crop == 1, "Wheat (Irrigated)")) %>% 
    mutate(crop = ifelse(crop == 1, "Wheat (Irrigated)",
                         ifelse(crop == 27, "Wheat (Rainfed)",
                                ifelse(crop == 2, "Maize (Irrigated)",
                                       ifelse(crop == 28, "Maize (Rainfed)",
                                              ifelse(crop == 3, "Rice (Irrigated)",
                                                     ifelse(crop == 29, "Rice (Rainfed)",
                                                            ifelse(crop == 8, "Soybeans (Irrigated)",
                                                                   ifelse(crop == 34, "Soybeans (Rainfed)", 
                                                                          crop))))))))) %>% 
    rename(plant_month = start, 
           harvest_month = end) %>% 
    relocate(c("lon", "lat", "crop", "plant_month", "harvest_month")) %>% 
    dplyr::select(!c(area))
}


# create crop calendar data ----------------------

# merge
# think about this function - is there a better way to create crop calendar dataset?

create_crop_calendar <- function(sacks, mirca){
  rbind(sacks, mirca) %>% 
    mutate(n_growing_months = ifelse(harvest_month >= plant_month, harvest_month - plant_month + 1,
                                     12 - plant_month + 1 + harvest_month - 0)) %>% 
    relocate("lon", "lat", "crop", "plant_month", "harvest_month", "n_growing_months") %>% 
  # create 12 monthly indicator dummy vars for whether the crop/cell grows in that month (jan_growing, ... , dec_growing)
    mutate("1" = ifelse(1 >= plant_month & 1 <= harvest_month, 1, 
                        ifelse(plant_month > harvest_month & 1 >= plant_month, 1,
                               ifelse(plant_month > harvest_month & 1 <= harvest_month, 1, 0))),
           "2" = ifelse(2 >= plant_month & 2 <= harvest_month, 1, 
                        ifelse(plant_month > harvest_month & 2 >= plant_month, 1,
                               ifelse(plant_month > harvest_month & 2 <= harvest_month, 1, 0))),
           "3" = ifelse(3 >= plant_month & 3 <= harvest_month, 1, 
                        ifelse(plant_month > harvest_month & 3 >= plant_month, 1,
                               ifelse(plant_month > harvest_month & 3 <= harvest_month, 1, 0))),
           "4" = ifelse(4 >= plant_month & 4 <= harvest_month, 1, 
                        ifelse(plant_month > harvest_month & 4 >= plant_month, 1,
                               ifelse(plant_month > harvest_month & 4 <= harvest_month, 1, 0))),
           "5" = ifelse(5 >= plant_month & 5 <= harvest_month, 1, 
                        ifelse(plant_month > harvest_month & 5 >= plant_month, 1,
                               ifelse(plant_month > harvest_month & 5 <= harvest_month, 1, 0))),
           "6" = ifelse(6 >= plant_month & 6 <= harvest_month, 1, 
                        ifelse(plant_month > harvest_month & 6 >= plant_month, 1,
                               ifelse(plant_month > harvest_month & 6 <= harvest_month, 1, 0))),
           "7" = ifelse(7 >= plant_month & 7 <= harvest_month, 1, 
                        ifelse(plant_month > harvest_month & 7 >= plant_month, 1,
                               ifelse(plant_month > harvest_month & 7 <= harvest_month, 1, 0))),
           "8" = ifelse(8 >= plant_month & 8 <= harvest_month, 1, 
                        ifelse(plant_month > harvest_month & 8 >= plant_month, 1,
                               ifelse(plant_month > harvest_month & 8 <= harvest_month, 1, 0))),
           "9" = ifelse(9 >= plant_month & 9 <= harvest_month, 1, 
                        ifelse(plant_month > harvest_month & 9 >= plant_month, 1,
                               ifelse(plant_month > harvest_month & 9 <= harvest_month, 1, 0))),
           "10" = ifelse(10 >= plant_month & 10 <= harvest_month, 1, 
                         ifelse(plant_month > harvest_month & 10 >= plant_month, 1,
                                ifelse(plant_month > harvest_month & 10 <= harvest_month, 1, 0))),
           "11" = ifelse(11 >= plant_month & 11 <= harvest_month, 1, 
                         ifelse(plant_month > harvest_month & 11 >= plant_month, 1,
                                ifelse(plant_month > harvest_month & 11 <= harvest_month, 1, 0))),
           "12" = ifelse(12 >= plant_month & 12 <= harvest_month, 1, 
                         ifelse(plant_month > harvest_month & 12 >= plant_month, 1,
                                ifelse(plant_month > harvest_month & 12 <= harvest_month, 1, 0)))) %>% 
    rename(jan_growing = "1",
           feb_growing = "2",
           mar_growing = "3",
           apr_growing = "4",
           may_growing = "5",
           jun_growing = "6",
           jul_growing = "7",
           aug_growing = "8",
           sept_growing = "9",
           oct_growing = "10",
           nov_growing = "11",
           dec_growing = "12") %>% 
    mutate(crop=ifelse(crop=="Soybeans","Soybean",crop))
} 

rasterise_crop_calendar_data <- function(data){
  lapply(unique(data$crop), function(i){
    raster::rasterFromXYZ(data %>% 
                            filter(crop == i))
  }) 
}

# extend raster extent because mirca is truncated for crops with missing data 

extend_raster_crop_season <- function(raster){
  e <- extent(-180, 180, -90, 90)
  lapply(
  raster, function(r){
    raster::extend(r, e)}
  )
}
  

extract_country_crop_season <- function(raster, map_boundaries){
  lapply(1:15, function(i){ # renamed this from formerly crop_season_tmp, need to make sure later dependencies reflect this
  crop <- raster::extract(raster[[i]], 
                          map_boundaries, fun = mean, na.rm = TRUE)
  crop <- as.data.frame(crop)
  crop$crop2 <- i
  crop$NAME <- map_boundaries$NAME
  crop$ISO_A2 <- map_boundaries$ISO_A2
  crop
    })
}


# create concordance df
# relate 15 unique crop types from MIRCA to just 4 crop types
create_crop_season_concordance <- function(data){
  # unique(tar_read(crop_season_df)$crop) # see the order of 15 unique crop names
  data.frame(crop_no = c(1:15), # 15 unique crops
             crop_name = unique(data$crop)) %>% 
    # aggregate to 4 crop types
    mutate(crop_monfreda = c("maize", "maize", 
                             "rice", "rice", 
                             "soybean", "wheat", 
                             "wheat", "wheat", 
                             "maize", "soybean", "wheat", 
                             "maize", "rice", "rice", "soybean"))
}

create_dt_country_crop_season <- function(list, data){
  rbindlist(list) %>% 
    dplyr::select(!crop) %>% 
    rename("crop" = "crop2") %>% 
    left_join(data, by = c("crop" = "crop_no")) %>% 
    relocate(NAME, ISO_A2, crop, crop_name, crop_monfreda)
}


# read in and wrangle CRU temperature data --------------------------------

read_cru_data <- function(file){
  #1901-2010
  data <- raster::brick(here("data", "CRU data", file))
  str(data)
  data_date <- raster::getZ(data)
  # find indices for subsetting to 1950-2010
  grep("1950-01-16", data_date) # 589
  grep("2010-12-16", data_date) # 1320
  # subset years
  data_1950_2010 <- raster::subset(data, 589:1320)
  data_1950_2010@z$Date <- data@z$Date[589:1320]
  data_1950_2010
}

extract_country_cru_data <- function(raster, map_boundaries, weights){
  lapply(1:4, function(i){
    exactextractr::exact_extract(raster, 
    # note this is a rasterstack 
    # of monthly temperatures from 1950-2010
                               map_boundaries, 
                               'weighted_mean', 
                               weights = weights[[i]])
  })
}

create_df_country_cru_data <- function(list, map_boundaries){
  rbindlist(list) %>% 
  as.data.frame() %>% 
    mutate(NAME = rep(map_boundaries$NAME,4),
           ISO_A2 = rep(map_boundaries$ISO_A2,4)) %>% 
    relocate(NAME, ISO_A2) %>% 
    mutate(crop_monfreda = rep(c("maize", "rice", "soybean", "wheat"), each = 243))
}

# a few wrangling steps before calculating baseline growing season average CRU vars
# applying averaging process to CGIAR data
prep_baseline_gs_vars <- function(data1, data2, var){
  
  # join crop calendar month information by country with 1950-2010 CRU var data
  df <- data1 %>% 
    mutate(crop_name=as.character(crop_name)) %>% 
    left_join(data2, by = c("NAME", "ISO_A2", "crop_monfreda")) 
  
  weights <- df %>% 
    dplyr::select(c(9:20)) # binary variable showing whether growing month or not
  
  n_growing_months <- df %>% 
    dplyr::select("n_growing_months")
  
  n_1950_2010 <- 21:ncol(df)
  ind_1950_2010 <- matrix(c(n_1950_2010, rep(NA, 12 - ncol(df)%%12)), byrow = TRUE, ncol = 12)
  ind_1950_2010 <- data.frame(t(na.omit(ind_1950_2010))) # this identifies the columns for 12 months in each year from 1950-2010
  
  # then do matrix addition of those columns by growing month weights div no. growing months 
  # to estimate weighted growing season precipitation for each year from 1950-2010
  # assign to new df for crop growing season weighted average temperatures 
  weighted_avg_1950_2010 <- do.call(cbind, lapply(ind_1950_2010, function(i) {
    weighted_avg <- Reduce(`+`, Map(`*`, df[,..i], weights))/n_growing_months}))
  
  colnames(weighted_avg_1950_2010) <- paste0(var, "_", c(1950:2010))
  
  x <- weighted_avg_1950_2010 %>% 
    cbind(dplyr::select(df, NAME, ISO_A2, crop, crop_name)) %>% 
    relocate(NAME, ISO_A2, crop, crop_name) %>% 
    mutate(crop_name = ifelse(crop_name=="Soybeans", "Soybean", crop_name))

  crop_concordance <- tribble(
    ~ AGIMPACTS_BASELINE_CROP, ~ GROWING_SEASON_CROP,
    "Rice", "Rice",
    "Rice (Rainfed)", "Rice (Rainfed)",
    "Rice (Irrigated)", "Rice (Irrigated)",
    "Wheat", "Wheat",
    "Wheat (Spring)", "Wheat",
    "Wheat (Winter)", "Wheat.Winter",
    "Wheat (Rainfed)", "Wheat (Rainfed)",
    "Wheat (Irrigated)", "Wheat (Irrigated)",
    "Wheat (Durum)", "Wheat",
    "Maize", "Maize",
    "Soybean", "Soybean"
  )
  
  x %>% 
    filter(crop_name %in% crop_concordance$GROWING_SEASON_CROP) %>% 
    left_join(crop_concordance, by = c("crop_name" = "GROWING_SEASON_CROP")) %>% 
    relocate(NAME, ISO_A2, crop_name, AGIMPACTS_BASELINE_CROP) %>% # note this has increased the number of rows by replicating crop-dependent tmp for the other crop categories of AGIMPACTS_BASELINE_CROP, i.e. durum and spring wheat 
    rename(baseline_crop = AGIMPACTS_BASELINE_CROP,
           season_crop = crop_name) %>% 
    filter(!NAME == "Ashmore and Cartier Is.")
}
  
  