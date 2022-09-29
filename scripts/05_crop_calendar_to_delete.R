###########
# this script contains more of the crop calendar wrangling steps that must be completed prior to 
# calculating baseline growing season temperature and precipitation using 05.1 cru temp and 05.2 cru precip scripts respectively
# the order in which scripts should be run is this script 05.0; then 05.1 cru-temp followed by 05.2 cru-precip
# there are still a few steps that need to be run in 05.1 cru-temp before 05.2 cru-precip

# INPUTS: AGIMPACTS_MAIN; crop_season
crop_season <- readRDS(crop_season, here("processed", "crop_season.RData"))

# scale crop_season to country (calcs common to temp & precip) ------------------------------------------

unique(crop_season$crop) # these 15 crop classes combine Sacks and MIRCA seasonal + rainfed/irrigated crop varieties

# first rasterise crop_season - this gives 15 rasterbricks, one for each crop, showing growing season binary variables for each month

raster_crop_season <- lapply(unique(crop_season$crop), function(i){
  raster::rasterFromXYZ(crop_season %>% 
                          filter(crop == i))
}) 

# NAs introduced in matrix by coercion as MIRCA2000 crop layers have different spatial extent, xminmax and yminmax smaller than -180 180 -90 90
# extend the spatial extent

e <- extent(-180, 180, -90, 90)

raster_extended_crop_season <- lapply(raster_crop_season, function(r){raster::extend(r, e)})

saveRDS(raster_extended_crop_season, here("processed", "raster_extended_crop_season.RData"))

# store values extracted from 15 individual layers in a list, then rbind

crop_season_country <- lapply(1:15, function(i){ # renamed this from formerly crop_season_tmp, need to make sure later dependencies reflect this
  
  crop <- raster::extract(raster_extended_crop_season[[i]], worldmap, fun = mean, na.rm = TRUE)
  crop <- as.data.frame(crop)
  #crop$crop2 <- i
  crop$country <- worldmap$NAME
  crop$abbrev <- worldmap$ISO_A2
  crop
  
})

# this is now dt of 15 crops and monthly crop calendar info, i.e. binary variables for growing season across 12 months
crop_season_country_dt <- rbindlist(crop_season_country, idcol="crop2")

crop_season_country_dt <- crop_season_country_dt %>%
  dplyr::select(!crop) %>% 
  rename("crop" = "crop2") %>% 
  relocate(country, abbrev, crop)

dim(crop_season_country_dt) # 3645 rows for 243 countries x 15 crops, 18 cols for country, abbrev, n_growing_months and growing month indicators

# create concordance table for crops to merge by
unique(crop_season_country_dt$crop) # 1:15
unique(crop_season$crop) # crop names - same order as per line 69

crop_concordance_15 <- data.frame(crop_no = unique(crop_season_country_dt$crop),
                                  crop_name = unique(crop_season$crop)) # 15 unique 

# add variable for Monfreda crop list 
crop_concordance_15 <- crop_concordance_15 %>% 
  mutate(crop_monfreda = c("maize", "maize", "rice", "rice", "soybean", "wheat", "wheat", "wheat", "maize", "soybean", "wheat", "maize", "rice", "rice", "soybean"))

# mutate crop_season_country_tmp variable equivalent to crop_monfreda values
crop_season_country_dt <- crop_season_country_dt %>% left_join(crop_concordance_15, by = c("crop" = "crop_no")) %>% 
  relocate(country, abbrev, crop, crop_name, crop_monfreda)

crop_season_country_dt %>% readr::write_csv(here("processed", "crop_season_country_dt.csv"))



# read in crop production data  (common to temp and precip) ---------------------

crop_production_files <- c("maize_Production.tif",
                           "rice_Production.tif",
                           "soybean_Production.tif",
                           "wheat_Production.tif")

crop_production_files2 <- here("data", "Monfreda data", crop_production_files)

crop_production_rasters <- lapply(crop_production_files2, raster)

# stack em
crop_production_stack <- stack(crop_production_rasters)

maize_production <- crop_production_stack[[1]]
rice_production <- crop_production_stack[[2]]
soybean_production <- crop_production_stack[[3]]
wheat_production <- crop_production_stack[[4]]
