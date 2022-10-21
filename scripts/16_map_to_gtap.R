
# Recalculate country level results, including those with NA production
# Then, aggregate to GTAP level

# READ IN GRIDDED POOLED PREDICTIONS DATA----------------------------------------------

crops <- c("Maize", "Rice", "Soybean", "Wheat")

# parsing failures here as well - 
pooled_predictions <- lapply(1:4, function(i){
  
  readr::read_csv(here("processed", paste0("pooled_predictions_", crops[[i]], ".csv")),
                  col_types = readr::cols(.default = "d"))
  
})



# READ IN COUNTRY POOLED PREDICTIONS --------------------------------------

# read m-pooled country results for yield changes
predicted_yield_country_reweighted <- list.files(path = here("results", "tables"), 
                                                 pattern = "predicted_country_weighted") # alphabetical order of crops

predicted_yield_country_reweighted <- lapply(1:4, function(i) readr::read_csv(here("results", "tables", predicted_yield_country_reweighted[[i]]))) # list

# READ IN GRIDDED CROP PRODUCTION DATA --------------------------------------

# Go to script 03 and run lines 41-48, or here:

crop_production_files <- c("maize_Production.tif",
                           "rice_Production.tif",
                           "soybean_Production.tif",
                           "wheat_Production.tif")


crop_production_files2 <- here("data", "Monfreda data", crop_production_files)


# Go to script 05 and run line 126-129, or here:

crop_production_rasters <- lapply(crop_production_files2, raster)

# stack em
crop_production_stack <- stack(crop_production_rasters)


# READ IN COUNTRY CROP PRODUCTION DATA --------------------------------------------

crop_production_csv <- readr::read_csv(here("processed", "crop_production_list.csv"))


# READ COUNTRY-TO-GTAP CORRESPONDANCE -------------------------------------

# read Ha's mapping file
mapping <- readr::read_csv(here("data", "GTAP data", "mapping.csv"))


# Go to script 03 and run line 52-52, or here 

worldmap <- rworldmap::getMap(resolution = "coarse")

worldmap_clean <- cleangeo::clgeo_Clean(worldmap) # some polygons are not closed, i.e. Canada and Western Sahara


# CALCULATE COUNTRY NON-WEIGHTED AVERAGE RESULTS -------------------------

# # if running from scratch, then read in csv results from line 120

# Calculate simple average yield change predictions for all countries


pooled_simple_avg_country <- function(i){
  
  raster_0dc <- raster(ncol = 720, nrow = 360) 
  values(raster_0dc) <-  pooled_predictions[[i]][["mean_y_0"]]
  
  raster_1dc <- raster(ncol = 720, nrow = 360) 
  values(raster_1dc) <- pooled_predictions[[i]][["mean_y_1"]]
  
  raster_2dc <- raster(ncol = 720, nrow = 360) 
  values(raster_2dc) <- pooled_predictions[[i]][["mean_y_2"]]
  
  raster_3dc <- raster(ncol = 720, nrow = 360) 
  values(raster_3dc) <- pooled_predictions[[i]][["mean_y_3"]]
  
  raster_4dc <- raster(ncol = 720, nrow = 360) 
  values(raster_4dc) <- pooled_predictions[[i]][["mean_y_4"]]
  
  raster_5dc <- raster(ncol = 720, nrow = 360) 
  values(raster_5dc) <- pooled_predictions[[i]][["mean_y_5"]]
  
  raster_alldc_stack <- stack(raster_0dc, raster_1dc, raster_2dc, raster_3dc, raster_4dc, raster_5dc)
  
  
  predicted_yield_all_dc_country <- exactextractr::exact_extract(raster_alldc_stack, worldmap_clean, 'mean')
  
  predicted_yield_all_dc_country <- predicted_yield_all_dc_country %>% rename("0 degrees" = 1, "1 degree" = 2, "2 degrees" = 3,
                                                                              "3 degrees" = 4, "4 degrees" = 5, "5 degrees" = 6)
  
  worldmap_copy <- worldmap
  worldmap_copy@data <- worldmap_copy@data %>% cbind(predicted_yield_all_dc_country)
  
  predicted_yield_all_dc_country %>% 
    cbind(worldmap_copy@data$NAME) %>%
    cbind(worldmap_copy@data$ADMIN.1) %>% 
    cbind(worldmap_copy@data$ISO_A2) %>% 
    rename("Country" = 7,
           "Admin" = 8,
           "ISO_A2" = 9) %>% readr::write_csv(here("results", "tables", paste0("predicted_country_nonweighted", "_", crops[[i]], ".csv")))
  
  
}


pooled_country_predictions_avg <- lapply(1:4, pooled_simple_avg_country)



# READ IN COUNTRY NON-WEIGHTED AVERAGE RESULTS AS CSV --------------------


# if running the above, then no need for this as pooled_country_predictions_avg is saved as the same thing

predicted_yield_country_savg <- lapply(1:4, function(i){
  
  readr::read_csv(here("results", "tables", paste0("predicted_country_nonweighted", "_", crops[[i]], ".csv")))
  
}) 



# UNDERSTANDING COUNTRY NA'S---------------------------------------------------------

# there are still 47 countries with NA 
# may come down to missing prediction data e.g. baseline temp
# or the fit ?

sum(is.na(predicted_yield_country_savg[[1]]$`0 degrees`)) # 47
sum(is.na(predicted_yield_country_savg[[1]]$`5 degrees`)) # 47
sum(is.na(predicted_yield_country_savg[[1]]$Country)) # 1 - NA
sum(is.na(predicted_yield_country_savg[[2]]$`0 degrees`)) # 47
sum(is.na(predicted_yield_country_savg[[3]]$`0 degrees`)) # 47
sum(is.na(predicted_yield_country_savg[[4]]$`1 degree`)) # 45 - why is there predictions for Aruba and Malta- it doesn't make sense

maize_na_savg_countries <- predicted_yield_country_savg[[1]] %>% 
  filter(is.na(`0 degrees`)) %>% 
  dplyr::select(Country) %>% 
  print(n=Inf)

wheat_missing_countries <- predicted_yield_country_savg[[4]] %>% 
  filter(is.na(`0 degrees`)) %>% 
  dplyr::select(Country) %>% 
  print(n=Inf)

setdiff(maize_na_savg_countries$Country, wheat_missing_countries$Country) # wheat is the only crop that has data for aruba and malta

# compare to the list of missing countries in rasterising

setdiff(maize_na_savg_countries$Country, unique(world_d$name))

# check by extracting country baseline temperatures in prediction data
# crops_prediction_data mutate ncell = row_number() for each df in list
# then rasterise cell number of worldmap

r <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, crs=4326, res=0.5)

world <- worldmap_clean %>% 
  st_as_sf() %>% 
  mutate(country=as.numeric(as.factor(NAME)))

world_ras <- rasterize(world, r, field = 'country') # 1-243
plot(world_ras)

world_d <- as.data.frame(world_ras) %>% 
  mutate(cell=row_number()) %>% 
  rename(country='layer') %>% 
#  dplyr::filter(!is.na(country)) %>% 
  as.data.table # could it be due to two countries within the one cell? therefore can only allocate to one country, 52 drop out? islands close to other islands
# 0.5 degree is not high enough resolution?
# note that using worldmap high res only takes us up to 195 countries

# experiment with making a very high res raster grid cell
r_highres <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, crs=4326, res=0.05)

world_ras_highres <- rasterize(world, r_highres, field = 'country') # 1-243

world_d_highres <- as.data.frame(world_ras_highres) %>% 
  mutate(cell=row_number()) %>% 
  rename(country='layer') %>% 
  #  dplyr::filter(!is.na(country)) %>% 
  as.data.table

length(unique(world_d_highres$country)) # 240 countries - there we go

length(unique(world_d$country)) # 191 countries - why not 243? possibly due to NAs?
length(unique(world_d$name))
unique(world_d$name)

# join with country name

countries <- data.frame(
  country = as.numeric(as.factor(worldmap_clean@data$NAME)),
  name = worldmap_clean@data$NAME) %>% 
  as.data.table()

length(unique(countries$country))

world_d <- countries[world_d, on='country']

# the issue is in rasterising - suddenly there are 52 countries missing - these mostly overlap with countries missing in prediction (47)
missing_raster <- setdiff(unique(countries$name), unique(world_d$name)) # these are all small islands

raster_missing <- unique(missing_raster[missing_raster %in% maize_na_savg_countries$Country]) # 42 countries missing in raster that feature NA results in maize/rice/soy predictions
# , a further 10 countries missing in raster
unique(missing_raster[!missing_raster %in% maize_na_savg_countries$Country]) # there are the 10 countries further missing in raster due to rasterisation

# rasterisation issues (holes?) is most likely why there are some NA results; however there are an additional 5 that are NA for other reasons?
unique(maize_na_savg_countries$Country[!maize_na_savg_countries$Country %in% missing_raster])

# using exactextractr was meant to take care of issue coming from the grid cell center of small polygons 
# attempt to solve rasterisation issue by using high resolution worldmap (resolution = "High")
# if it works, this may make country predictions more time intensive, rewrite code to do country extraction in datatable then re-rasterise 
# only gets to 195 countries, without cleaning up the polygon holes


# otherwise, if we accept the NA predictions for the 40-something countries
# production-weighted average has around 78 country NAs 
# should use the mixed approach anyway
# look at the extra 30-something countries - do they have no production? check against production list

sum(is.na(predicted_yield_country_reweighted[[1]]$`0 degrees`)) # 78

maize_na_wavg_countries <- predicted_yield_country_reweighted[[1]] %>% 
  filter(is.na(`0 degrees`)) %>% 
  dplyr::select(Country)

maize_na_wavg_only <- maize_na_wavg_countries$Country[!maize_na_wavg_countries$Country %in% maize_na_savg_countries$Country]
# these 31 countries that have NA predictions in the production-weighted avg but not in the simple avg, suggesting that
# NAs result from 0 production of that crop
# confirm this

# Identify countries with NA production
crop_production_csv %>%
  filter(Y_maize == 0 | Y_rice == 0 | Y_soybean == 0 | Y_wheat == 0) # 140 countries with no production of at least 1 crop

countries_maize_na_production <- crop_production_csv %>% 
  filter(Y_maize == 0) %>% 
  dplyr::select(name)

countries_rice_na_production <- crop_production_csv %>% 
  filter(Y_rice == 0) %>% 
  dplyr::select(name)

countries_soybean_na_production <- crop_production_csv %>% 
  filter(Y_soybean == 0) %>% 
  dplyr::select(name)

countries_wheat_na_production <- crop_production_csv %>% 
  filter(Y_wheat == 0) %>% 
  dplyr::select(name)
  
production_na <- maize_na_wavg_only[maize_na_wavg_only %in% countries_maize_na_production$name] # all 31 countries

# then there are 42 countries with NA results due to raster issues (can only read in 191 countries from rworldmap spatialpolygons)
  
# then there are an additional 5 countries with NA results due to unknown reasons  - as they are not missing in raster form
  # perhaps due to prediction data? - yes - missing baseline temperatures
  # based on baseline temperature map, Antarctica (one of 5 NA unaccounted for countries) is missing

# this is because EITHER calendar information or temperature information is missing for these grid cells - which is it?
# investigate by joining with world_d

crop_season <- readr::read_csv(here("processed", "crop_season.csv"),
                               col_types = readr::cols(.default = "c"))


crop_season[,c(1:2)] <- lapply(crop_season[,c(1:2)], as.numeric)
    
crop_season[,c(4:18)] <- lapply(crop_season[,c(4:18)], as.numeric)

crop_season %>% 
  group_by(crop) %>% 
  summarise(n=n())
  
crop_season_main  <- crop_season %>% 
  filter(crop %in% c("Maize", "Rice", "Soybean", "Wheat"))

crop_n <- crop_season_main %>% 
  mutate(cell = rep(seq(1:259200),4)) %>% 
  left_join(world_d, by = "cell") %>% 
  group_by(name) %>% 
  summarise(n_growing_months = sum(n_growing_months, na.rm=TRUE))

sum(is.na(crop_n$n_growing_months)) # 0 there is Sacks calendar information for every single country
sum(crop_n$n_growing_months==0) # 4 countries with 0 growing months, plus Antarctica = 5

crop_n %>% 
  filter(n_growing_months == 0)

# focus on wheat
crop_season_main %>% 
  filter(crop == "Wheat") %>% 
  mutate(cell = row_number()) %>% 
  left_join(world_d, by = "cell") %>% 
  group_by(name) %>% 
  summarise(n_growing_months = sum(n_growing_months, na.rm=TRUE)) # Malta is missing

# see if we can redo world_d with exactextractr instead... i.e. rasterize with partial coverage of cells
wheat_calendar <- crop_season_main %>% 
  dplyr::select(lon, lat, crop, n_growing_months) %>% 
  filter(crop == "Wheat") %>% 
  dplyr::select(!c("crop"))

wheat_calendar_raster <- rasterFromXYZ(wheat_calendar, crs=4326)

wheat_calendar_country <- exactextractr::exact_extract(wheat_calendar_raster, worldmap_clean, 'mean')

wheat_calendar_country %>% 
  as.data.frame() %>% 
  rename(mean_growing_months = 1) %>% 
  cbind(NAME = worldmap_copy@data$NAME) %>% 
  summarise(sum_na = sum(is.na(mean_growing_months))) # 46

wheat_calendar_country %>% 
  as.data.frame() %>% 
  rename(mean_growing_months = 1) %>% 
  cbind(NAME = worldmap_copy@data$NAME) %>% 
  filter(is.na(mean_growing_months)) %>% 
  dplyr::select(NAME) 

# focus on maize

maize_calendar <- crop_season_main %>% 
  dplyr::select(lon, lat, crop, n_growing_months) %>% 
  filter(crop == "Maize") %>% 
  dplyr::select(!c("crop"))

maize_calendar_raster <- rasterFromXYZ(maize_calendar, crs=4326)

maize_calendar_country <- exactextractr::exact_extract(maize_calendar_raster, worldmap_clean, 'mean')

maize_calendar_country %>% 
  as.data.frame() %>% 
  rename(mean_growing_months = 1) %>% 
  cbind(NAME = worldmap_copy@data$NAME) %>% 
  summarise(sum_na = sum(is.na(mean_growing_months))) # 46

maize_calendar_country %>% 
  as.data.frame() %>% 
  rename(mean_growing_months = 1) %>% 
  cbind(NAME = worldmap_copy@data$NAME) %>% 
  filter(is.na(mean_growing_months)) %>% 
  dplyr::select(NAME) 


# maize and wheat the same - 46 countries with missing crop calendar information, meaning baseline temperatures could not be constructed in training data set

# now look at prediction data set, do the same thing
# suspect might be the same thing?

# check for maize
maize_pred_bs <- crops_prediction_data_check[[1]][[2]] %>% 
  cbind(lon = crop_season_main$lon,
        lat = crop_season_main$lat) %>% 
  dplyr::select(lon, lat, Baseline_tmp_weighted)

maize_pred_bs_raster <- rasterFromXYZ(maize_pred_bs, crs=4326)

maize_pred_bs_country <- exactextractr::exact_extract(maize_pred_bs_raster, worldmap_clean, 'mean')

maize_pred_bs_country %>% 
  as.data.frame() %>% 
  rename(predicted_baseline_tmp = 1) %>% 
  cbind(NAME = worldmap_copy@data$NAME) %>% 
  summarise(sum_na = sum(is.na(predicted_baseline_tmp))) # 47

maize_pred_na_countries <- maize_pred_bs_country %>% 
  as.data.frame() %>% 
  rename(predicted_baseline_tmp = 1) %>% 
  cbind(NAME = worldmap_copy@data$NAME) %>% 
  filter(is.na(predicted_baseline_tmp)) %>% 
  dplyr::select(NAME)

# check for wheat
wheat_pred_bs <- crops_prediction_data_check[[4]][[2]] %>% 
  cbind(lon = crop_season_main$lon,
        lat = crop_season_main$lat) %>% 
  dplyr::select(lon, lat, Baseline_tmp_weighted)

wheat_pred_bs_raster <- rasterFromXYZ(wheat_pred_bs, crs=4326)

wheat_pred_bs_country <- exactextractr::exact_extract(wheat_pred_bs_raster, worldmap_clean, 'mean')

wheat_pred_na_countries <- wheat_pred_bs_country %>% 
  as.data.frame() %>% 
  rename(predicted_baseline_tmp = 1) %>% 
  cbind(NAME = worldmap_copy@data$NAME) %>% 
  filter(is.na(predicted_baseline_tmp)) %>% 
  dplyr::select(NAME) # Malta and Aruba are not missing
 
 summarise(sum_na = sum(is.na(predicted_baseline_tmp))) # 45
 
 setdiff(maize_pred_na_countries$NAME, wheat_pred_na_countries$NAME)
 
 # why this divergence from 46 NA countries in the crop calendar information to 47 / 45 in the baseline temperature prediction data?
 # there is only one set of temperature data, so any divergence across crops should come from crop calendar information 
 # it could come from extra MIRCA information in the prediction data (as we pool 4 types of wheat calendar information together)
 # in the training baseline temperature data, we use only Sacks' 'wheat' here when we call 'wheat' in crop_season.csv
 
 # so look at other wheat varieties in the training data
 
 wheat_all_calendar <- crop_season %>% 
   dplyr::select(lon, lat, crop, n_growing_months) %>% 
   filter(crop == "Wheat" | crop == "Wheat.Winter" | crop == "Wheat (Rainfed)" | crop == "Wheat (Irrigated)") %>%
   group_by(lon, lat) %>% 
   summarise(n_growing_months = mean(n_growing_months)) 
 
 wheat_all_calendar_raster <- rasterFromXYZ(wheat_all_calendar, crs=4326)
 
 wheat_all_calendar_country <- exactextractr::exact_extract(wheat_all_calendar_raster, worldmap_clean, 'mean')
 
 wheat_all_calendar_country %>% 
   as.data.frame() %>% 
   rename(mean_growing_months = 1) %>% 
   cbind(NAME = worldmap_copy@data$NAME) %>% 
   summarise(sum_na = sum(is.na(mean_growing_months))) # 46
 
 wheat_countries <- wheat_all_calendar_country %>% 
   as.data.frame() %>% 
   rename(mean_growing_months = 1) %>% 
   cbind(NAME = worldmap_copy@data$NAME) %>% 
   filter(is.na(mean_growing_months)) %>% 
   dplyr::select(NAME) 
 
 setdiff(wheat_pred_na_countries$NAME, wheat_countries$NAME) 
 
 setdiff(wheat_countries$NAME, wheat_pred_na_countries$NAME)
 
 # Antarctica is missing in prediction data but not in training data.. 
 # Aruba and Malta are missing in training data but not missing in prediction data - is it a different geo calculation method? extract?
 
 # so where did the Aruba and Malta prediction baseline temp data come from?!
 # it's all gridded, and then gets summarised to country via country predictions in script 15 using exact_extract()
 
 # look at prediction data
 baseline_tmp_grid_df_2015_2020 <-  readr::read_csv(here("processed", "baseline_tmp_grid_df_2015_2020.csv"),
                                                    col_types = readr::cols(.default = "d"))
 
 # cbind, rasterise then do exactextractr, look at wheat
 baseline_tmp_pred <- baseline_tmp_grid_df_2015_2020 %>% 
   cbind(lon = crop_season_main$lon,
       lat = crop_season_main$lat) %>% 
   relocate(lon, lat)
 
 baseline_tmp_pred_raster <- rasterFromXYZ(baseline_tmp_pred, crs=4326)
   
 baseline_tmp_pred_country <- exactextractr::exact_extract(baseline_tmp_pred_raster, worldmap_clean, 'mean')
 
 baseline_tmp_pred_country %>% 
   as.data.frame() %>% 
   cbind(NAME = worldmap_copy@data$NAME) %>% 
   relocate(NAME) %>% # Aruba and Malta have non-NAs for wheat
   filter(is.na(mean.Wheat))

 # MIRCA does not seem to add more information
 # still unclear how we end up with prediction baseline temp for wheat in Malta
 # it could be because in generating baseline prediction data, we use baseline_tmps_2015_2020_grid_raster[], script 13 line 626
 
 # backtrack and read in earlier prediction frame before raster[]
 
 mean_baseline_tmps_2015_2020_grid_wide <-  readr::write_csv(here("processed", "mean_baseline_tmps_2015_2020_grid_wide.csv"))
 
 # make lon and lat numeric
 mean_baseline_tmps_2015_2020_grid_wide[,c(1,2)] <- sapply(mean_baseline_tmps_2015_2020_grid_wide[,c(1,2)], as.numeric)
 
 baseline_tmp_pre_raster <- rasterFromXYZ(mean_baseline_tmps_2015_2020_grid_wide, crs=4326)
 
 baseline_tmp_pre_country <- exactextractr::exact_extract(baseline_tmp_pre_raster, worldmap_clean, 'mean')
 
 baseline_tmp_pre_country %>% 
   as.data.frame() %>% 
   cbind(NAME = worldmap_copy@data$NAME) %>% 
   relocate(NAME)
 
 # Malta and Aruba already have non-NA results here, so it is not because of the raster[] but precedes that, try process up to line 481
 
 # need to spread n_growing_months across crops
  crop_season_extended_subset_spread <- spread(crop_season_extended_subset, crop, n_growing_months)
 
 
 crop_season_extended_subset_test <- crop_season_extended_subset_spread %>% 
   relocate(lon,lat) %>% 
   dplyr::select(lon,lat,c("Wheat":"Wheat.Winter"))
 
 crop_season_ext_raster <- rasterFromXYZ(crop_season_extended_subset_test, crs=4326) # NAs introduced by coercion..
 
 crop_season_ext_country <- exactextractr::exact_extract(crop_season_ext_raster, worldmap_clean, 'mean')
 
 crop_season_ext_country %>% 
   as.data.frame() %>% 
   cbind(NAME = worldmap_copy@data$NAME) %>% 
   relocate(NAME) # THERE IS DATA IN RAINFED WHEAT !!
   

# remember that it is production-weighted baseline temperature that we calculate

# therefore every country that is missing production information will result in country NAs, either in the construction of baseline temps, or in production-wavg

maize_na_savg_countries$Country[maize_na_savg_countries$Country %in% countries_maize_na_production$name] # all 47 countries

  # all NA results are driven by NA production, but only 31 are 'resolved' when doing savg, 47 remain NA
  # seems odd that there are 31 countries with NA production that can have non-NA results in savg
  # when NA production means that all countries with NA production should have NA baseline temperature - and should have NA results
  # check on baseline temperature
  # suspect it is because prediction baseline temperature data is not production-weighted - because it is gridded

# baseline temperatures used to construct training dataset are production-weighted, hence full list of countries with NA is high (78)

  # count number of countries with NA baseline tmp
  
# read in here("processed", "tmp_1950_2020_crop_production.csv")
  
  growing_season_tmp_1950_2010 <- readr::read_csv(here("processed", "growing_season_tmp_1950_2010.csv"))
  
  # count number of countries with NA baseline tmp
  
  growing_season_tmp_1950_2010 %>% 
    filter(crop_name %in% c("Maize")) %>% 
    summarise(sum = sum(is.na(tmp_2010))) # 78 countries with NA baseline growing season temperature
  
  growing_season_tmp_1950_2010 %>% 
    filter(crop_name %in% c("Wheat")) %>% 
    summarise(sum = sum(is.na(tmp_2010))) # 100 countries
  
  # however, this shouldn't strictly matter - it's how many countries with NA baseline temperature in prediction data that matters
  
  
  # read in prediction data from script 14
  # now mutate cell number
  
    crops_prediction_data_check <- lapply(1:4, function(j){
    
    lapply(1:12, function(i,j){
           crops_prediction_data[[j]][[i]] %>% 
             mutate(cell = row_number())}, j)
  })
    
    crop_calendar_na <- crops_prediction_data_check[[1]][[1]] %>% 
      left_join(world_d, by = "cell") %>% 
      group_by(name) %>% 
      summarise(mean_baseline_tmp = mean(Baseline_tmp_weighted, na.rm=TRUE)) %>% print(n=Inf) %>% 
      filter(is.na(mean_baseline_tmp)) # 5 NAs out of 191, which explains the additional NA 5 countries
    
    # we need to do this using exactextractr properly - prediction/crop calendar data could be responsible for more NAs than the raster issue, world_d is unreliable and unrelated to our process
    
      
      # so we can conclude that 5 countries are missing baseline temperature, 4 due to zero growing months (Sacks)- Kiribati, Cayman Is, St. Vin. and Gren., US Virgin Is
      # plus Antarctica - not sure why Antarctica returns NA as there are non-zero growing months
      
      # 31 countries missing due to production-weighting and NA production of crop in these countries
      # the remaining 42 missing countries is due to low resolution of 0.5 x 0.5 grid cells - 
      # either bc more than 2 countries per grid cell or polygon not covering center of grid cell? note we don't use exactextractr here but rasterize then dataframe()
      # total 78 countries
   
    

# COALESCE COUNTRY WEIGHTED AND NON-WEIGHTED PREDICTIONS ------------------------

  
  predicted_country_coalesced <- lapply(1:4, function(i) { # note that predicted_yield_country_savg doesn't have the first column X1 that's in the wavg results
    data.frame("0 degrees" = coalesce(predicted_yield_country_reweighted[[i]][2], predicted_yield_country_savg[[i]][1]),
             "1 degree" = coalesce(predicted_yield_country_reweighted[[i]][3], predicted_yield_country_savg[[i]][2]),
             "2 degrees" = coalesce(predicted_yield_country_reweighted[[i]][4], predicted_yield_country_savg[[i]][3]),
             "3 degrees" = coalesce(predicted_yield_country_reweighted[[i]][5], predicted_yield_country_savg[[i]][4]),
             "4 degrees" = coalesce(predicted_yield_country_reweighted[[i]][6], predicted_yield_country_savg[[i]][5]),
             "5 degrees" = coalesce(predicted_yield_country_reweighted[[i]][7], predicted_yield_country_savg[[i]][6]),
             Country = coalesce(predicted_yield_country_reweighted[[i]][8], predicted_yield_country_savg[[i]][7]),
             Admin = coalesce(predicted_yield_country_reweighted[[i]][9], predicted_yield_country_savg[[i]][8]),
             ISO_A2 = coalesce(predicted_yield_country_reweighted[[i]][10], predicted_yield_country_savg[[i]][9])) %>% 
      rename("0 degrees" = 1,
             "1 degree" = 2,
             "2 degrees" = 3,
             "3 degrees" = 4,
             "4 degrees" = 5,
             "5 degrees" = 6) %>% 
      mutate(imputed = ifelse(!is.na(predicted_yield_country_reweighted[[i]][2]), 0, # must record list of countries coalesced from savg results
                              ifelse(is.na(predicted_yield_country_reweighted[[i]][2]) & !is.na(predicted_yield_country_savg[[i]][1]), 1, 0)))
    }
    )
  
# write to csv
      lapply(1:4, function(i) {
        
        readr::write_csv(predicted_country_coalesced[[i]], 
                         here("results", "tables", paste0("predicted_country_coalesced_", crops[[i]], ".csv")))
        
      }) 
      
  
  # checks 
  
  sum(is.na(predicted_country_coalesced[[1]]$`0 degrees`)) # 47
  predicted_country_coalesced[[1]] %>% as.data.table() %>% print(n=Inf)
  predicted_country_coalesced[[1]] %>% summarise(sum = sum(imputed)) # 31 country entries imputed
  
# AGGREGATE GTAP REGIONAL RESULTS (WEIGHTED, NONWEIGHTED AND COALESCED)  ------------------------------

# MERGE WITH MAPPING FILE 

# left_join on Admin, Country and id243/X1, new vars will be c141, id141, n141

predicted_yield_country <- lapply(1:4, function(i) {
  
  left_join(predicted_country_coalesced[[i]],  # using coalesced results, not predicted_yield_country_reweighted[[i]] which report only the production-weighted country averages
            mapping,
            by = c("Country",
                   "Admin"))
})

# SIMPLE AVERAGE OVER 141 REGIONS - this is unnecessary as already doing this later as non-weighted, unless need to differentiate between non-coalesced and coalesced country results

gtap_region_results <- lapply(1:4, function(i) {
  
  predicted_yield_country[[i]] %>%
    group_by(id141, n141, c141) %>% 
    summarise_at(vars(`0 degrees`:`5 degrees`), mean, na.rm=TRUE)
  
})

# write to csv

crops <- c("Maize", "Rice", "Soybean", "Wheat")

# save in the processed folder

lapply(1:4, function(i) {
  
  readr::write_csv(gtap_region_results[[i]], 
                   here("results", "tables", paste0("gtap_region_", crops[[i]], ".csv")))
  
}) 


# PRODUCTION-WEIGHTED GTAP REGIONAL AVERAGE 

# Transform country production volumes into list to make merging by crop country result easier

crop_production_list <- list(
  data.frame(name = crop_production_csv$name, Y = crop_production_csv$Y_maize),
  data.frame(name = crop_production_csv$name, Y = crop_production_csv$Y_rice),
  data.frame(name = crop_production_csv$name, Y = crop_production_csv$Y_soybean),
  data.frame(name = crop_production_csv$name, Y = crop_production_csv$Y_wheat))

# merge country_results with crop_production data

country_production_results <- lapply(1:4, function(i){
  
  left_join(predicted_yield_country[[i]], # using coalesced results, not predicted_yield_country_reweighted[[i]] which report only the production-weighted country averages
            crop_production_list[[i]],
            by = c("Country" = "name")) 
})

# Calculate production weighted regional average

gtap_region_weighted_results <- lapply(1:4, function(i) {
  
  country_production_results[[i]] %>%
    group_by(id141, n141, c141) %>% 
    summarise(across(`0 degrees`:`5 degrees`, ~weighted.mean(., w = Y, na.rm=TRUE)))
  
})


lapply(1:4, function(i) {
  
  readr::write_csv(gtap_region_weighted_results[[i]], 
                   here("results", "tables", paste0("predicted_gtap_weighted_", crops[[i]], ".csv")))
  
}) 

# write to csv

# Calculate simple regional average

gtap_region_nonweighted_results <- lapply(1:4, function(i) {
  
  country_production_results[[i]] %>%
    group_by(id141, n141, c141) %>% 
    summarise(across(`0 degrees`:`5 degrees`, ~mean(., na.rm=TRUE)))
  
})



lapply(1:4, function(i) {
  
  readr::write_csv(gtap_region_nonweighted_results[[i]], 
                   here("results", "tables", paste0("predicted_gtap_nonweighted_", crops[[i]], ".csv")))
  
}) 

# write to csv

# coalesce gtap region weighted results with gtap region non-weighted results

predicted_gtap_coalesced <- lapply(1:4, function(i) { # note that predicted_yield_country_savg doesn't have the first column X1 that's in the wavg results
  data.frame(             Country = coalesce(gtap_region_weighted_results[[i]][1], gtap_region_nonweighted_results[[i]][1]),
                          Admin = coalesce(gtap_region_weighted_results[[i]][2], gtap_region_nonweighted_results[[i]][2]),
                          ISO_A2 = coalesce(gtap_region_weighted_results[[i]][3], gtap_region_nonweighted_results[[i]][3]),
    "0 degrees" = coalesce(gtap_region_weighted_results[[i]][4], gtap_region_nonweighted_results[[i]][4]),
             "1 degree" = coalesce(gtap_region_weighted_results[[i]][5], gtap_region_nonweighted_results[[i]][5]),
             "2 degrees" = coalesce(gtap_region_weighted_results[[i]][6], gtap_region_nonweighted_results[[i]][6]),
             "3 degrees" = coalesce(gtap_region_weighted_results[[i]][7], gtap_region_nonweighted_results[[i]][7]),
             "4 degrees" = coalesce(gtap_region_weighted_results[[i]][8], gtap_region_nonweighted_results[[i]][8]),
             "5 degrees" = coalesce(gtap_region_weighted_results[[i]][9], gtap_region_nonweighted_results[[i]][9])) %>% 
    rename("0 degrees" = 4,
           "1 degree" = 5,
           "2 degrees" = 6,
           "3 degrees" = 7,
           "4 degrees" = 8,
           "5 degrees" = 9) %>% 
    mutate(imputed = ifelse(!is.na(gtap_region_weighted_results[[i]][4]), 0, # must record list of countries coalesced from savg results
                            ifelse(is.na(gtap_region_weighted_results[[i]][4]) & !is.na(gtap_region_nonweighted_results[[i]][4]), 1, 0))) %>% 
    rename(imputed = 10)
})


lapply(1:4, function(i) {
  
  readr::write_csv(predicted_gtap_coalesced[[i]], 
                   here("results", "tables", paste0("predicted_gtap_coalesced_", crops[[i]], ".csv")))
  
}) 


# NOTES
# gtap results should be interpreted/accompanied by predicted_country_coalesced.csv as this tells which countries have been imputed using simple average country results
# gtap coalesced average results should also be accompanied by 
# both should be accompanied by list of countries that had NAs and what was done 


# COUNTRY NA LOG -------------------------------------------------------------

countries %>% 
  mutate(ISO_A3 = worldmap@data$ISO_A3,
         NA_crop_calendar = ifelse(name %in% maize_pred_na_countries$NAME, 1, 0), # NA because no crop calendar information (47 countries)
         NA_crop_production_imputed = ifelse(name %in% production_na, 1, 0)) %>% # NA because no production information, therefore imputed with non-weighted average predictions (31 countries)
  # summarise(sum = sum(across(c(NA_crop_calendar:NA_crop_production_imputed)))) %>% # check, should be 78
  readr::write_csv(here("results", "tables", "country_NA_log.csv"))


# GTAP NA LOG -------------------------------------------------------------

# this will vary by crop

lapply(1:4, function(i) {
  
  sum(is.na(gtap_region_weighted_results[[i]][4])) # 15, 33, 29, 27 - should mostly be due to NA production
  
})


# after coalescing with non-weighted data, the only NA results are in malta for maize, rice, soybean but not wheat
lapply(1:4, function(i) {
  
  sum(is.na(gtap_region_nonweighted_results[[i]][4])) # 1, 1, 1, 0
  
})

# which countries?

lapply(1:4, function(i) {
  
  gtap_region_nonweighted_results[[i]] %>% 
    filter(is.na(`0 degrees`)) %>% 
    ungroup() %>% 
    dplyr::select(n141) # Malta -
  # suspect this is because other small island countries have been aggregated into Rest of Oceania or Carribbean where at least one non-NA result is used for the region
  # except for Malta, which remains its own GTAP region. And wheat results exist for Malta due to presence of prediction baseline temperature data
  
})

gtap_na_log_full <- mapping %>% 
  mutate(NA_maize_crop_production = ifelse(Country %in% countries_maize_na_production$name, 1, 0),
         NA_rice_crop_production = ifelse(Country %in% countries_rice_na_production$name, 1, 0),
         NA_soybean_crop_production = ifelse(Country %in% countries_soybean_na_production$name, 1, 0),
         NA_wheat_crop_production = ifelse(Country %in% countries_wheat_na_production$name, 1, 0)) %>%  
  group_by(id141, n141) %>% 
  summarise(n_countries = n(),
            sum_na_maize_production = sum(NA_maize_crop_production),
            sum_na_rice_production = sum(NA_rice_crop_production),
            sum_na_soybean_production = sum(NA_soybean_crop_production),
            sum_na_wheat_production = sum(NA_wheat_crop_production)) %>% 
  mutate(maize_na_weighted = ifelse(sum_na_maize_production == n_countries, 1, 0),
         rice_na_weighted = ifelse(sum_na_rice_production == n_countries, 1, 0),
         soybean_na_weighted = ifelse(sum_na_soybean_production == n_countries, 1, 0),
         wheat_na_weighted = ifelse(sum_na_wheat_production == n_countries, 1, 0)) #%>% 

gtap_na_log_full %>% readr::write_csv(here("results", "tables", "gtap_NA_log.csv"))


gtap_na_log_full %>% 
  ungroup() %>% # as a check, this reproduces the same sum NAs
  summarise(sum_maize_na = sum(maize_na_weighted),
            sum_rice_na = sum(rice_na_weighted),
            sum_soybean_na = sum(soybean_na_weighted),
            sum_wheat_na = sum(wheat_na_weighted))

# malta is the outstanding NA for non-wheat crops; it comes down to the savg results, why are these populated for only wheat?
# perhaps there is crop calendar information only for wheat in malta? exactly
# there is no other calendar information for other crops in malta
# and there is no crop calendar information for many Oceania countries, 
# but there are at least one country with non-NA results that mean a non-NA result is generated for the region overall
# the same cannot be said for Malta
# check hypothesis by looking at crop calendar information for other 'wheat' varieties in Malta
# COALESCE/IMPUTE ONLY AFTER AGGREGATING TO GTAP REGION -------------------


# at the gtap region, we have been coalescing and then aggregating. 
# but perhaps... it would be better to do weighted average to the greatest extent, and then impute with nonweighted results?
# it could be better because it would maintain the production-weighting as far as possible (to GTAP region level)
# no changes to GTAP log, but slight differences in results for Chile, Luxembourg, and EFTA

# MERGE WITH MAPPING FILE 

# left_join on Admin, Country and id243/X1, new vars will be c141, id141, n141

predicted_yield_country_v2 <- lapply(1:4, function(i) {
  
  left_join(predicted_yield_country_reweighted[[i]],  # using coalesced results, not predicted_yield_country_reweighted[[i]] which report only the production-weighted country averages
            mapping,
            by = c("Country",
                   "Admin"))
})

# now do non-grid-cell-weighted country averages


predicted_yield_country_v2_nonweighted <- lapply(1:4, function(i) {
  
  left_join(predicted_yield_country_savg[[i]],  # using coalesced results, not predicted_yield_country_reweighted[[i]] which report only the production-weighted country averages
            mapping,
            by = c("Country",
                   "Admin"))
})

# SIMPLE AVERAGE OVER 141 REGIONS - this is unnecessary as already doing this later as non-weighted, unless need to differentiate between non-coalesced and coalesced country results

gtap_region_nonweighted_results_v2 <- lapply(1:4, function(i) {
  
  predicted_yield_country_v2_nonweighted[[i]] %>%
    group_by(id141, n141, c141) %>% 
    summarise_at(vars(`0 degrees`:`5 degrees`), mean, na.rm=TRUE)
  
})

# write to csv

crops <- c("Maize", "Rice", "Soybean", "Wheat")

# save in the processed folder

lapply(1:4, function(i) {
  
  readr::write_csv(gtap_region_nonweighted_results_v2[[i]], 
                   here("results", "tables", paste0("gtap_region_nonweighted_", crops[[i]], "_v2", ".csv")))
  
}) 


# PRODUCTION-WEIGHTED GTAP REGIONAL AVERAGE 

# Transform country production volumes into list to make merging by crop country result easier

crop_production_list <- list(
  data.frame(name = crop_production_csv$name, Y = crop_production_csv$Y_maize),
  data.frame(name = crop_production_csv$name, Y = crop_production_csv$Y_rice),
  data.frame(name = crop_production_csv$name, Y = crop_production_csv$Y_soybean),
  data.frame(name = crop_production_csv$name, Y = crop_production_csv$Y_wheat))

# merge country_results with crop_production data

country_production_results_v2 <- lapply(1:4, function(i){
  
  left_join(predicted_yield_country_v2[[i]], 
            crop_production_list[[i]],
            by = c("Country" = "name")) 
})

# Calculate production weighted regional average

gtap_region_weighted_results_v2 <- lapply(1:4, function(i) {
  
  country_production_results_v2[[i]] %>%
    group_by(id141, n141, c141) %>% 
    summarise(across(`0 degrees`:`5 degrees`, ~weighted.mean(., w = Y, na.rm=TRUE)))
  
})


lapply(1:4, function(i) {
  
  readr::write_csv(gtap_region_weighted_results_v2[[i]], 
                   here("results", "tables", paste0("predicted_gtap_weighted_", crops[[i]], "_v2", ".csv")))
  
}) 

lapply(1:4, function(i) {
  
  sum(is.na(gtap_region_weighted_results_v2[[i]][4])) # 15, 33, 29, 27 - actually same as for gtap_region_weighted_results (v1)
  
})

# now coalesce for remaining NA GTAP regions

predicted_gtap_coalesced_v2 <- lapply(1:4, function(i) { 
  data.frame(
                          id141 = gtap_region_weighted_results_v2[[i]][1],
                          n141 = gtap_region_weighted_results_v2[[i]][2],
                          c141 = gtap_region_weighted_results_v2[[i]][3],
                          "0 degrees" = coalesce(gtap_region_weighted_results_v2[[i]][4], gtap_region_nonweighted_results_v2[[i]][4]),
                          "1 degree" = coalesce(gtap_region_weighted_results_v2[[i]][5], gtap_region_nonweighted_results_v2[[i]][5]),
                          "2 degrees" = coalesce(gtap_region_weighted_results_v2[[i]][6], gtap_region_nonweighted_results_v2[[i]][6]),
                          "3 degrees" = coalesce(gtap_region_weighted_results_v2[[i]][7], gtap_region_nonweighted_results_v2[[i]][7]),
                          "4 degrees" = coalesce(gtap_region_weighted_results_v2[[i]][8], gtap_region_nonweighted_results_v2[[i]][8]),
                          "5 degrees" = coalesce(gtap_region_weighted_results_v2[[i]][9], gtap_region_nonweighted_results_v2[[i]][9])) %>% 
    rename("0 degrees" = 4,
           "1 degree" = 5,
           "2 degrees" = 6,
           "3 degrees" = 7,
           "4 degrees" = 8,
           "5 degrees" = 9) %>% 
    mutate(imputed = ifelse(!is.na(gtap_region_weighted_results_v2[[i]][4]), 0, # must record list of countries coalesced from savg results
                            ifelse(is.na(gtap_region_weighted_results_v2[[i]][4]) & !is.na(gtap_region_nonweighted_results_v2[[i]][4]), 1, 0))) %>% 
             rename(imputed = 10)
})


lapply(1:4, function(i) {
  
  readr::write_csv(predicted_gtap_coalesced_v2[[i]], 
                   here("results", "tables", paste0("predicted_gtap_coalesced_", crops[[i]], "_v2", " .csv")))
  
}) 

# count the number of remaining NAs in GTAP results for each crop, compared to v1

lapply(1:4, function(i) {
  
  sum(is.na(predicted_gtap_coalesced_v2[[i]][4])) # 1, 1, 1, 0 - should mostly be due to NA crop calendar
  
})

# count the number of imputed/coalesced results for GTAP regions for each crop, compared to v1

lapply(1:4, function(i) {
  
  sum(predicted_gtap_coalesced_v2[[i]][10]) # 14, 32, 28, 27 
  
})

# compare
lapply(1:4, function(i) {
  data.frame(id141 = predicted_gtap_coalesced[[i]][1],
             n141 = predicted_gtap_coalesced[[i]][2],
             c141 = predicted_gtap_coalesced[[i]][3],
             "0 degrees" = predicted_gtap_coalesced_v2[[i]][4] - predicted_gtap_coalesced[[i]][4],
             "1 degree" = predicted_gtap_coalesced_v2[[i]][5] - predicted_gtap_coalesced[[i]][5],
             "2 degrees" = predicted_gtap_coalesced_v2[[i]][6] - predicted_gtap_coalesced[[i]][6],
             "3 degrees" = predicted_gtap_coalesced_v2[[i]][7] - predicted_gtap_coalesced[[i]][7],
             "4 degrees" = predicted_gtap_coalesced_v2[[i]][8] - predicted_gtap_coalesced[[i]][8],
             "5 degrees" = predicted_gtap_coalesced_v2[[i]][9] - predicted_gtap_coalesced[[i]][9])
})

# differences for Chile and Luxembourg for soy, and EFTA for all crops

# do other logs

# process is: take grid cell production weighted country average, then country production weighted gtap average
# then impute NAs with non-weighted results, which are non-grid-cell-weighted country results and non-country-weighted gtap results 
# log should not be any different, even though there are slight calculation differences

# REPEAT FOR CONFIDENCE INTERVALS -----------------------------------------

# calculate country weighted average CI results (repeat of 15_predict_country.R but for tables not plots)

# 2.5% percentile
pooled_country_conf_lwr_dc <- function(i){
  
  raster_0dc <- raster(ncol = 720, nrow = 360) 
  values(raster_0dc) <-  pooled_predictions[[i]][["conf_lwr_0"]]
  
  raster_1dc <- raster(ncol = 720, nrow = 360) 
  values(raster_1dc) <- pooled_predictions[[i]][["conf_lwr_1"]]
  
  raster_2dc <- raster(ncol = 720, nrow = 360) 
  values(raster_2dc) <- pooled_predictions[[i]][["conf_lwr_2"]]
  
  raster_3dc <- raster(ncol = 720, nrow = 360) 
  values(raster_3dc) <- pooled_predictions[[i]][["conf_lwr_3"]]
  
  raster_4dc <- raster(ncol = 720, nrow = 360) 
  values(raster_4dc) <- pooled_predictions[[i]][["conf_lwr_4"]]
  
  raster_5dc <- raster(ncol = 720, nrow = 360) 
  values(raster_5dc) <- pooled_predictions[[i]][["conf_lwr_5"]]
  
  raster_alldc_stack <- stack(raster_0dc, raster_1dc, raster_2dc, raster_3dc, raster_4dc, raster_5dc)
  
  
  predicted_yield_all_dc_country <- exactextractr::exact_extract(raster_alldc_stack, worldmap_clean, 'weighted_mean', weights = crop_production_stack[[i]])
  
  predicted_yield_all_dc_country <- predicted_yield_all_dc_country %>% rename("0 degrees" = 1, "1 degree" = 2, "2 degrees" = 3,
                                                                              "3 degrees" = 4, "4 degrees" = 5, "5 degrees" = 6)
  
  worldmap_copy <- worldmap
  worldmap_copy@data <- worldmap_copy@data %>% 
    dplyr::select(c("NAME", "ISO_A2")) %>% 
    cbind(predicted_yield_all_dc_country) 
  
}


pooled_country_predictions_conf_lwr <- lapply(1:4, pooled_country_conf_lwr_dc)


# 97.5% percentile
pooled_country_conf_upr_dc <- function(i){
  
  raster_0dc <- raster(ncol = 720, nrow = 360) 
  values(raster_0dc) <-  pooled_predictions[[i]][["conf_upr_0"]]
  
  raster_1dc <- raster(ncol = 720, nrow = 360) 
  values(raster_1dc) <- pooled_predictions[[i]][["conf_upr_1"]]
  
  raster_2dc <- raster(ncol = 720, nrow = 360) 
  values(raster_2dc) <- pooled_predictions[[i]][["conf_upr_2"]]
  
  raster_3dc <- raster(ncol = 720, nrow = 360) 
  values(raster_3dc) <- pooled_predictions[[i]][["conf_upr_3"]]
  
  raster_4dc <- raster(ncol = 720, nrow = 360) 
  values(raster_4dc) <- pooled_predictions[[i]][["conf_upr_4"]]
  
  raster_5dc <- raster(ncol = 720, nrow = 360) 
  values(raster_5dc) <- pooled_predictions[[i]][["conf_upr_5"]]
  
  raster_alldc_stack <- stack(raster_0dc, raster_1dc, raster_2dc, raster_3dc, raster_4dc, raster_5dc)
  
  
  predicted_yield_all_dc_country <- exactextractr::exact_extract(raster_alldc_stack, worldmap_clean, 'weighted_mean', weights = crop_production_stack[[i]])
  
  predicted_yield_all_dc_country <- predicted_yield_all_dc_country %>% rename("0 degrees" = 1, "1 degree" = 2, "2 degrees" = 3,
                                                                              "3 degrees" = 4, "4 degrees" = 5, "5 degrees" = 6)
  
  worldmap_copy <- worldmap
  worldmap_copy@data <- worldmap_copy@data  %>% 
    dplyr::select(c("NAME", "ISO_A2")) %>% 
    cbind(predicted_yield_all_dc_country) 
  
  
}


pooled_country_predictions_conf_upr <- lapply(1:4, pooled_country_conf_upr_dc)
# calculate country non-weighted average CI results

# 2.5% percentile
pooled_country_conf_lwr_dc_savg <- function(i){
  
  raster_0dc <- raster(ncol = 720, nrow = 360) 
  values(raster_0dc) <-  pooled_predictions[[i]][["conf_lwr_0"]]
  
  raster_1dc <- raster(ncol = 720, nrow = 360) 
  values(raster_1dc) <- pooled_predictions[[i]][["conf_lwr_1"]]
  
  raster_2dc <- raster(ncol = 720, nrow = 360) 
  values(raster_2dc) <- pooled_predictions[[i]][["conf_lwr_2"]]
  
  raster_3dc <- raster(ncol = 720, nrow = 360) 
  values(raster_3dc) <- pooled_predictions[[i]][["conf_lwr_3"]]
  
  raster_4dc <- raster(ncol = 720, nrow = 360) 
  values(raster_4dc) <- pooled_predictions[[i]][["conf_lwr_4"]]
  
  raster_5dc <- raster(ncol = 720, nrow = 360) 
  values(raster_5dc) <- pooled_predictions[[i]][["conf_lwr_5"]]
  
  raster_alldc_stack <- stack(raster_0dc, raster_1dc, raster_2dc, raster_3dc, raster_4dc, raster_5dc)
  
  
  predicted_yield_all_dc_country <- exactextractr::exact_extract(raster_alldc_stack, worldmap_clean, 'mean')
  
  predicted_yield_all_dc_country <- predicted_yield_all_dc_country %>% rename("0 degrees" = 1, "1 degree" = 2, "2 degrees" = 3,
                                                                              "3 degrees" = 4, "4 degrees" = 5, "5 degrees" = 6)
  
  worldmap_copy <- worldmap
  worldmap_copy@data <- worldmap_copy@data  %>% 
    dplyr::select(c("NAME", "ISO_A2")) %>% 
    cbind(predicted_yield_all_dc_country) 
  
}


pooled_country_predictions_conf_lwr_savg <- lapply(1:4, pooled_country_conf_lwr_dc_savg)


# 97.5% percentile
pooled_country_conf_upr_dc_savg <- function(i){
  
  raster_0dc <- raster(ncol = 720, nrow = 360) 
  values(raster_0dc) <-  pooled_predictions[[i]][["conf_upr_0"]]
  
  raster_1dc <- raster(ncol = 720, nrow = 360) 
  values(raster_1dc) <- pooled_predictions[[i]][["conf_upr_1"]]
  
  raster_2dc <- raster(ncol = 720, nrow = 360) 
  values(raster_2dc) <- pooled_predictions[[i]][["conf_upr_2"]]
  
  raster_3dc <- raster(ncol = 720, nrow = 360) 
  values(raster_3dc) <- pooled_predictions[[i]][["conf_upr_3"]]
  
  raster_4dc <- raster(ncol = 720, nrow = 360) 
  values(raster_4dc) <- pooled_predictions[[i]][["conf_upr_4"]]
  
  raster_5dc <- raster(ncol = 720, nrow = 360) 
  values(raster_5dc) <- pooled_predictions[[i]][["conf_upr_5"]]
  
  raster_alldc_stack <- stack(raster_0dc, raster_1dc, raster_2dc, raster_3dc, raster_4dc, raster_5dc)
  
  
  predicted_yield_all_dc_country <- exactextractr::exact_extract(raster_alldc_stack, worldmap_clean, 'mean')
  
  predicted_yield_all_dc_country <- predicted_yield_all_dc_country %>% rename("0 degrees" = 1, "1 degree" = 2, "2 degrees" = 3,
                                                                              "3 degrees" = 4, "4 degrees" = 5, "5 degrees" = 6)
  
  worldmap_copy <- worldmap
  worldmap_copy@data <- worldmap_copy@data %>% 
    dplyr::select(c("NAME", "ISO_A2")) %>% 
    cbind(predicted_yield_all_dc_country) 
  
  
}


pooled_country_predictions_conf_upr_savg <- lapply(1:4, pooled_country_conf_upr_dc_savg)

# Aggregate to GTAP region for both sets of CI results

# Weighted, lower


gtap_region_weighted_results_conf_lwr <- lapply(1:4, function(i) {
  
  pooled_country_predictions_conf_lwr[[i]] %>%
    left_join(crop_production_list[[i]], by = c("NAME" = "name")) %>% 
    left_join(mapping, by = c("NAME" = "Country")) %>% 
    group_by(id141, n141, c141) %>% 
    summarise(across(`0 degrees`:`5 degrees`, ~weighted.mean(., w = Y, na.rm=TRUE)))
  
})

# nonweighted, lower
gtap_region_nonweighted_results_conf_lwr <- lapply(1:4, function(i) {
  
  pooled_country_predictions_conf_lwr_savg[[i]] %>%
    left_join(mapping, by = c("NAME" = "Country")) %>% 
    group_by(id141, n141, c141) %>% 
    summarise_at(vars(`0 degrees`:`5 degrees`), mean, na.rm=TRUE)
  
})

# Weighted, upper


gtap_region_weighted_results_conf_upr <- lapply(1:4, function(i) {
  
  pooled_country_predictions_conf_upr[[i]] %>%
    left_join(crop_production_list[[i]], by = c("NAME" = "name")) %>% 
    left_join(mapping, by = c("NAME" = "Country")) %>%
    group_by(id141, n141, c141) %>% 
    summarise(across(`0 degrees`:`5 degrees`, ~weighted.mean(., w = Y, na.rm=TRUE)))
  
})

# nonweighted, upper
gtap_region_nonweighted_results_conf_upr <- lapply(1:4, function(i) {
  
  pooled_country_predictions_conf_upr_savg[[i]] %>%
    left_join(mapping, by = c("NAME" = "Country")) %>% 
    group_by(id141, n141, c141) %>% 
    summarise_at(vars(`0 degrees`:`5 degrees`), mean, na.rm=TRUE)
  
})

# now coalesce for remaining NA GTAP regions

predicted_gtap_coalesced_conf_lwr <- lapply(1:4, function(i) { 
  data.frame(
    id141 = gtap_region_weighted_results_conf_lwr[[i]][1],
    n141 = gtap_region_weighted_results_conf_lwr[[i]][2],
    c141 = gtap_region_weighted_results_conf_lwr[[i]][3],
    "0 degrees" = coalesce(gtap_region_weighted_results_conf_lwr[[i]][4], gtap_region_nonweighted_results_conf_lwr[[i]][4]),
    "1 degree" = coalesce(gtap_region_weighted_results_conf_lwr[[i]][5], gtap_region_nonweighted_results_conf_lwr[[i]][5]),
    "2 degrees" = coalesce(gtap_region_weighted_results_conf_lwr[[i]][6], gtap_region_nonweighted_results_conf_lwr[[i]][6]),
    "3 degrees" = coalesce(gtap_region_weighted_results_conf_lwr[[i]][7], gtap_region_nonweighted_results_conf_lwr[[i]][7]),
    "4 degrees" = coalesce(gtap_region_weighted_results_conf_lwr[[i]][8], gtap_region_nonweighted_results_conf_lwr[[i]][8]),
    "5 degrees" = coalesce(gtap_region_weighted_results_conf_lwr[[i]][9], gtap_region_nonweighted_results_conf_lwr[[i]][9])) %>% 
    rename("0 degrees" = 4,
           "1 degree" = 5,
           "2 degrees" = 6,
           "3 degrees" = 7,
           "4 degrees" = 8,
           "5 degrees" = 9) %>% 
    mutate(imputed = ifelse(!is.na(gtap_region_weighted_results_conf_lwr[[i]][4]), 0, # must record list of countries coalesced from savg results
                            ifelse(is.na(gtap_region_weighted_results_conf_lwr[[i]][4]) & !is.na(gtap_region_nonweighted_results_conf_lwr[[i]][4]), 1, 0))) %>% 
    rename(imputed = 10)
})

predicted_gtap_coalesced_conf_upr <- lapply(1:4, function(i) { 
  data.frame(
    id141 = gtap_region_weighted_results_conf_upr[[i]][1],
    n141 = gtap_region_weighted_results_conf_upr[[i]][2],
    c141 = gtap_region_weighted_results_conf_upr[[i]][3],
    "0 degrees" = coalesce(gtap_region_weighted_results_conf_upr[[i]][4], gtap_region_nonweighted_results_conf_upr[[i]][4]),
    "1 degree" = coalesce(gtap_region_weighted_results_conf_upr[[i]][5], gtap_region_nonweighted_results_conf_upr[[i]][5]),
    "2 degrees" = coalesce(gtap_region_weighted_results_conf_upr[[i]][6], gtap_region_nonweighted_results_conf_upr[[i]][6]),
    "3 degrees" = coalesce(gtap_region_weighted_results_conf_upr[[i]][7], gtap_region_nonweighted_results_conf_upr[[i]][7]),
    "4 degrees" = coalesce(gtap_region_weighted_results_conf_upr[[i]][8], gtap_region_nonweighted_results_conf_upr[[i]][8]),
    "5 degrees" = coalesce(gtap_region_weighted_results_conf_upr[[i]][9], gtap_region_nonweighted_results_conf_upr[[i]][9])) %>% 
    rename("0 degrees" = 4,
           "1 degree" = 5,
           "2 degrees" = 6,
           "3 degrees" = 7,
           "4 degrees" = 8,
           "5 degrees" = 9) %>% 
    mutate(imputed = ifelse(!is.na(gtap_region_weighted_results_conf_upr[[i]][4]), 0, # must record list of countries coalesced from savg results
                            ifelse(is.na(gtap_region_weighted_results_conf_upr[[i]][4]) & !is.na(gtap_region_nonweighted_results_conf_upr[[i]][4]), 1, 0))) %>% 
    rename(imputed = 10)
})

# write to csv

lapply(1:4, function(i) {
  
  readr::write_csv(predicted_gtap_coalesced_conf_lwr[[i]], 
                   here("results", "tables", paste0("predicted_gtap_coalesced_conf_lwr_", crops[[i]], ".csv")))
  
}) 

lapply(1:4, function(i) {
  
  readr::write_csv(predicted_gtap_coalesced_conf_upr[[i]], 
                   here("results", "tables", paste0("predicted_gtap_coalesced_conf_upr_", crops[[i]], ".csv")))
  
}) 

# fairly narrow confidence intervals for rice and soy, much wider for maize and wheat 