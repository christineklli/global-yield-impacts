
r <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, crs=4326, res=0.5)

# read in crop calendar and season data ------------------------------------------------------------

# gridded growing season length and growing month indicators for combined Sacks and MIRCA data 
crop_season <- readRDS(here("processed", "crop_season.RData")) #dim(crop_season) # 2013110      18
# 15 rasterbricks, one for each crop, showing growing season binary variables for each month
raster_extended_crop_season <- readRDS(here("processed", "raster_extended_crop_season.RData"))
# gridded crop calendar information - planting and harvesting month
crop_calendar <- readRDS(here("processed", "crop_calendar.RData"))

# manipulate crop calendar and season data -------------------

crop_season_extended <- lapply(1:15, function(i){
  matrix <- raster_extended_crop_season[[i]][] # turn raster of full 259200 grid cells for 15 crop varieties into matrix
  matrix[,1] <- unique(crop_season$crop)[i] # crop name
  matrix <- as.data.frame(matrix)
  matrix$lon <- crop_calendar$lon[1:259200] # full list replicated x 7 crops
  matrix$lat <- crop_calendar$lat[1:259200]
  matrix
})

crop_season_extended <- rbindlist(crop_season_extended)

# crop_season_extended is then the extended dataframe of crop_season and should have 15 x 259200 rows, unlike crop_season (recall MIRCA crops had incomplete grid cells where no calendar values were available)
dim(crop_season_extended) # 3888000 18

cols <- c("n_growing_months", "jan_growing", "feb_growing", "mar_growing", "apr_growing", "may_growing", "jun_growing", "jul_growing", "aug_growing", "sept_growing", "oct_growing", "nov_growing", "dec_growing")
crop_season_extended_numeric <- crop_season_extended # note copy, remove from environment
# some cols are character when they should be numeric
crop_season_extended_numeric[,(cols):=lapply(.SD, as.numeric), .SDcols=cols]

rm(crop_season_extended)

# create tmp_x_wide x 11 times - and calculate annual average temperatures one at a time, remove when done for memory management
# for each dataframe, mutate new variable with major crop variety level, group_by major crop level and take average baseline temperatures over the sub-crop-variety temperatures
# however, need lon lat points in crop_season_extended

# only keep 9 crops in crop_season_extended_numeric

unique(crop_season_extended_numeric$crop)

crop_season_extended_subset <- crop_season_extended_numeric %>% 
  filter(crop %in% c("Rice", "Rice (Rainfed)", "Rice (Irrigated)", "Wheat", "Wheat.Winter", "Wheat (Rainfed)", "Wheat (Irrigated)", "Maize", "Soybeans"))


# read in CRU temp data -------------------------------------------------------

# read in temp data
tmps_2011_2020 <- nc_open(here("data", "CRU data", "cru_ts4.05.2011.2020.tmp.dat.nc"))

# store values from variables and attributes
nc_tmp_2011_2020 <- ncvar_get(tmps_2011_2020, attributes(tmps_2011_2020$var)$names[1])
nc_tmps_lon_2011_2020 <- ncvar_get(tmps_2011_2020,attributes(tmps_2011_2020$dim)$names[1])
nc_tmps_lat_2011_2020 <- ncvar_get(tmps_2011_2020, attributes(tmps_2011_2020$dim)$names[2])
nc_tmps_time_2011_2020 <- ncvar_get(tmps_2011_2020, attributes(tmps_2011_2020$dim)$names[3]) # 120 months
nc_tmps_atts_2011_2020 <- ncatt_get(tmps_2011_2020, 0)

# set dimension names and values to corresponding lon and lat values
dimnames(nc_tmp_2011_2020) <- list(lon = nc_tmps_lon_2011_2020, 
                                   lat = nc_tmps_lat_2011_2020, 
                                   time = nc_tmps_time_2011_2020)


tmp_2011_2020_df <- reshape2::melt(nc_tmp_2011_2020, value.name = "tmp") 
tmp_2011_2020_df$date <- as.Date(tmp_2011_2020_df$time, origin = "1900-1-1")
tmp_2011_2020_df <- tmp_2011_2020_df %>% dplyr::select(!time)


# read in CRU precip data -----------------------------------------------------

pre_2011_2020 <- nc_open(here("data", "CRU data", "cru_ts4.05.2011.2020.pre.dat.nc"))

# store values from variables and attributes
nc_pre_2011_2020 <- ncvar_get(pre_2011_2020, attributes(pre_2011_2020$var)$names[1])
nc_pre_lon_2011_2020 <- ncvar_get(pre_2011_2020,attributes(pre_2011_2020$dim)$names[1])
nc_pre_lat_2011_2020 <- ncvar_get(pre_2011_2020, attributes(pre_2011_2020$dim)$names[2])
nc_pre_time_2011_2020 <- ncvar_get(pre_2011_2020, attributes(pre_2011_2020$dim)$names[3]) # 120 months
nc_pre_atts_2011_2020 <- ncatt_get(pre_2011_2020, 0)

# set dimension names and values to corresponding lon and lat values
dimnames(nc_pre_2011_2020) <- list(lon = nc_pre_lon_2011_2020, 
                                   lat = nc_pre_lat_2011_2020, 
                                   time = nc_pre_time_2011_2020)


pre_2011_2020_df <- reshape2::melt(nc_pre_2011_2020, value.name = "pre") 
pre_2011_2020_df$date <- as.Date(pre_2011_2020_df$time, origin = "1900-1-1")
pre_2011_2020_df <- pre_2011_2020_df %>% dplyr::select(!time)


# calculate mean annual temp and precip for 2015 --------------------------

tmp_mean_2015 <- tmp_2011_2020_df %>% 
  as.data.table() %>% 
  filter(date >="2015-01-16" & date <= "2015-12-16") %>% 
  group_by(lon,lat) %>% 
  summarise(tmp_2015 = mean(tmp, na.rm=TRUE)) %>% 
  ungroup()

pre_mean_2015 <- pre_2011_2020_df %>% 
  as.data.table() %>% 
  filter(date >="2015-01-16" & date <= "2015-12-16") %>% 
  group_by(lon,lat) %>% 
  summarise(pre_2015 = mean(pre, na.rm=TRUE)) %>% 
  ungroup()


# calculate baseline average growing season temp and precip for each year of 2015-2020 from CRU data  -------------------------------------------------------------------

cru_vars <- c("tmp_2011_2020_df", "pre_2011_2020_df") 
var <- c("tmp", "pre")

calc_bs_var <- function(YEAR.START, YEAR.END, i) { # note that i relates to tmp, pre data
  
  YEAR <- eval(rlang::parse_expr(cru_vars[[i]])) %>% 
    dplyr::filter(date >= as.Date(YEAR.START) & date <= as.Date(YEAR.END)) # this changes dynamically
  
  # need to reshape wide by month
  YEAR_WIDE <- tidyr::spread(YEAR, date, eval(rlang::parse_expr(var[[i]])))
  colnames(YEAR_WIDE) <- paste(var[[i]], colnames(YEAR_WIDE), sep = "_")
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

# calc_bs_var(YEAR.START = "2000-01-16", YEAR.END = "2000-12-16", i = 1)

bs_years = data.frame(YEAR.START = c("2015-01-16", "2016-01-16", "2017-01-16", "2018-01-16", "2019-01-16", "2020-01-16"), 
                      YEAR.END = c("2015-12-16", "2016-12-16", "2017-12-16", "2018-12-16", "2019-12-16", "2020-12-16"))

# calculate baseline growing season temperature averaged over 2015-2020

bs_years_tmp <- apply(bs_years[,c("YEAR.START", "YEAR.END")], 1, function(x) calc_bs_var(x[1], x[2], 1))

# calculate baseline growing season precipitation averaged over 2015-2020

bs_years_pre <- apply(bs_years[,c("YEAR.START", "YEAR.END")], 1, function(x) calc_bs_var(x[1], x[2], 2))

# save these

saveRDS(bs_years_tmp, here("processed", "bs_years_tmp.RData")) # list of six dataframes, one for each year from 2015-2020
saveRDS(bs_years_pre, here("processed", "bs_years_pre.RData"))

# cbind precip into the same dataframe as temp for each of 2015-2020 (6) tables in the list
# note cbind here instead of join by is ok because temp and precip data have exactly the same format

bs_years_tmp_pre <- lapply(1:6, function(i) {
  cbind(bs_years_tmp[[i]], bs_years_pre[[i]][5]) %>%
    rename(bs_gs_tmp = 5, bs_gs_pre = 6) %>% 
    dplyr::select(!c("year")) %>% 
    as.data.table()}) 

# focus on 2015 - extract first element dataframe
bs_2015_tmp_pre <- bs_years_tmp_pre[[1]]

# group_split into list by crop_pooled

bs_2015_tmp_pre_list <- bs_2015_tmp_pre %>% group_split(crop_pooled)


# at later point, if wanting 2015-2020 average baseline period, can do group_by means for averaging over 2015-2020, group-split into list by crop_pooled

# join with baseline yield data by lon/lat

# read in GDHY yield data -------------------------------------------------

# only read in raster data for year 2015
# extract coordinates too
# aim is to have a df similar to tmp and pre, except with 3 cols and 259200 rows (lon, lat, yield)

crops_lwr <- c("maize", "rice", "soybean", "wheat")

gdhy_function <- function(i,j) {
  
  crop_yields_ts <- list.files(here("data", "GDHY data", crops_lwr[[j]]), pattern = "^.*\\.(nc4)$")
  
  data <- nc_open(here("data", "GDHY data", crops_lwr[[j]], crop_yields_ts[[i]]))
  
  lon <- ncvar_get(data, "lon")
  lat <- ncvar_get(data, "lat")
  yields <- ncvar_get(data, "var")
  
  fillvalue <- ncatt_get(data, "var", "_FillValue")
  
  yields[yields == fillvalue$value] <- NA
  
  # set dimension names and values to corresponding lon and lat values
  dimnames(yields) <- list(lon = lon, lat = lat)
  
  # transform into dataframe
  colnames(yields) <- c(seq(-89.75, 89.75, 0.5)) # lat
  rownames(yields) <- c(seq(0.25, 359.75, 0.5)) # lon, this needs to be converted to -180 to 180 # -179.75, 179.75, 0.5 # 0.25, 359.75
  
  yields <- reshape2::melt(yields)
  
  yields %>% rename(bs_yield=value) %>% 
    mutate(lon = ifelse(lon > 180, -(360-lon), lon)) %>% 
    as.data.table()
  
}

crop_yields_df <- lapply(1:4, function(j){
  
  lapply(35, gdhy_function,j)})
    

crop_yields_df <- unlist(crop_yields_df, recursive=FALSE)


# read in GAEZ 2015 yield data --------------------------------------------
# note this is an alternative to GDHY yield data[[35]] because GDHY yield data contain so many NAs
# as such final output will look exactly like crop_yields_df

crop_yield_gaez <- c("GAEZAct2015_Yield_Maize_Mean.tif",
                     "GAEZAct2015_Yield_Rice_Mean.tif",
                     "GAEZAct2015_Yield_Soybean_Mean.tif",
                     "GAEZAct2015_Yield_Wheat_Mean.tif")

crop_yield_gaez <- here("data", "GAEZ data", crop_yield_gaez)

# rasterise 

crop_yield_gaez_raster <- lapply(crop_yield_gaez, raster)

res.factor <- raster::res(r)/raster::res(crop_yield_gaez_raster[[1]]) # 0.5 0.5 / 0.083 0.083 

crop_yield_gaez_agg <- lapply(1:4, function(i){
  raster::aggregate(crop_yield_gaez_raster[[i]], fact = res.factor, fun = mean)
  }
)
# v large max values for each rasterlayer, check this

# turn crop_production_gaez_raster into df
# multiply pooled_predictions list element by element, by crop_production_gaez_raster

crop_yield_gaez_dt <- lapply(1:4, function(i) {
  raster::as.data.frame(crop_yield_gaez_agg[[i]], xy=TRUE) %>% 
    rename(lon=x, lat=y, bs_yield=3) %>% 
    as.data.table})

sum(is.na(crop_yield_gaez_dt[[1]]$bs_yield)) # 232199 only slightly better than GDHY data - still missing a lot
sum(is.na(crop_yields_df[[1]]$bs_yield)) # 248065
# note that CGIAR baseline yield data is still sourced from GDHY as values are time-varying


# read in Monfreda yield data (2000) --------------------------------------

crop_yield_monf <- c("maize_YieldPerHectare.tif",
                     "rice_YieldPerHectare.tif",
                     "soybean_YieldPerHectare.tif",
                     "wheat_YieldPerHectare.tif")

crop_yield_monf <- here("data", "Monfreda data", crop_yield_monf)

# rasterise and stack

crop_yield_monf_raster <- lapply(crop_yield_monf, raster)

crop_yield_monf_agg <- lapply(1:4, function(i){
  raster::aggregate(crop_yield_monf_raster[[i]], fact = res.factor, fun = mean)
}
)

crop_yield_monf_dt <- lapply(1:4, function(i) {
  raster::as.data.frame(crop_yield_monf_agg[[i]], xy=TRUE) %>% 
    rename(lon=x, lat=y, bs_yield=3) %>% 
    as.data.table})

sum(is.na(crop_yield_monf_dt[[1]]$bs_yield)) # 191812
# significantly more data than both GDHY(2015) and GAEZ(2015), but trade-off is assumption that yields have not changed significantly in 15 years


# compare monfreda 2000 yield data and gdhy 2015 yield data ---------------

# merge with coords_countries data and calculate % diff between GDHY2015 and Monfreda 2000


# calculate gdhy 2000 data

data.frame(year=c(1981:2016)) %>% 
  mutate(id=1:n())

crop_yields2000_df <- lapply(1:4, function(j){
  lapply(20, gdhy_function,j)})

crop_yields2000_df <- unlist(crop_yields2000_df, recursive=FALSE)

sum(is.na(crop_yields2000_df[[1]]$bs_yield)) # 244121 - still way more NAs than Monfreda for same year

# calculate gdhy2000-2015 % change

# apply % change to monfreda 2000 data to extrapolate monfreda 2015 values

# however this will only work for about 20,000 pixels, the remaining 220,000 pixels will have no values for gdhy2000 either?
# not necessarily - NA values may vary with year in gdhy

# merge tmp, precip and yield data for baseline year 2015 -----------------

# note that yield data coordinates not in same order, need to use join_by for safety

bs_2015_tmp_pre_yld <- lapply(1:4, function(i){
                      bs_2015_tmp_pre_list[[i]] %>% 
                        left_join(crop_yield_monf_dt[[i]], # swap with crop_yields_df[[i]] for GDHY data instead
                                  by=c("lon","lat"))
  })
 
# check this by plotting yields as a test

raster_yields <- bs_2015_tmp_pre_yld[[1]] %>%
  dplyr::select(lon, lat, bs_yield)

raster_yields <- rasterFromXYZ(raster_yields)

rasterVis::levelplot(raster_yields, 
                     col.regions = rev(terrain.colors(10000)))

# now we can apply predictions by model specification element in parallel

saveRDS(bs_2015_tmp_pre_yld, here("processed", "bs_2015_tmp_pre_yld.RData"))


bs_2015_tmp_pre_yld <- readRDS(here("processed", "bs_2015_tmp_pre_yld.RData"))


# add country and adapt dummy -------------------------------------------------------------

worldmap <- rworldmap::getMap(resolution = "coarse")

worldmap_clean <- cleangeo::clgeo_Clean(worldmap) # some polygons are not closed, i.e. Canada and Western Sahara

r <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, crs=4326, res=0.5)

# convert coordinates to country  
  # The single argument to this function, points, is a data.frame in which:
  #   - column 1 contains the longitude in degrees
  #   - column 2 contains the latitude in degrees
  coords2country = function(points)
  {  
    countriesSP <- getMap(resolution='low')
    #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
    
    # convert our list of points to a SpatialPoints object
    
    # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
    
    #setting CRS directly to that from rworldmap
    pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
    
    
    # use 'over' to get indices of the Polygons object containing each point 
    indices = over(pointsSP, countriesSP)
    
    # return the ADMIN names of each country
    indices$ADMIN  
    #indices$ISO_A2 # returns the ISO3 code 
    #indices$continent   # returns the continent (6 continent model)
    #indices$REGION   # returns the continent (7 continent model)
  }
# create all other constant variables in the prediction data

points <- bs_2015_tmp_pre_yld[[1]] %>% 
    dplyr::select(lon, lat)

# this requires some dynamic running
coords_countries <- coords2country(points) # do this by setting indices$ADMIN in the function and commenting out other outputs
coords_countries <- cbind(points, coords_countries)

coords_ISO2 <- coords2country(points) # comment out indices$ADMIN and turn on indices$ISO_A2

coords_countries <- cbind(coords_countries, coords_ISO2) 

coords_countries <- coords_countries %>% rename(ADMIN = 3, ISO_A2 = 4)
  
bs_2015_vars <- lapply(1:4, function(i) {
            bs_2015_tmp_pre_yld[[i]] %>% 
            left_join(coords_countries, by=c("lon","lat")) %>%
            rename(Country2_fact = ISO_A2,
                   Country_name = ADMIN) %>%
            mutate(Country2_fact=na_if(Country2_fact, "-99"),
                   #Country_int=ifelse(Country_int=="<NA>",NA,Country_int),
                   #Country_name=ifelse(Country_name=="<NA>",NA,Country_name),
                   adapt_dummy=0,
                   adapt_dummy=as.factor(adapt_dummy),
                   C3=0,
                   C4=0) %>% 
            rename(Baseline_tmp_weighted = bs_gs_tmp,
                   Baseline_pre_weighted = bs_gs_pre,
                   Baseline_yield = bs_yield) %>% 
            as.data.table
            })

# change C4=1 for maize
bs_2015_vars[[1]] <- bs_2015_vars[[1]] %>% mutate(C4=1)

# change C3=1 for rice, soy, wheat
lapply(2:4, function(i){
  bs_2015_vars[[i]] <<- bs_2015_vars[[i]] %>% mutate(C3=1) # superassign to global environment
})

# all other vars like temp, precip, f_CO2, depend on KMNI RCP8.5 CMIP6 data on temp and precip

saveRDS(bs_2015_vars, here("processed", "bs_2015_vars.RData"))

bs_2015_vars <- readRDS(here("processed", "bs_2015_vars.RData"))
# do some checks that worldmap_clean ISO_A2 as factor produces same factor levels as coords_Countries

worldmap_clean@data %>% 
  dplyr::select(ADMIN, ISO_A2) %>% 
  mutate(Country2_fact = as.factor(ISO_A2)) %>% 
  mutate(Country2_fact=ifelse(Country2_fact==-99,NA,Country2_fact)) %>% 
  as_tibble() %>% 
  print(n=Inf) # checked - it's correct

# read in this data when it is time to merge with temp and precip variables

# rasterise bs gs temp (skip)-----------------------------------------

# calculate average baseline temperature over 2015-2020 tables in list and rasterise averaged result

bs_tmp_2015_2020 <- cbind(
  bs_years_tmp[[1]][5],
  bs_years_tmp[[2]][5],
  bs_years_tmp[[3]][5],
  bs_years_tmp[[4]][5],
  bs_years_tmp[[5]][5],
  bs_years_tmp[[6]][5]) %>% rowMeans() %>% 
  cbind(lon = bs_years_tmp[[1]][[1]], 
        lat = bs_years_tmp[[1]][[2]],
        crop_pooled = bs_years_tmp[[1]][[3]]) 

bs_tmp_2015_2020 <- as.data.frame(bs_tmp_2015_2020) %>% 
  rename(bs_years_tmp = 1) %>% 
  mutate(bs_years_tmp = as.numeric(bs_years_tmp))

# pivot wide by crop
bs_tmp_2015_2020_wide <- bs_tmp_2015_2020 %>% 
  pivot_wider(names_from = crop_pooled, values_from = bs_years_tmp)

bs_tmp_2015_2020_wide %>% readr::write_csv(here("processed", "bs_tmp_2015_2020_wide.csv"))

saveRDS(bs_tmp_2015_2020_wide, here("processed", "bs_tmp_2015_2020_wide.RData"))

# rasterise
bs_tmp_2015_2020_wide_raster <- rasterFromXYZ(bs_tmp_2015_2020_wide)

# save mean_baseline_tmps_2015_2020_grid_wide as a list with four (crop) elements
# we will want to join this to baseline precipitation by crop and lon/lat once the same calculations are repeated for bs gs precip in 2015-2020

# plot
rasterVis::levelplot(bs_tmp_2015_2020_wide_raster, main = "Average baseline growing-season temperature for 2015 to 2020")

bs_tmp_2015_2020_matrix <- bs_tmp_2015_2020_wide_raster[]

bs_tmp_2015_2020_df <- data.frame(Maize = bs_tmp_2015_2020_matrix[,1],
                                  Rice = bs_tmp_2015_2020_matrix[,2],
                                  Soybean = bs_tmp_2015_2020_matrix[,3],
                                  Wheat = bs_tmp_2015_2020_matrix[,4])

bs_tmp_2015_2020_df %>% readr::write_csv(here("processed", "bs_tmp_2015_2020_df.csv"))

saveRDS(bs_tmp_2015_2020_df, here("processed", "bs_tmp_2015_2020_df.RData"))
