
# Read in CMIP5 data - gridded temperature projections - RCP 8.5  ------------------------------------------------------

# Temperature projections for RCP 8.5


nc_tas_1861_2100_rcp85 <- nc_open(here("data", "CMIP5 data", "cmip5_tas_Amon_modmean_rcp85_1_mean_ave.nc"))

# 144 lon (row), 72 lat (col), 240 years (matrix slices)

# store values from variables and attributes
nc_tas_1861_2100_var_rcp85 <- ncvar_get(nc_tas_1861_2100_rcp85, attributes(nc_tas_1861_2100_rcp85$var)$names[1])

nc_tas_1861_2100_time_rcp85 <- ncvar_get(nc_tas_1861_2100_rcp85,attributes(nc_tas_1861_2100_rcp85$dim)$names[1]) # time, from 0-239
nc_tas_1861_2100_lon_rcp85 <- ncvar_get(nc_tas_1861_2100_rcp85,attributes(nc_tas_1861_2100_rcp85$dim)$names[2]) # lon seq by 2.5 degrees, 

# however this is from 0 360, need to adjust to -180 180

# nc_tas_1861_2100_lon_centred <- nc_tas_1861_2100_lon - 180

# nc_tas_1861_2100_lon_centred <- ifelse(nc_tas_1861_2100_lon > 180, # change back to > 180
#     nc_tas_1861_2100_lon - 360, # change back to -360
#    nc_tas_1861_2100_lon)


nc_tas_1861_2100_lat_rcp85 <- ncvar_get(nc_tas_1861_2100_rcp85,attributes(nc_tas_1861_2100_rcp85$dim)$names[3]) # lat seq by 2.5 degrees
nc_tas_1861_2100_atts_rcp85 <- ncatt_get(nc_tas_1861_2100_rcp85, 0)

# set the dimension names and values to the appropriate lon and lat values
dimnames(nc_tas_1861_2100_var_rcp85) <- list(lon = nc_tas_1861_2100_lon_rcp85, lat = nc_tas_1861_2100_lat_rcp85, time = nc_tas_1861_2100_time_rcp85)

# plot(raster(nc_tas_1861_2100_var[,,1]))


# close the connection
nc_close(nc_tas_1861_2100_rcp85)

# slice out historical baseline - 1861-1900 (40 years)

nc_tas_1861_1900_var_rcp85 <- nc_tas_1861_2100_var_rcp85[,,1:40] # note that this actually corresponds to time = 0 : time = 39 due to the way years are labelled starting from 1861 = 0th year; however no 0 index in a matrix array

# average over 1861-1900

avg_tas_1861_1900_var_rcp85 <- rowMeans(nc_tas_1861_1900_var_rcp85, dims = 2)

# slice out future scenario run

# since index for 1861 -> 1861-1861 -> 0; index for 2035 is 2035-1861 -> 174 and for 2065 is 2065-1861 -> 204; then shift forward by one such that index is 173:203

nc_tas_2035_2065_var_rcp85 <- nc_tas_1861_2100_var_rcp85[,,173:203]

# average over 2035-2065

avg_tas_2035_2065_var_rcp85 <- rowMeans(nc_tas_2035_2065_var_rcp85, dims = 2)

# calculate temperature change from future to historical

avg_tas_change_var_rcp85 <- avg_tas_2035_2065_var_rcp85 - avg_tas_1861_1900_var_rcp85


# Read in RCP 8.5 global mean warming as csv --------------------------------------------------------------------

# http://climexp.knmi.nl/CMIP5/Tglobal/index.cgi?email=someone@somewhere

global_tas_1860_2100_rcp85 <- readr::read_csv(here("data", "CMIP5 data", "global-mean-warming-mod-mean-tas-rcp85.csv"))

# note starts from 1860 not 1861; note monthly format, convert to dataframe
# first pivot long

global_tas_1860_2100_long_rcp85 <- global_tas_1860_2100_rcp85 %>% 
  rename(year = 1) %>% 
  pivot_longer(!year, names_to = "month", values_to = "tas") %>% 
  print(n=Inf)

# slice out 1861-1900

global_tas_1861_1900_rcp85 <- global_tas_1860_2100_long_rcp85 %>% 
  filter(year %in% c(1861:1900))

# average over 1861-1900
global_tas_1861_1900_avg_rcp85 <- global_tas_1861_1900_rcp85 %>% 
  summarise(mean = mean(tas)) # 287 rounded

# slice out 2035-2065

global_tas_2035_2065_rcp85 <- global_tas_1860_2100_long_rcp85 %>% 
  filter(year %in% c(2035:2065))

# average over 2035-2065

global_tas_2035_2065_avg_rcp85 <- global_tas_2035_2065_rcp85 %>% 
  summarise(mean = mean(tas)) # 289 rounded

# global temp change

global_tas_change_var_rcp85 <- global_tas_2035_2065_avg_rcp85$mean - global_tas_1861_1900_avg_rcp85$mean # 2.449769 for RCP8.5, 1.983262 for RCP4.5
#global_tas_change_var_c <- (global_tas_2035_2065_avg$mean -273.15) - (global_tas_1861_1900_avg$mean - 273.15) # same, given linear relationship



# Estimate local/global warming pattern scaling relationship --------------

scaled_tas_change_rcp85 <- avg_tas_change_var_rcp85 / global_tas_change_var_rcp85


scaled_tas_change_raster_rcp85 <- raster(scaled_tas_change_rcp85, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

flipped <- flip(scaled_tas_change_raster_rcp85, direction = 'x')
rotated <- t(flipped)
extent(rotated) <- c(0, 360, -90, 90)
res(rotated) <- c(2.5, 2.5)
scaled_tas_change_low_res_rcp85 <- rotate(rotated)

plot(scaled_tas_change_low_res_rcp85, col = topo.colors(n=10000))


# Interpolate and resample values to 0.5 x 0.5 degree resolution

scaled_tas_change_raster_disaggregate_rcp85 <- raster::disaggregate(scaled_tas_change_low_res_rcp85, fact = 5, method = 'bilinear')

res(scaled_tas_change_raster_disaggregate_rcp85)

plot(scaled_tas_change_raster_disaggregate_rcp85, col = topo.colors(n=10000))
plot(extendedworldmap, add = TRUE)



# convert back to df


scaled_tas_change_1dc_rcp85 <- scaled_tas_change_raster_disaggregate_rcp85 * 1
scaled_tas_change_2dc_rcp85 <- scaled_tas_change_raster_disaggregate_rcp85 * 2
scaled_tas_change_3dc_rcp85 <- scaled_tas_change_raster_disaggregate_rcp85 * 3
scaled_tas_change_4dc_rcp85 <- scaled_tas_change_raster_disaggregate_rcp85 * 4
scaled_tas_change_5dc_rcp85 <- scaled_tas_change_raster_disaggregate_rcp85 * 5

local_tas_change_1dc_rcp85 <- scaled_tas_change_1dc_rcp85[]
local_tas_change_2dc_rcp85 <- scaled_tas_change_2dc_rcp85[]
local_tas_change_3dc_rcp85 <- scaled_tas_change_3dc_rcp85[]
local_tas_change_4dc_rcp85 <- scaled_tas_change_4dc_rcp85[]
local_tas_change_5dc_rcp85 <- scaled_tas_change_5dc_rcp85[]


# Extrapolate local temperature change at global 1-3 degrees warming

# using scaled_tas_change_raster_disaggregate
col_heat_func <- colorRampPalette(c("white", "red"))


# to accurate represent the different scales across plots, use levelplot facet?
extendedworldmap <- worldmap

scaled_local_tas_stack_rcp85 <- stack(scaled_tas_change_1dc_rcp85, scaled_tas_change_2dc_rcp85, scaled_tas_change_3dc_rcp85)
names(scaled_local_tas_stack_rcp85) <- c("1 degree", '2 degrees', '3 degrees')

plot_local_warming_rcp85 <- rasterVis::levelplot(scaled_local_tas_stack_rcp85, 
                                                 col.regions = col_heat_func(100),
                                                 main = "Local warming at 1?C to 3?C global warming, RCP8.5",
                                                 names.attr = c("1?C", "2?C", "3?C"))

plot_local_warming_rcp85 

plot_local_warming_rcp85 + latticeExtra::layer(sp.lines(extendedworldmap)) # with boundaries


# Repeat for RCP 4.5 ------------------------------------------------------



nc_tas_1861_2100_rcp45 <- nc_open(here("data", "CMIP5 data", "cmip5_tas_Amon_modmean_rcp45_1_mean_ave.nc"))

# 144 lon (row), 72 lat (col), 240 years (matrix slices)

# store values from variables and attributes
nc_tas_1861_2100_var_rcp45 <- ncvar_get(nc_tas_1861_2100_rcp45, attributes(nc_tas_1861_2100_rcp45$var)$names[1])

nc_tas_1861_2100_time_rcp45 <- ncvar_get(nc_tas_1861_2100_rcp45,attributes(nc_tas_1861_2100_rcp45$dim)$names[1]) # time, from 0-239
nc_tas_1861_2100_lon_rcp45 <- ncvar_get(nc_tas_1861_2100_rcp45,attributes(nc_tas_1861_2100_rcp45$dim)$names[2]) # lon seq by 2.5 degrees, 

# however this is from 0 360, need to adjust to -180 180

# nc_tas_1861_2100_lon_centred <- nc_tas_1861_2100_lon - 180

# nc_tas_1861_2100_lon_centred <- ifelse(nc_tas_1861_2100_lon > 180, # change back to > 180
#     nc_tas_1861_2100_lon - 360, # change back to -360
#    nc_tas_1861_2100_lon)


nc_tas_1861_2100_lat_rcp45 <- ncvar_get(nc_tas_1861_2100_rcp45,attributes(nc_tas_1861_2100_rcp45$dim)$names[3]) # lat seq by 2.5 degrees
nc_tas_1861_2100_atts_rcp45 <- ncatt_get(nc_tas_1861_2100_rcp45, 0)

# set the dimension names and values to the appropriate lon and lat values
dimnames(nc_tas_1861_2100_var_rcp45) <- list(lon = nc_tas_1861_2100_lon_rcp45, lat = nc_tas_1861_2100_lat_rcp45, time = nc_tas_1861_2100_time_rcp45)

# plot(raster(nc_tas_1861_2100_var[,,1]))


# close the connection
nc_close(nc_tas_1861_2100_rcp45)

# slice out historical baseline - 1861-1900 (40 years)

nc_tas_1861_1900_var_rcp45 <- nc_tas_1861_2100_var_rcp45[,,1:40] # note that this actually corresponds to time = 0 : time = 39 due to the way years are labelled starting from 1861 = 0th year; however no 0 index in a matrix array

# average over 1861-1900

avg_tas_1861_1900_var_rcp45 <- rowMeans(nc_tas_1861_1900_var_rcp45, dims = 2)

# slice out future scenario run

# since index for 1861 -> 1861-1861 -> 0; index for 2035 is 2035-1861 -> 174 and for 2065 is 2065-1861 -> 204; then shift forward by one such that index is 173:203

nc_tas_2035_2065_var_rcp45 <- nc_tas_1861_2100_var_rcp45[,,173:203]

# average over 2035-2065

avg_tas_2035_2065_var_rcp45 <- rowMeans(nc_tas_2035_2065_var_rcp45, dims = 2)

# calculate temperature change from future to historical

avg_tas_change_var_rcp45 <- avg_tas_2035_2065_var_rcp45 - avg_tas_1861_1900_var_rcp45

# Load RCP 4.5 global mean warming

# http://climexp.knmi.nl/CMIP5/Tglobal/index.cgi?email=someone@somewhere

global_tas_1860_2100_rcp45 <- readr::read_csv(here("data", "CMIP5 data", "global-mean-warming-mod-mean-tas-rcp45.csv"))

# note starts from 1860 not 1861; note monthly format, convert to dataframe
# first pivot long

global_tas_1860_2100_long_rcp45 <- global_tas_1860_2100_rcp45 %>% 
  rename(year = 1) %>% 
  pivot_longer(!year, names_to = "month", values_to = "tas") %>% 
  print(n=Inf)

# slice out 1861-1900

global_tas_1861_1900_rcp45 <- global_tas_1860_2100_long_rcp45 %>% 
  filter(year %in% c(1861:1900))

# average over 1861-1900
global_tas_1861_1900_avg_rcp45 <- global_tas_1861_1900_rcp45 %>% 
  summarise(mean = mean(tas)) # 287 rounded

# slice out 2035-2065

global_tas_2035_2065_rcp45 <- global_tas_1860_2100_long_rcp45 %>% 
  filter(year %in% c(2035:2065))

# average over 2035-2065

global_tas_2035_2065_avg_rcp45 <- global_tas_2035_2065_rcp45 %>% 
  summarise(mean = mean(tas)) # 289 rounded

# global temp change

global_tas_change_var_rcp45 <- global_tas_2035_2065_avg_rcp45$mean - global_tas_1861_1900_avg_rcp45$mean # 2.449769 for RCP8.5, 1.983262 for RCP4.5
#global_tas_change_var_c <- (global_tas_2035_2065_avg$mean -273.15) - (global_tas_1861_1900_avg$mean - 273.15) # same, given linear relationship


# Estimate local/global warming pattern scaling relationship

scaled_tas_change_rcp45 <- avg_tas_change_var_rcp45 / global_tas_change_var_rcp45


# plot
# rasterise scaled_tas_change attribute values on grid cell
#m <- t(scaled_tas_change)
#raster <- raster(m[nrow(m):1,])
#extent(raster) <- c(0, 360, -90, 90) 
#plot(raster)
#plot(rotate(raster)) 

# rasterlayer is organized by row, 
# i.e. left-right, top-bottom, while a matrix is organized by column, i.e. top-bottom, left-right. 

# equivalent code:

scaled_tas_change_raster_rcp45 <- raster(scaled_tas_change_rcp45, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

flipped <- flip(scaled_tas_change_raster_rcp45, direction = 'x')
rotated <- t(flipped)
extent(rotated) <- c(0, 360, -90, 90)
res(rotated) <- c(2.5, 2.5)
scaled_tas_change_low_res_rcp45 <- rotate(rotated)

plot(scaled_tas_change_low_res_rcp45, col = topo.colors(n=10000))



# repeat for RCP 4.5 using same code, just swap out the filename


# Interpolate and resample values to 0.5 x 0.5 degree resolution

scaled_tas_change_raster_disaggregate_rcp45 <- raster::disaggregate(scaled_tas_change_low_res_rcp45, fact = 5, method = 'bilinear')

res(scaled_tas_change_raster_disaggregate_rcp45)

plot(scaled_tas_change_raster_disaggregate_rcp45, col = topo.colors(n=10000))
plot(extendedworldmap, add = TRUE)



# convert back to df 


scaled_tas_change_1dc_rcp45 <- scaled_tas_change_raster_disaggregate_rcp45 * 1
scaled_tas_change_2dc_rcp45 <- scaled_tas_change_raster_disaggregate_rcp45 * 2
scaled_tas_change_3dc_rcp45 <- scaled_tas_change_raster_disaggregate_rcp45 * 3
scaled_tas_change_4dc_rcp45 <- scaled_tas_change_raster_disaggregate_rcp45 * 4
scaled_tas_change_5dc_rcp45 <- scaled_tas_change_raster_disaggregate_rcp45 * 5


local_tas_change_1dc_rcp45 <- scaled_tas_change_1dc_rcp45[]

local_tas_change_2dc_rcp45 <- scaled_tas_change_2dc_rcp45[]

local_tas_change_3dc_rcp45 <- scaled_tas_change_3dc_rcp45[]
local_tas_change_4dc_rcp45 <- scaled_tas_change_4dc_rcp45[]
local_tas_change_5dc_rcp45 <- scaled_tas_change_5dc_rcp45[]

# Extrapolate local temperature change at global 1-3 degrees warming

# using scaled_tas_change_raster_disaggregate
col_heat_func <- colorRampPalette(c("white", "red"))


# to accurate represent the different scales across plots, use levelplot facet?
extendedworldmap <- worldmap

scaled_local_tas_stack_rcp45 <- stack(scaled_tas_change_1dc_rcp45, scaled_tas_change_2dc_rcp45, scaled_tas_change_3dc_rcp45)
names(scaled_local_tas_stack_rcp45) <- c("1 degree", '2 degrees', '3 degrees')

plot_local_warming_rcp45 <- rasterVis::levelplot(scaled_local_tas_stack_rcp45, 
                                                 col.regions = col_heat_func(100),
                                                 main = "Local warming at 1?C to 3?C global warming, RCP4.5",
                                                 names.attr = c("1?C", "2?C", "3?C"))

plot_local_warming_rcp45 

plot_local_warming_rcp45 + latticeExtra::layer(sp.lines(extendedworldmap)) # with boundaries

# Estimate RCP 8.5 CO2 levels ------------------------------------------------------


# quadratic function

# use RCP85_CO2 and RCP85 temp for 1860:2100

# first take annual mean of monthly temp data, unfortunately
global_tas_1860_2100_month_rcp85 <- global_tas_1860_2100_long_rcp85 %>% 
  group_by(year) %>% 
  summarise(mean_tmp = mean(tas))

nc_CO2_rcp85 <- nc_open(here("data", "CMIP5 data", "iRCP85_CO2_1860_2100.nc"))

# only one dimension

# store values from variables and attributes
nc_CO2_var_rcp85 <- ncvar_get(nc_CO2_rcp85, attributes(nc_CO2_rcp85$var)$names[1])

nc_CO2_time_rcp85 <- ncvar_get(nc_CO2_rcp85,attributes(nc_CO2_rcp85$dim)$names[1]) # time, from 0-239

global_CO2_rcp85 <- data.frame(index = nc_CO2_time_rcp85, year = c(1860:2100), global_CO2_ppm = nc_CO2_var_rcp85, global_tmp_K = global_tas_1860_2100_month_rcp85$mean_tmp, global_tmp_C = global_tas_1860_2100_month_rcp85$mean_tmp - 273.15)

dim(global_CO2_rcp85) # 241   5

# fit quadratic model with intercept, linear term and quadratic term

tmp_CO2_fit_rcp85 <- lm(global_CO2_ppm ~ global_tmp_K + I(global_tmp_K^2), data = global_CO2_rcp85)

summary(tmp_CO2_fit_rcp85) # R squared and adjusted R squared of 99.81%, since we have 238 DF and Moore et al had 98 degrees of freedom they likely only used 100 years eg 1860-1960 or 1900-2000 rather than to 2100.

tmp_C_CO2_fit_rcp85 <- lm(global_CO2_ppm ~ global_tmp_C + I(global_tmp_C^2), data = global_CO2_rcp85)

summary(tmp_C_CO2_fit_rcp85)

# predict CO2 at temp change of 1-3 degrees above average temperature at 1995-2005

mean_tmp_changes_rcp85 <- global_CO2_rcp85 %>% 
  filter(year %in% c(1995:2005)) %>% 
  summarise(mean_tmp = mean(global_tmp_C)) %>%  # 14.32234
  mutate(mean_tmp_plus_1 = mean_tmp + 1,
         mean_tmp_plus_2 = mean_tmp + 2,
         mean_tmp_plus_3 = mean_tmp + 3,
         mean_tmp_plus_4 = mean_tmp + 4,
         mean_tmp_plus_5 = mean_tmp + 5)


mean_tmp_changes_long_rcp85 <-  pivot_longer(mean_tmp_changes_rcp85, 
                                             cols = mean_tmp:mean_tmp_plus_5,
                                             names_to = "change", 
                                             values_to = "global_tmp_C") 

predicted_CO2_rcp85 <- predict(tmp_C_CO2_fit_rcp85, as.data.frame(mean_tmp_changes_long_rcp85))


# Estimate RCP 4.5 CO2 levels ------------------------------------------------------

global_tas_1860_2100_month_rcp45 <- global_tas_1860_2100_long_rcp45 %>% 
  group_by(year) %>% 
  summarise(mean_tmp = mean(tas))

nc_CO2_rcp45 <- nc_open(here("data", "CMIP5 data", "iRCP45_CO2_1860_2100.nc"))

# only one dimension

# store values from variables and attributes
nc_CO2_var_rcp45 <- ncvar_get(nc_CO2_rcp45, attributes(nc_CO2_rcp45$var)$names[1])

nc_CO2_time_rcp45 <- ncvar_get(nc_CO2_rcp45,attributes(nc_CO2_rcp45$dim)$names[1]) # time, from 0-239

global_CO2_rcp45 <- data.frame(index = nc_CO2_time_rcp45, year = c(1860:2100), global_CO2_ppm = nc_CO2_var_rcp45, global_tmp_K = global_tas_1860_2100_month_rcp45$mean_tmp, global_tmp_C = global_tas_1860_2100_month_rcp45$mean_tmp - 273.15)

dim(global_CO2_rcp45) # 241   5

# fit quadratic model with intercept, linear term and quadratic term

tmp_CO2_fit_rcp45 <- lm(global_CO2_ppm ~ global_tmp_K + I(global_tmp_K^2), data = global_CO2_rcp45)

summary(tmp_CO2_fit_rcp85) # R squared and adjusted R squared of 99.81%, since we have 238 DF and Moore et al had 98 degrees of freedom they likely only used 100 years eg 1860-1960 or 1900-2000 rather than to 2100.

tmp_C_CO2_fit_rcp45 <- lm(global_CO2_ppm ~ global_tmp_C + I(global_tmp_C^2), data = global_CO2_rcp45)

summary(tmp_C_CO2_fit_rcp45)

# predict CO2 at temp change of 1-3 degrees above average temperature at 1995-2005

mean_tmp_changes_rcp45 <- global_CO2_rcp45 %>% 
  filter(year %in% c(1995:2005)) %>% 
  summarise(mean_tmp = mean(global_tmp_C)) %>%  # 14.32234
  mutate(mean_tmp_plus_1 = mean_tmp + 1,
         mean_tmp_plus_2 = mean_tmp + 2,
         mean_tmp_plus_3 = mean_tmp + 3,
         mean_tmp_plus_4 = mean_tmp + 4,
         mean_tmp_plus_5 = mean_tmp + 5)


mean_tmp_changes_long_rcp45 <-  pivot_longer(mean_tmp_changes_rcp45, 
                                             cols = mean_tmp:mean_tmp_plus_5,
                                             names_to = "change", 
                                             values_to = "global_tmp_C") 

predicted_CO2_rcp45 <- predict(tmp_C_CO2_fit_rcp45, as.data.frame(mean_tmp_changes_long_rcp45))  

# Estimate Baseline growing season temperature at 2015-2020-------------------------------------


# first run script 4, line 10-151


crop_season_extended <- data.frame()

for (i in 1:15){
  matrix <- raster_extended_crop_season[[i]][] # turn raster of full 259200 grid cells for 15 crop varieties into matrix
  matrix[,1] <- unique(crop_season$crop)[i] # crop name
  matrix <- as.data.frame(matrix)
  matrix$lon <- crop_calendar$lon[1:259200] # full list replicated x 7 crops
  matrix$lat <- crop_calendar$lat[1:259200]
  crop_season_extended <<- rbind(crop_season_extended, matrix)
}

# crop_season_extended is then the extended dataframe of crop_season and should have 15 x 259200 rows, unlike crop_season (recall MIRCA crops had incomplete grid cells where no calendar values were available)
dim(crop_season_extended) # 3888000 18

cols <- c("n_growing_months", "jan_growing", "feb_growing", "mar_growing", "apr_growing", "may_growing", "jun_growing", "jul_growing", "aug_growing", "sept_growing", "oct_growing", "nov_growing", "dec_growing")
crop_season_extended_numeric <- crop_season_extended # note copy, remove from environment
crop_season_extended_numeric[cols] <- sapply(crop_season_extended_numeric[cols], as.numeric)
rm(crop_season_extended)

# create tmp_x_wide x 11 times - and calculate annual average temperatures one at a time, remove when done for memory management
# for each dataframe, mutate new variable with major crop variety level, group_by major crop level and take average baseline temperatures over the sub-crop-variety temperatures
# however, need lon lat points in crop_season_extended

# only keep 9 crops in crop_season_extended_numeric

unique(crop_season_extended_numeric$crop)

crop_season_extended_subset <- crop_season_extended_numeric %>% 
  filter(crop %in% c("Rice", "Rice (Rainfed)", "Rice (Irrigated)", "Wheat", "Wheat.Winter", "Wheat (Rainfed)", "Wheat (Irrigated)", "Maize", "Soybean"))


tmps_2011_2020 <- nc_open(here("data", "CRU data", "cru_ts4.05.2011.2020.tmp.dat.nc"))

# store values from variables and attributes
nc_tmp_2011_2020 <- ncvar_get(tmps_2011_2020, attributes(tmps_2011_2020$var)$names[1])
nc_tmps_lon_2011_2020 <- ncvar_get(tmps_2011_2020,attributes(tmps_2011_2020$dim)$names[1])
nc_tmps_lat_2011_2020 <- ncvar_get(tmps_2011_2020, attributes(tmps_2011_2020$dim)$names[2])
nc_tmps_time_2011_2020 <- ncvar_get(tmps_2011_2020, attributes(tmps_2011_2020$dim)$names[3]) # 120 months
nc_tmps_atts_2011_2020 <- ncatt_get(tmps_2011_2020, 0)

# set dimension names and values to corresponding lon and lat values
dimnames(nc_tmp_2011_2020) <- list(lon = nc_tmps_lon_2011_2020, lat = nc_tmps_lat_2011_2020, time = nc_tmps_time_2011_2020)


tmp_2011_2020_df <- reshape2::melt(nc_tmp_2011_2020, value.name = "tmp") 
tmp_2011_2020_df$date <- as.Date(tmp_2011_2020_df$time, origin = "1900-1-1")
tmp_2011_2020_df <- tmp_2011_2020_df %>% dplyr::select(!time)


generate_baseline_tmp_2011_2020_annual <- function(YEAR.START, YEAR.END) {
  
  TMP_YEAR <- tmp_2011_2020_df %>% # CHANGE THIS
    dplyr::filter(date >= as.Date(YEAR.START) & date <= as.Date(YEAR.END)) # this changes dynamically
  
  # need to reshape wide by month
  TMP_YEAR_WIDE <- tidyr::spread(TMP_YEAR, date, tmp)
  except <- c("lon", "lat")
  colnames(TMP_YEAR_WIDE) <- paste("tmp", colnames(TMP_YEAR_WIDE), sep = "_")
  TMP_YEAR_WIDE <- TMP_YEAR_WIDE %>% dplyr::rename(lon = tmp_lon, lat = tmp_lat)
  
  # merge by lon lat
  CROP_TMP_YEAR <- merge(crop_season_extended_subset, TMP_YEAR_WIDE, 
                         by = c("lon", "lat"),
                         all.x = T)
  
  # convert tmp vars to months (rename colnames) (jan_tmp, ... , dec_tmp)
  
  CROP_TMP_YEAR <- CROP_TMP_YEAR %>% # make sure column indices are correctly ordered first
    rename(jan_tmp = 19,
           feb_tmp = 20,
           mar_tmp = 21,
           apr_tmp = 22,
           may_tmp = 23,
           jun_tmp = 24,
           jul_tmp = 25,
           aug_tmp = 26,
           sept_tmp = 27,
           oct_tmp = 28,
           nov_tmp = 29,
           dec_tmp = 30)
  
  # created weighted average
  CROP_TMP_YEAR <- CROP_TMP_YEAR %>% 
    mutate(avg_growing_tmp = (jan_tmp*jan_growing + 
                                feb_tmp*feb_growing +
                                mar_tmp*mar_growing +
                                apr_tmp*apr_growing +
                                may_tmp*may_growing +
                                jun_tmp*jun_growing +
                                jul_tmp*jul_growing +
                                aug_tmp*aug_growing +
                                sept_tmp*sept_growing +
                                oct_tmp*oct_growing +
                                nov_tmp*nov_growing +
                                dec_tmp*dec_growing)/n_growing_months)
  
  AVG_TMP_YEAR <- CROP_TMP_YEAR %>% 
    dplyr::select(lon, lat, crop, avg_growing_tmp) %>% 
    mutate(year = year(YEAR.START)) %>%  
    
    # now average over sub-crop variety baseline temperatures
    # for each dataframe, mutate new variable with major crop variety level, group_by major crop level and take average baseline temperatures over the sub-crop-variety temperatures
    mutate(crop_pooled = case_when(crop %in% c("Wheat", "Wheat.Winter", "Wheat (Rainfed)", "Wheat (Irrigated)") ~ "Wheat",
                                   crop %in% c("Rice", "Rice (Irrigated)", "Rice (Rainfed)") ~ "Rice",
                                   crop == "Maize" ~ "Maize",
                                   crop == "Soybean" ~ "Soybean")) %>%
    group_by(lon, lat, crop_pooled, year) %>% 
    summarise(mean_avg_growing_tmp = mean(avg_growing_tmp, na.rm = TRUE))
  
  
  
}

# generate_baseline_tmp_1995_2005_annual(YEAR.START = "2000-01-16", YEAR.END = "2000-12-16")

df_2015_2020 = data.frame(YEAR.START = c("2015-01-16", "2016-01-16", "2017-01-16", "2018-01-16", "2019-01-16", "2020-01-16"), 
                          YEAR.END = c("2015-12-16", "2016-12-16", "2017-12-16", "2018-12-16", "2019-12-16", "2020-12-16"))

# do.call(function(x,z,...) generate_baseline_tmp_1995_2005_annual(x,z), df)

avg_baseline_tmps_2015_2020 <- apply(df_2015_2020[,c("YEAR.START", "YEAR.END")], 1, function(x) generate_baseline_tmp_2011_2020_annual(x[1], x[2]))

write_to_csv <- function(i){
  
  avg_baseline_tmps_2015_2020[[i]] %>% 
    readr::write_csv(here("processed", paste0("avg_baseline_tmps_2015_2020_", i, ".csv")))
}

lapply(1:6, write_to_csv)


# if running from here, read in avg_baseline_tmps_2015_2020 -------------------------------------

# read_data_csv <- function(i,j){

avg_baseline_tmps_2015_2020 <- lapply(1:6, function(i) {
  
  readr::read_csv(here("processed", paste0("avg_baseline_tmps_2015_2020_", i, ".csv")),
                  col_types = readr::cols(.default = "c"))
  
})


refactorise_baseline_data <- function(i){
  
  avg_baseline_tmps_2015_2020[[i]][,c(1,2,5)] <- lapply(avg_baseline_tmps_2015_2020[[i]][,c(1,2,5)], as.numeric)
  
  avg_baseline_tmps_2015_2020[[i]][,c(4)] <- lapply(avg_baseline_tmps_2015_2020[[i]][,c(4)], factor)
  
  
  avg_baseline_tmps_2015_2020[[i]]
  
}

avg_baseline_tmps_2015_2020 <- lapply(1:6, refactorise_baseline_data)

# calculate average annual baseline temperature across 1995-2000 tables in list, cbindlist? avg_baseline_tmps_1995_2000[[1]][[5]]

mean_baseline_tmps_2015_2020_grid <- cbind(
  avg_baseline_tmps_2015_2020[[1]][5],
  avg_baseline_tmps_2015_2020[[2]][5],
  avg_baseline_tmps_2015_2020[[3]][5],
  avg_baseline_tmps_2015_2020[[4]][5],
  avg_baseline_tmps_2015_2020[[5]][5],
  avg_baseline_tmps_2015_2020[[6]][5]) %>% rowMeans() %>% 
  cbind(lon = avg_baseline_tmps_2015_2020[[1]][[1]], 
        lat = avg_baseline_tmps_2015_2020[[1]][[2]],
        crop_pooled = avg_baseline_tmps_2015_2020[[1]][[3]]) 

mean_baseline_tmps_2015_2020_grid <- as.data.frame(mean_baseline_tmps_2015_2020_grid) %>% 
  rename(avg_baseline_tmp_2015_2020 = 1) %>% 
  mutate(avg_baseline_tmp_2015_2020 = as.numeric(avg_baseline_tmp_2015_2020))


rm(avg_baseline_tmps_2015_2020) # 705Mb!

# pivot wide by crop
mean_baseline_tmps_2015_2020_grid_wide <- mean_baseline_tmps_2015_2020_grid %>% 
  pivot_wider(names_from = crop_pooled, values_from = avg_baseline_tmp_2015_2020)

mean_baseline_tmps_2015_2020_grid_wide %>% readr::write_csv(here("processed", "mean_baseline_tmps_2015_2020_grid_wide.csv"))

# rasterise
baseline_tmps_2015_2020_grid_raster <- rasterFromXYZ(mean_baseline_tmps_2015_2020_grid_wide)


# plot
rasterVis::levelplot(baseline_tmps_2015_2020_grid_raster, main = "Average baseline growing-season temperature for 2015 to 2020")

baseline_tmp_grid_matrix_2015_2020 <- baseline_tmps_2015_2020_grid_raster[]

baseline_tmp_grid_df_2015_2020 <- data.frame(Maize = baseline_tmp_grid_matrix_2015_2020[,1],
                                             Rice = baseline_tmp_grid_matrix_2015_2020[,2],
                                             Soybean = baseline_tmp_grid_matrix_2015_2020[,3],
                                             Wheat = baseline_tmp_grid_matrix_2015_2020[,4])

baseline_tmp_grid_df_2015_2020 %>% readr::write_csv(here("processed", "baseline_tmp_grid_df_2015_2020.csv"))


# Repredict CO2 for baseline temperature at 2015-2020 ----------------------------------------------------------------

mean_tmp_changes_rcp85_2015_2020 <- global_CO2_rcp85 %>% 
  filter(year %in% c(2015:2020)) %>% 
  summarise(mean_tmp = mean(global_tmp_C)) %>%  # 14.32234
  mutate(mean_tmp_plus_1 = mean_tmp + 1,
         mean_tmp_plus_2 = mean_tmp + 2,
         mean_tmp_plus_3 = mean_tmp + 3,
         mean_tmp_plus_4 = mean_tmp + 4,
         mean_tmp_plus_5 = mean_tmp + 5)


mean_tmp_changes_long_rcp85_2015_2020 <-  pivot_longer(mean_tmp_changes_rcp85_2015_2020, 
                                                       cols = mean_tmp:mean_tmp_plus_5,
                                                       names_to = "change", 
                                                       values_to = "global_tmp_C") 

predicted_CO2_rcp85_2015_2020 <- predict(tmp_C_CO2_fit_rcp85, as.data.frame(mean_tmp_changes_long_rcp85_2015_2020))

mean_tmp_changes_rcp45_2015_2020 <- global_CO2_rcp45 %>% 
  filter(year %in% c(2015:2020)) %>% 
  summarise(mean_tmp = mean(global_tmp_C)) %>%  # 14.32234
  mutate(mean_tmp_plus_1 = mean_tmp + 1,
         mean_tmp_plus_2 = mean_tmp + 2,
         mean_tmp_plus_3 = mean_tmp + 3,
         mean_tmp_plus_4 = mean_tmp + 4,
         mean_tmp_plus_5 = mean_tmp + 5)


mean_tmp_changes_long_rcp45_2015_2020 <-  pivot_longer(mean_tmp_changes_rcp45_2015_2020, 
                                                       cols = mean_tmp:mean_tmp_plus_5,
                                                       names_to = "change", 
                                                       values_to = "global_tmp_C") 

predicted_CO2_rcp45_2015_2020 <- predict(tmp_C_CO2_fit_rcp45, as.data.frame(mean_tmp_changes_long_rcp45_2015_2020))  

# CO2 levels
predicted_CO2_2015_2020_df <- data.frame(CO2_0dc_rcp85 = predicted_CO2_rcp85_2015_2020[1],
                                         CO2_1dc_rcp85 = predicted_CO2_rcp85_2015_2020[2],
                                         CO2_2dc_rcp85 = predicted_CO2_rcp85_2015_2020[3],
                                         CO2_3dc_rcp85 = predicted_CO2_rcp85_2015_2020[4],
                                         CO2_4dc_rcp85 = predicted_CO2_rcp85_2015_2020[5],
                                         CO2_5dc_rcp85 = predicted_CO2_rcp85_2015_2020[6],
                                         CO2_0dc_rcp45 = predicted_CO2_rcp45_2015_2020[1],
                                         CO2_1dc_rcp45 = predicted_CO2_rcp45_2015_2020[2],
                                         CO2_2dc_rcp45 = predicted_CO2_rcp45_2015_2020[3],
                                         CO2_3dc_rcp45 = predicted_CO2_rcp45_2015_2020[4],
                                         CO2_4dc_rcp45 = predicted_CO2_rcp45_2015_2020[5],
                                         CO2_5dc_rcp45 = predicted_CO2_rcp45_2015_2020[6])



predicted_CO2_df_rep_2015_2020 <- do.call(rbind, replicate(259200, predicted_CO2_2015_2020_df, simplify = FALSE))

dim(predicted_CO2_df_rep_2015_2020) # 259200      12

# change to fCO2_C3 and fCO2_C4
predicted_f_CO2_2015_2020 <- predicted_CO2_df_rep_2015_2020 %>% 
  mutate(f_CO2_1dc_C3_rcp85 = (CO2_1dc_rcp85-CO2_0dc_rcp85)/(CO2_1dc_rcp85-CO2_0dc_rcp85 + 100),
         f_CO2_2dc_C3_rcp85 = (CO2_2dc_rcp85-CO2_0dc_rcp85)/(CO2_2dc_rcp85-CO2_0dc_rcp85 + 100),
         f_CO2_3dc_C3_rcp85 = (CO2_3dc_rcp85-CO2_0dc_rcp85)/(CO2_3dc_rcp85-CO2_0dc_rcp85 + 100),
         f_CO2_4dc_C3_rcp85 = (CO2_4dc_rcp85-CO2_0dc_rcp85)/(CO2_4dc_rcp85-CO2_0dc_rcp85 + 100),
         f_CO2_5dc_C3_rcp85 = (CO2_5dc_rcp85-CO2_0dc_rcp85)/(CO2_5dc_rcp85-CO2_0dc_rcp85 + 100),
         f_CO2_1dc_C4_rcp85 = (CO2_1dc_rcp85-CO2_0dc_rcp85)/(CO2_1dc_rcp85-CO2_0dc_rcp85 + 50),
         f_CO2_2dc_C4_rcp85 = (CO2_2dc_rcp85-CO2_0dc_rcp85)/(CO2_2dc_rcp85-CO2_0dc_rcp85 + 50),
         f_CO2_3dc_C4_rcp85 = (CO2_3dc_rcp85-CO2_0dc_rcp85)/(CO2_3dc_rcp85-CO2_0dc_rcp85 + 50),
         f_CO2_4dc_C4_rcp85 = (CO2_4dc_rcp85-CO2_0dc_rcp85)/(CO2_4dc_rcp85-CO2_0dc_rcp85 + 50),
         f_CO2_5dc_C4_rcp85 = (CO2_5dc_rcp85-CO2_0dc_rcp85)/(CO2_5dc_rcp85-CO2_0dc_rcp85 + 50),
         f_CO2_1dc_C3_rcp45 = (CO2_1dc_rcp45-CO2_0dc_rcp45)/(CO2_1dc_rcp45-CO2_0dc_rcp45 + 100),
         f_CO2_2dc_C3_rcp45 = (CO2_2dc_rcp45-CO2_0dc_rcp45)/(CO2_2dc_rcp45-CO2_0dc_rcp45 + 100),
         f_CO2_3dc_C3_rcp45 = (CO2_3dc_rcp45-CO2_0dc_rcp45)/(CO2_3dc_rcp45-CO2_0dc_rcp45 + 100),
         f_CO2_4dc_C3_rcp45 = (CO2_4dc_rcp45-CO2_0dc_rcp45)/(CO2_4dc_rcp45-CO2_0dc_rcp45 + 100),
         f_CO2_5dc_C3_rcp45 = (CO2_5dc_rcp45-CO2_0dc_rcp45)/(CO2_5dc_rcp45-CO2_0dc_rcp45 + 100),
         f_CO2_1dc_C4_rcp45 = (CO2_1dc_rcp45-CO2_0dc_rcp45)/(CO2_1dc_rcp45-CO2_0dc_rcp45 + 50),
         f_CO2_2dc_C4_rcp45 = (CO2_2dc_rcp45-CO2_0dc_rcp45)/(CO2_2dc_rcp45-CO2_0dc_rcp45 + 50),
         f_CO2_3dc_C4_rcp45 = (CO2_3dc_rcp45-CO2_0dc_rcp45)/(CO2_3dc_rcp45-CO2_0dc_rcp45 + 50),
         f_CO2_4dc_C4_rcp45 = (CO2_4dc_rcp45-CO2_0dc_rcp45)/(CO2_4dc_rcp45-CO2_0dc_rcp45 + 50),
         f_CO2_5dc_C4_rcp45 = (CO2_5dc_rcp45-CO2_0dc_rcp45)/(CO2_5dc_rcp45-CO2_0dc_rcp45 + 50))

f_CO2_2015_2020 <- predicted_f_CO2_2015_2020[13:32]


# Include 0 degrees temperature change ------------------------------------

scaled_tas_change_0dc_rcp85 <- scaled_tas_change_raster_disaggregate_rcp85 * 0
local_tas_change_0dc_rcp85 <- scaled_tas_change_0dc_rcp85[]
scaled_tas_change_0dc_rcp45 <- scaled_tas_change_raster_disaggregate_rcp45 * 0
local_tas_change_0dc_rcp45 <- scaled_tas_change_0dc_rcp45[]

local_tmp_changes_zero <- data.frame(rcp85_0dc = local_tas_change_0dc_rcp85,
                                     rcp85_1dc = local_tas_change_1dc_rcp85,
                                     rcp85_2dc = local_tas_change_2dc_rcp85,
                                     rcp85_3dc = local_tas_change_3dc_rcp85,
                                     rcp85_4dc = local_tas_change_4dc_rcp85,
                                     rcp85_5dc = local_tas_change_5dc_rcp85,
                                     rcp45_0dc = local_tas_change_0dc_rcp45,
                                     rcp45_1dc = local_tas_change_1dc_rcp45,
                                     rcp45_2dc = local_tas_change_2dc_rcp45,
                                     rcp45_3dc = local_tas_change_3dc_rcp45,
                                     rcp45_4dc = local_tas_change_4dc_rcp45,
                                     rcp45_5dc = local_tas_change_5dc_rcp45)


predicted_f_CO2_2015_2020_zero <- predicted_CO2_df_rep_2015_2020 %>% 
  mutate(f_CO2_0dc_C3_rcp85 = (CO2_0dc_rcp85-CO2_0dc_rcp85)/(CO2_0dc_rcp85-CO2_0dc_rcp85 + 100),
         f_CO2_1dc_C3_rcp85 = (CO2_1dc_rcp85-CO2_0dc_rcp85)/(CO2_1dc_rcp85-CO2_0dc_rcp85 + 100),
         f_CO2_2dc_C3_rcp85 = (CO2_2dc_rcp85-CO2_0dc_rcp85)/(CO2_2dc_rcp85-CO2_0dc_rcp85 + 100),
         f_CO2_3dc_C3_rcp85 = (CO2_3dc_rcp85-CO2_0dc_rcp85)/(CO2_3dc_rcp85-CO2_0dc_rcp85 + 100),
         f_CO2_4dc_C3_rcp85 = (CO2_4dc_rcp85-CO2_0dc_rcp85)/(CO2_4dc_rcp85-CO2_0dc_rcp85 + 100),
         f_CO2_5dc_C3_rcp85 = (CO2_5dc_rcp85-CO2_0dc_rcp85)/(CO2_5dc_rcp85-CO2_0dc_rcp85 + 100),
         f_CO2_0dc_C4_rcp85 = (CO2_0dc_rcp85-CO2_0dc_rcp85)/(CO2_0dc_rcp85-CO2_0dc_rcp85 + 50),
         f_CO2_1dc_C4_rcp85 = (CO2_1dc_rcp85-CO2_0dc_rcp85)/(CO2_1dc_rcp85-CO2_0dc_rcp85 + 50),
         f_CO2_2dc_C4_rcp85 = (CO2_2dc_rcp85-CO2_0dc_rcp85)/(CO2_2dc_rcp85-CO2_0dc_rcp85 + 50),
         f_CO2_3dc_C4_rcp85 = (CO2_3dc_rcp85-CO2_0dc_rcp85)/(CO2_3dc_rcp85-CO2_0dc_rcp85 + 50),
         f_CO2_4dc_C4_rcp85 = (CO2_4dc_rcp85-CO2_0dc_rcp85)/(CO2_4dc_rcp85-CO2_0dc_rcp85 + 50),
         f_CO2_5dc_C4_rcp85 = (CO2_5dc_rcp85-CO2_0dc_rcp85)/(CO2_5dc_rcp85-CO2_0dc_rcp85 + 50),
         f_CO2_0dc_C3_rcp45 = (CO2_0dc_rcp45-CO2_0dc_rcp45)/(CO2_0dc_rcp45-CO2_0dc_rcp45 + 100),
         f_CO2_1dc_C3_rcp45 = (CO2_1dc_rcp45-CO2_0dc_rcp45)/(CO2_1dc_rcp45-CO2_0dc_rcp45 + 100),
         f_CO2_2dc_C3_rcp45 = (CO2_2dc_rcp45-CO2_0dc_rcp45)/(CO2_2dc_rcp45-CO2_0dc_rcp45 + 100),
         f_CO2_3dc_C3_rcp45 = (CO2_3dc_rcp45-CO2_0dc_rcp45)/(CO2_3dc_rcp45-CO2_0dc_rcp45 + 100),
         f_CO2_4dc_C3_rcp45 = (CO2_4dc_rcp45-CO2_0dc_rcp45)/(CO2_4dc_rcp45-CO2_0dc_rcp45 + 100),
         f_CO2_5dc_C3_rcp45 = (CO2_5dc_rcp45-CO2_0dc_rcp45)/(CO2_5dc_rcp45-CO2_0dc_rcp45 + 100),
         f_CO2_0dc_C4_rcp45 = (CO2_0dc_rcp45-CO2_0dc_rcp45)/(CO2_0dc_rcp45-CO2_0dc_rcp45 + 50),
         f_CO2_1dc_C4_rcp45 = (CO2_1dc_rcp45-CO2_0dc_rcp45)/(CO2_1dc_rcp45-CO2_0dc_rcp45 + 50),
         f_CO2_2dc_C4_rcp45 = (CO2_2dc_rcp45-CO2_0dc_rcp45)/(CO2_2dc_rcp45-CO2_0dc_rcp45 + 50),
         f_CO2_3dc_C4_rcp45 = (CO2_3dc_rcp45-CO2_0dc_rcp45)/(CO2_3dc_rcp45-CO2_0dc_rcp45 + 50),
         f_CO2_4dc_C4_rcp45 = (CO2_4dc_rcp45-CO2_0dc_rcp45)/(CO2_4dc_rcp45-CO2_0dc_rcp45 + 50),
         f_CO2_5dc_C4_rcp45 = (CO2_5dc_rcp45-CO2_0dc_rcp45)/(CO2_5dc_rcp45-CO2_0dc_rcp45 + 50))

f_CO2_2015_2020_zero <- predicted_f_CO2_2015_2020_zero[13:36]

d_zero <- list(colnames(local_tmp_changes_zero), colnames(f_CO2_2015_2020_zero), colnames(baseline_tmp_grid_df_2015_2020)) # change back to baseline_tmp_df for country level predictions
gr_zero <- expand.grid(d_zero)

models_vars_zero <- gr_zero %>% filter(
  Var1 == "rcp85_0dc" & Var2 == "f_CO2_0dc_C3_rcp85"|
    Var1 == "rcp85_1dc" & Var2 == "f_CO2_1dc_C3_rcp85"|
    Var1 == "rcp85_2dc" & Var2 == "f_CO2_2dc_C3_rcp85"|
    Var1 == "rcp85_3dc" & Var2 == "f_CO2_3dc_C3_rcp85"|
    Var1 == "rcp85_4dc" & Var2 == "f_CO2_4dc_C3_rcp85"|
    Var1 == "rcp85_5dc" & Var2 == "f_CO2_5dc_C3_rcp85"|
    Var1 == "rcp45_0dc" & Var2 == "f_CO2_0dc_C3_rcp45"|
    Var1 == "rcp45_1dc" & Var2 == "f_CO2_1dc_C3_rcp45"|
    Var1 == "rcp45_2dc" & Var2 == "f_CO2_2dc_C3_rcp45"|
    Var1 == "rcp45_3dc" & Var2 == "f_CO2_3dc_C3_rcp45"|
    Var1 == "rcp45_4dc" & Var2 == "f_CO2_4dc_C3_rcp45"|
    Var1 == "rcp45_5dc" & Var2 == "f_CO2_5dc_C3_rcp45"|
    Var1 == "rcp85_0dc" & Var2 == "f_CO2_0dc_C4_rcp85" & Var3 == "Maize" |
    Var1 == "rcp85_1dc" & Var2 == "f_CO2_1dc_C4_rcp85" & Var3 == "Maize" |
    Var1 == "rcp85_2dc" & Var2 == "f_CO2_2dc_C4_rcp85" & Var3 == "Maize" |
    Var1 == "rcp85_3dc" & Var2 == "f_CO2_3dc_C4_rcp85" & Var3 == "Maize" |
    Var1 == "rcp85_4dc" & Var2 == "f_CO2_4dc_C4_rcp85" & Var3 == "Maize" |
    Var1 == "rcp85_5dc" & Var2 == "f_CO2_5dc_C4_rcp85" & Var3 == "Maize" |
    Var1 == "rcp45_0dc" & Var2 == "f_CO2_0dc_C4_rcp45" & Var3 == "Maize" |
    Var1 == "rcp45_1dc" & Var2 == "f_CO2_1dc_C4_rcp45" & Var3 == "Maize" |
    Var1 == "rcp45_2dc" & Var2 == "f_CO2_2dc_C4_rcp45" & Var3 == "Maize" |
    Var1 == "rcp45_3dc" & Var2 == "f_CO2_3dc_C4_rcp45" & Var3 == "Maize" |
    Var1 == "rcp45_4dc" & Var2 == "f_CO2_4dc_C4_rcp45" & Var3 == "Maize" |
    Var1 == "rcp45_5dc" & Var2 == "f_CO2_5dc_C4_rcp45" & Var3 == "Maize") %>% 
  filter(!((endsWith(as.character(Var2), "C3_rcp85") | endsWith(as.character(Var2), "C3_rcp45")) & Var3 == "Maize"))  



prediction_dataframe_2015_2020_list_zero = list()

generate_prediction_values_2015_2020_zero <- function(models_vars_zero) {
  for(i in 1:nrow(models_vars_zero)){
    
    model_index <- models_vars_zero[i,]
    
    data_frame <- data.frame(Temp.Change = local_tmp_changes_zero[model_index$Var1],
                             f_CO2 = f_CO2_2015_2020_zero[model_index$Var2], # change this for CO2 
                             Baseline_tmp_weighted = baseline_tmp_grid_df_2015_2020[model_index$Var3], # change back to baseline_tmp_df for country level - is this for the right crop?
                             adapt_dummy = 0,
                             Precipitation.change = 0) %>% 
      rename(Temp.Change = 1,
             f_CO2 = 2,
             Baseline_tmp_weighted = 3, # change to Baseline_tmp_1995_2005_weighted for Model 3
             adapt_dummy = 4,
             Precipitation.change = 5) %>% 
      mutate(adapt_dummy = as.factor(adapt_dummy)) # since variable coded as factor in AGIMPACTS_TRUNCATED_ADAPT
    
    
    prediction_dataframe_2015_2020_list_zero[[i]] <<- data_frame
    
  }
}

generate_prediction_values_2015_2020_zero(models_vars_zero)

length(prediction_dataframe_2015_2020_list_zero) # 48


rice <- data.frame(C3 = 1, C4 = 0, crop_pooled = "Rice")
soybean <- data.frame(C3 = 1, C4 = 0, crop_pooled = "Soybean")
wheat <- data.frame(C3 = 1, C4 = 0, crop_pooled = "Wheat")
maize <- data.frame(C3 = 0, C4 = 1, crop_pooled = "Maize")

maize_prediction_2015_2020_zero_data <- lapply(prediction_dataframe_2015_2020_list_zero[1:12], cbind, maize)
rice_prediction_2015_2020_zero_data <- lapply(prediction_dataframe_2015_2020_list_zero[13:24], cbind, rice)
soybean_prediction_2015_2020_zero_data <- lapply(prediction_dataframe_2015_2020_list_zero[25:36], cbind, soybean)
wheat_prediction_2015_2020_zero_data <- lapply(prediction_dataframe_2015_2020_list_zero[37:48], cbind, wheat)

# create list of prediction data

crops_prediction_data <- list(maize_prediction_2015_2020_zero_data,
                              rice_prediction_2015_2020_zero_data,
                              soybean_prediction_2015_2020_zero_data,
                              wheat_prediction_2015_2020_zero_data) # 4 crops, 12 scenarios

# write to csv

write_to_csv <- function(i,j){
  
  crops_prediction_data[[j]][[i]] %>% 
    readr::write_csv(here("processed", paste0("crops_prediction_data_", crops[[j]], "_", i, ".csv")))
}


lapply(1:4, function(j){
  lapply(1:12, write_to_csv, j)
})





