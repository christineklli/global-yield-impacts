
# SACKS data is used for all general crops
# MIRCA data is used for differentiating rainfed and irrigated crops 

# READ SACKS CROP CALENDAR DATA -------------------------------------------

# Assume have already decompressed all crops from .tar to .gz data to .nc data

# creates an empty data frame
crop_data_long <- data.frame() 

# loop through the Sacks crop files  

files <- list.files(here("data", "Sacks data"), pattern = "^.*\\.(nc|NC|Nc|Nc)$")

process_nc <- function(files){
  # iterate through the nc
  for(i in 1:length(files)){ 
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
    
    crop_calendar_df$crop <- files[i]
    
    crop_data_long <- rbind(crop_data_long, crop_calendar_df)
    
  }
  print(crop_data_long)
}


crop_calendar <- process_nc(files)

# shorten file name for crop value

crop_calendar$crop <- substr(crop_calendar$crop, 1, nchar(crop_calendar$crop)-22)

# clean data

crop_calendar <- crop_calendar %>% mutate(plant.date = as.Date(plant, origin = "2000-01-01"),
                                          harvest.date = as.Date(harvest, origin = "2000-01-01"))

crop_calendar$lon <- round(crop_calendar$lon, digits = 2)
crop_calendar$lat <- round(crop_calendar$lat, digits = 2)

dim(crop_calendar) # 1814400, 9

colnames(crop_calendar) # lon, lat, index, filled_index, plant, harvest, crop, plant.date, harvest.date

crop_calendar %>% 
  group_by(crop) %>% 
  summarise(n=n()) # 259200 each, note that not all coordinates will have harvest and plant dates, most will have NA

crop_calendar %>% 
  group_by(crop) %>%
  count(is.na(plant.date)) # NA plant date coordinates are variable across the crops

# generate plant and harvest month


# Load MIRCA 2000 data ------------------------------------

mirca_30mn_data <- here("data", "MIRCA data", "CELL_SPECIFIC_CROPPING_CALENDARS_30MN.txt")

mirca_30mn <- read.table(mirca_30mn_data, header = TRUE)

dim(mirca_30mn) # 879244 10

# dimensions don't match crop calendar, because duplicated across an uneven number of crops - there are no missing values, so only crops that grow in a coordinate are recorded
# look at dimensions of unique lat/lon coordinates

dim(unique(mirca_30mn[c("lon","lat")])) # 39449 - also only takes for some crops... 

#### need to ensure when we do a join, missing values are auto generated for empty coordinates with no crop records

dim(crop_calendar)[1]/7 # 259200

# note mirca_30mn lat/lon cells are arranged in different order from crop_calendar, also note crop_calendar not in month form yet'

# Codes for crop classes
# Wheat - 1 (IRC), 27 (RFC)
# Maize - 2 (IRC), 28 (RFC)
# Rice - 3 (IRC), 29 (RFC)
# Soybeans - 8 (IRC), 34 (RFC)

# process MIRCA crop calendar data
# based on the "readme_growing_periods_listed.txt", column "start" and "end" give the months, we also want "crop" and "lat" and "lon"
# mirca_data <- gunzip("CELL_SPECIFIC_CROPPING_CALENDARS.TXT.gz")

# 5 arcminutes is equivalent to 1/12 degrees (1 arcminute = 1/60 degrees. There are 60 arcminutes [minutes of arc] in one degree), which is much higher than 0.5 degrees
# 30 ARCMINUTES IS EQUIVALENT TO 0.5 DEGREE SPATIAL RESOLUTION (30/60 = 0.5


# subset mirca_30mn for crops we care about: 1, 2, 3, 8, 27, 28, 29, 34

mirca_crop <- mirca_30mn %>% 
  filter(crop %in% c(1,2,3,8,27,28,29,34)) %>% 
  dplyr::select(!c("cell_ID", "row", "column", "subcrop"))

# recode crop codes to character names

mirca_crop %>% 
  mutate(crop = replace(crop, crop == 1, "Wheat (Irrigated"))

mirca_crop <- mirca_crop %>% 
  mutate(crop = ifelse(crop == 1, "Wheat (Irrigated)",
                       ifelse(crop == 27, "Wheat (Rainfed)",
                              ifelse(crop == 2, "Maize (Irrigated)",
                                     ifelse(crop == 28, "Maize (Rainfed)",
                                            ifelse(crop == 3, "Rice (Irrigated)",
                                                   ifelse(crop == 29, "Rice (Rainfed)",
                                                          ifelse(crop == 8, "Soybeans (Irrigated)",
                                                                 ifelse(crop == 34, "Soybeans (Rainfed)", crop)))))))))

mirca_crop <- mirca_crop %>% 
  rename(plant_month = start, 
         harvest_month = end)

# align Sacks and MIRCA data columns in preparation for rbind - rename and relocate colnames to match Sacks data

#colnames(crop_calendar)

# convert plant and harvest dates to months, drop plant/harvest/date vars

crop_calendar <- crop_calendar %>% 
  mutate(plant_month = lubridate::month(plant.date),
         harvest_month = month(harvest.date)) %>% 
  dplyr::select(!c("filled_index", "plant", "harvest", "plant.date", "harvest.date")) %>% 
  relocate(c("lon", "lat", "crop", "plant_month", "harvest_month")) 

# sum(!is.na(crop_calendar$plant_month)) # 460765
#sum(!is.na(crop_calendar$harvest_month)) # 460765

# coordinates will either have no info on either plant and harvest or info on both

#colnames(crop_calendar) # lon, lat, crop, plant_month, harvest_month

#colnames(mirca_crop) # lat, lon, crop, area, plant_month, harvest_month

mirca_crop <- mirca_crop %>% 
  relocate(c("lon", "lat", "crop", "plant_month", "harvest_month")) %>% 
  dplyr::select(!area)

all.equal(colnames(crop_calendar), colnames(mirca_crop))

saveRDS(crop_calendar, here("processed", "crop_calendar.RData"))

# Append/rbind MIRCA crop calendar data with Sacks crop_calendar data --------

crop_season <- rbind(crop_calendar, mirca_crop)

dim(crop_season) # 2,013,110 rows 5 cols

# Generate growing season length and growing month indicators for combined Sacks and MIRCA data --------

# create variable equal to difference between plant and harvest months, i.e. the growing season range (n_growing_months)
# e.g. if plant_month is Feb and harvest_month is Aug, growing_months = 6

# define n_growing_months to be:
# if harvest_month >= plant_month, harvest_month - plant_month + 1 
# e.g. April to May = 2 months
# if harvest_month < plant_month, n_growing months = 12 - plant_month  + harvest_month - 0
# e.g. September year 1 to August year 2 = 11 months (12 - 9 + 1 + 8 - 0 = 3 + 8 = 12) 

crop_season <- crop_season %>% 
  mutate(n_growing_months = ifelse(harvest_month >= plant_month, harvest_month - plant_month + 1,
                                   12 - plant_month + 1 + harvest_month - 0)) %>% 
  relocate("lon", "lat", "crop", "plant_month", "harvest_month", "n_growing_months")

#sum(!is.na(crop_season$n_growing_months)) # 659,475

# create 12 monthly indicator dummy vars for whether the crop/cell grows in that month (jan_growing, ... , dec_growing)
# based on plant_month + growing_months ; would have to be plant_month + i of length[1: growing_months]
# or more simply, 1 for >= Plant_month & <= Harvest_month, else 0
# but need to match plant_month to index of [1:12] from jan_growing to dec_growing
# note n_growing_months becomes unnecessary, merely descriptive
# e.g. Jan = 0, Feb = 1, April = 1, ..., August = 1, Sept = 0, Oct = 0, etc


# https://community.rstudio.com/t/creating-dummy-columns-based-on-multiple-columns/58145/2

#  fill in the rest for 1 if >= Plant_month & <= Harvest_month, else 0
# need to account for months where plant_month is closer to end of year than harvest_month is
crop_season <- crop_season %>% 
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
                              ifelse(plant_month > harvest_month & 12 <= harvest_month, 1, 0)))) # %>% 
# slice(100000:100010)

crop_season <- crop_season %>% 
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
         dec_growing = "12")

# check
#sum(!is.na(crop_season$n_growing_month)) # 659475
#dim(crop_season) # 2013110      18

crop_season %>% 
  group_by(n_growing_months) %>% 
  summarise(n=n()) # many coordinates - 1353635 - (from Sacks dataset) have no crop calendar info

crop_season %>% 
  group_by(crop) %>% 
  summarise(n=n())

crop_season %>% readr::write_csv(here("processed", "crop_season_04.csv"))

# save.image("agimpacts-200521.RData")

# save.image("agimpacts-210521.RData")

saveRDS(crop_season, here("processed", "crop_season.RData"))

