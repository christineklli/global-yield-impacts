####
# Refer to script 5 for how this was done for baseline temperature - write better code this time
# before running, first go to 05 cru_baseline_temperature_data script - and run all the steps common to temp & precip first
# this script contains steps specific to precip only
# once baseline gs precipitation is calculated, merge into CGIAR data that already contains baseline gs temp data

### WORKFLOW ###
# separate repositories from agimpacts
# slot this script into chronological position, then edit and re-run later scripts reflecting updated data 
# note that where certain raw or processed output files are missing in the repo, need to manually copy from agimpacts repo
# this is because files are too large, ignored by github so don't get cloned

### BASELINE PRECIPITATION CALCULATIONS ###
# Download and clean monthly baseline precipitation data from CRU from 1950-2010
# Extract weighted mean country monthly baseline precipitation 
# Calculate growing season average precipitation (across months) for each country
# Average across country unique baseline periods for each CGIAR data point
# Graph scatterplots of baseline temperature and precipitation ranges represented in CGIAR data

# read in CRU precipitation monthly data ----------------------------------

# gunzip(here("data", "CRU data", "cru_ts4.05.1901.2020.pre.dat.nc.gz"))

pre_1901_2020 <- nc_open(here("data", "CRU data", "cru_ts4.05.1901.2020.pre.dat.nc"))
# units are in mm/month
# show data attributes
attributes(pre_1901_2020$var)

# have a look at the data but do not need to actually extract values for analysis

nc_pre_1901_2020 <- ncvar_get(pre_1901_2020, attributes(pre_1901_2020$var)$names[1])
# nc_stn_1901_2020 <- ncvar_get(tmps_1901_2020, attributes(tmps$var)$names[2])
#nc_tmps_lon_1901_2020 <- ncvar_get(tmps_1901_2020,attributes(tmps$dim)$names[1])
#nc_tmps_lat_1901_2020 <- ncvar_get(tmps_1901_2020, attributes(tmps$dim)$names[2])
#nc_tmps_time_1901_2020 <- ncvar_get(tmps_1901_2020, attributes(tmps$dim)$names[3]) # 120 months
#nc_tmps_atts_1901_2020 <- ncatt_get(tmps_1901_2020, 0)

# set dimension names and values to corresponding lon and lat values
#dimnames(nc_tmp_1901_2020) <- list(lon = nc_tmps_lon_1901_2020, lat = nc_tmps_lat_1901_2020, time = nc_tmps_time_1901_2020)
#dimnames(nc_stn_1901_2020) <- list(lon = nc_tmps_lon_1901_2020, lat = nc_tmps_lat_1901_2020)

#attributes(nc_tmp_1901_2020)

# restrict analysis to 1950-2020
(p <- raster::brick(here("data", "CRU data", "cru_ts4.05.1901.2020.pre.dat.nc")))
str(p)
p_date <- raster::getZ(p)

# identify index of raster layers corresponding to 1950-2010
grep("1950-01-16", p_date) # 589
grep("2010-12-16", p_date) # 1320

# subset RasterStack into 1950-2020 
p_1950_2010 <- raster::subset(p, 589:1320)
p_1950_2010@z$Date <- p@z$Date[589:1320]



# Scale tmps 1950_2010 to country  ----------------------------------------

worldmap <- rworldmap::getMap(resolution = "coarse")

worldmap_clean <- cleangeo::clgeo_Clean(worldmap) # some polygons are not closed, i.e. Canada and Western Sahara

# extract country average precipitation weighted by where the crop is grown within the country, for each year from 1950-2010

pre_1950_2010_maize_production <- exactextractr::exact_extract(p_1950_2010, worldmap_clean, 'weighted_mean', weights = maize_production)
pre_1950_2010_rice_production <- exactextractr::exact_extract(p_1950_2010, worldmap_clean, 'weighted_mean', weights = rice_production)
pre_1950_2010_soybean_production <- exactextractr::exact_extract(p_1950_2010, worldmap_clean, 'weighted_mean', weights = soybean_production)
pre_1950_2010_wheat_production <- exactextractr::exact_extract(p_1950_2010, worldmap_clean, 'weighted_mean', weights = wheat_production)

# rbind for crop production location-weighted avg monthly precipitation for each country 
pre_1950_2010_crop_production <- rbind(pre_1950_2010_maize_production,
                                       pre_1950_2010_rice_production,
                                       pre_1950_2010_soybean_production,
                                       pre_1950_2010_wheat_production)

dim(pre_1950_2010_crop_production) # 972 732

pre_1950_2010_crop_production %>% readr::write_csv(here("processed", "pre_1950_2020_crop_production.csv"))

# merge with crop_season by country and crop
pre_1950_2010_crop_production <- as.data.frame(pre_1950_2010_crop_production) %>% 
  mutate(country = rep(worldmap$NAME,4),
         abbrev = rep(worldmap$ISO_A2,4)) %>% 
  relocate(abbrev, country)


# mutate using repetitions of crop_monfreda values (4x)

pre_1950_2010_crop_production <- pre_1950_2010_crop_production %>% 
  mutate(crop_monfreda = rep(c("maize", "rice", "soybean", "wheat"), each = 243))

# calc weighted average growing season precip --------------------------------

# run all necessary steps common to temp & precip and temp-specific steps in 05 CRU temp first (lines 63-119)
# then run precip-specific calcs here

# require crop_season_country_dt
# read this in from csv, as "c" and then refactorise as "dbl" using similar steps to start of 05 cru-temp if required

# merge crop_season_country_dt and pre_1950_2010_crop_production by crop_monfreda values and country

avg_pre_1950_2010_crop_production <- crop_season_country_dt %>% 
  left_join(pre_1950_2010_crop_production) # automatically joins by country, abbrev and crop_monfreda

dim(avg_pre_1950_2010_crop_production) # 3645 752
# rows represent unique identifier for combinations of country (243) and crop (15)

pre_weights <- avg_pre_1950_2010_crop_production %>% 
  dplyr::select(c(9:20)) # these are columns for jan to dec 

pre_n_growing_months <- avg_pre_1950_2010_crop_production %>% 
  dplyr::select("n_growing_months") # select column as external vector

# now mutate new columns for each year where 
# values = rowmeans of sum product/weighted average of growing season months and monthly temperatures 
# (ie only do weighted average on 12 cols at a time)

# calculate weighted average over every 12 columns (months of 61 years) from column 21 onwards

pre_n_1950_2010 <- 21:ncol(avg_pre_1950_2010_crop_production)
pre_ind_1950_2010 <- matrix(c(pre_n_1950_2010, rep(NA, 12 - ncol(avg_pre_1950_2010_crop_production)%%12)), byrow = TRUE, ncol = 12)
pre_ind_1950_2010 <- data.frame(t(na.omit(pre_ind_1950_2010))) # this identifies the columns for 12 months in each year from 1950-2010

# then do matrix addition of those columns by growing month weights div no. growing months 
# to estimate weighted growing season precipitation for each year from 1950-2010

# assign to new df for crop growing season weighted average temperatures 
pre_weighted_avg_1950_2010 <- do.call(cbind, 
                                      lapply(pre_ind_1950_2010, function(i) { # loop through each year-column of pre_ind_1950_2010 
                                          weighted_avg <- Reduce(`+`, # getting stuck trying to apply each column to select in avg_pre..
                                                            Map(`*`, avg_pre_1950_2010_crop_production[,..i], pre_weights))/pre_n_growing_months}))

# rename all column names

colnames(pre_weighted_avg_1950_2010) <- paste0("pre", "_", c(1950:2010))

# add back in crop and country IDs

growing_season_pre_1950_2010 <- pre_weighted_avg_1950_2010 %>% 
  cbind(dplyr::select(avg_pre_1950_2010_crop_production, country, abbrev, crop, crop_name)) %>% 
  relocate(country, abbrev, crop, crop_name)

dim(growing_season_pre_1950_2010) # 3645 65

growing_season_pre_1950_2010 %>%  readr::write_csv(here("processed", "growing_season_pre_1950_2010.csv"))

saveRDS(growing_season_pre_1950_2010, here("processed", "growing_season_pre_1950_2010.Rdata")) 


# prepare concordance to merge with CGIAR data for interannual averaging of gs precip over baseline periods -------------------------------------------------------

growing_season_pre_1950_2010  <-   readr::read_csv(here("processed", "growing_season_pre_1950_2010.csv"))

# there are a few temp&precip-common steps - run first in 05 cru temp script lines 276-301

growing_season_pre_1950_2010[growing_season_pre_1950_2010$crop_name == "Soybeans", "crop_name"] <- "Soybean"

# look up crop_concordance to join in growing_season_tmp_1901_2010

# first cut down the list of crops that will end up being used (i.e. are represented in CGIAR data), to 11 

growing_season_pre_production <- growing_season_pre_1950_2010 %>% 
  filter(crop_name %in% crop_concordance$GROWING_SEASON_CROP) 

dim(growing_season_pre_production) # 2187, down from 3645

unique(growing_season_pre_production$crop_name) # 9 unique growing season crops from Sacks and MIRCA crop categories

growing_season_pre_production <- growing_season_pre_production %>% 
  left_join(crop_concordance, by = c("crop_name" = "GROWING_SEASON_CROP")) %>% 
  relocate(country, abbrev, crop_name, AGIMPACTS_BASELINE_CROP) 
# note this has increased the number of rows by replicating crop-dependent pre for the other crop categories of AGIMPACTS_BASELINE_CROP, i.e. durum and spring wheat 

unique(growing_season_pre_production$AGIMPACTS_BASELINE_CROP) # 11 unique crops in agimpacts 

# prepare for joining growing season precip data with CGIAR baseline period data ----------------------------

# dynamic range iteratively averaging rowwise over different columns (based on baseline years in baseline_df) and of single row (based on country2 x crop in baseline_df)
# row wise averaging over dynamic range of columns
# or average for all rows and then filter to the single row? inefficient though.
# requires concordance between baseline years in baseline_df and colnames in growing_season_tmp_1901_2010
# save 141 outputs from averaging in list, or as new column in baseline_df
# maybe define group of columns and then rowmean?

colnames(growing_season_pre_production) # tmp_YEARS from 6:66, ie 61 years for 1950-2010

growing_season_pre_production <- growing_season_pre_production %>% 
  rename(baseline_crop = AGIMPACTS_BASELINE_CROP,
         season_crop = crop_name)

# there are a few temp&precip checks also to run in 05 cru-temp script from line 320
# requires filtering for problem countries in the precipitation dataset

growing_season_pre_production <- growing_season_pre_production %>% 
  filter(!country == "Ashmore and Cartier Is.")

dim(growing_season_pre_production) # 2651 66

# now ready for joining growing season annual growing season precipitation data with CGIAR data
# do this by editing script 06 but ONLY merge with the version of CGIAR data AFTER it already contains baseline gs temperature data
# this is to minimise unnecessary re-running of steps to merge baseline gs temp data with CGIAR data
# in essence we are appending baseline gs precip data to CGIAR(+baseline gs temp) data

growing_season_pre_production %>% readr::write_csv(here("processed","growing_season_pre_production.csv"))

saveRDS(growing_season_pre_production, here("processed", "growing_season_pre_production.Rdata")) 
