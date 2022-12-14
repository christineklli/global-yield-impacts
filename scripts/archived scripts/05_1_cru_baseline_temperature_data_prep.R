# CRU TEMPERATURES - GENERATE PRODUCTION-WEIGHTED BASELINE GROWING SEASON TEMPERATURES BY CROP AND LOCATION 

### Note this script will differ from the one in the agimpacts repo in order to
# split out common steps required for processing temp and precip data, steps required for temp only, and steps required for precip only
# therefore will be 'rewriting' some of the previous steps to separate the common steps
# and avoid re-running steps required for temp only
# this process aims to improve description, naming and commenting
####


# read in CRU temp data  (temp-specific) -----------------------------------------------------

# scale temperature data to country level

tmps_1901_2020 <- nc_open(here("data", "CRU data", "cru_ts4.05.1901.2020.tmp.dat.nc"))

nc_tmp_1901_2020 <- ncvar_get(tmps_1901_2020, attributes(tmps$var)$names[1])
#nc_stn_1901_2020 <- ncvar_get(tmps_1901_2020, attributes(tmps$var)$names[2])
#nc_tmps_lon_1901_2020 <- ncvar_get(tmps_1901_2020,attributes(tmps$dim)$names[1])
#nc_tmps_lat_1901_2020 <- ncvar_get(tmps_1901_2020, attributes(tmps$dim)$names[2])
#nc_tmps_time_1901_2020 <- ncvar_get(tmps_1901_2020, attributes(tmps$dim)$names[3]) # 120 months
#nc_tmps_atts_1901_2020 <- ncatt_get(tmps_1901_2020, 0)

# set dimension names and values to corresponding lon and lat values
#dimnames(nc_tmp_1901_2020) <- list(lon = nc_tmps_lon_1901_2020, lat = nc_tmps_lat_1901_2020, time = nc_tmps_time_1901_2020)
#dimnames(nc_stn_1901_2020) <- list(lon = nc_tmps_lon_1901_2020, lat = nc_tmps_lat_1901_2020)

#attributes(nc_tmp_1901_2020)

# rasterise temperature data directly
(t <- raster::brick(here("data", "CRU data", "cru_ts4.05.1901.2020.tmp.dat.nc")))
str(t)
t_date <- raster::getZ(t)
# 1950-2010
grep("1950-01-16", t_date) # 589
grep("2010-12-16", t_date) # 1320

# subset group 1
t_1950_2010 <- raster::subset(t, 589:1320)
t_1950_2010@z$Date <- t@z$Date[589:1320]



# Scale tmps 1950_2010 to country (temp-specific) ----------------------------------------

tmp_1950_2010_maize_production <- exactextractr::exact_extract(t_1950_2010, worldmap_clean, 'weighted_mean', weights = maize_production)
tmp_1950_2010_rice_production <- exactextractr::exact_extract(t_1950_2010, worldmap_clean, 'weighted_mean', weights = rice_production)
tmp_1950_2010_soybean_production <- exactextractr::exact_extract(t_1950_2010, worldmap_clean, 'weighted_mean', weights = soybean_production)
tmp_1950_2010_wheat_production <- exactextractr::exact_extract(t_1950_2010, worldmap_clean, 'weighted_mean', weights = wheat_production)

# rbind
tmp_1950_2010_crop_production <- rbind(tmp_1950_2010_maize_production,
                                       tmp_1950_2010_rice_production,
                                       tmp_1950_2010_soybean_production,
                                       tmp_1950_2010_wheat_production)

dim(tmp_1950_2010_crop_production) # 972 732

tmp_1950_2010_crop_production %>% readr::write_csv(here("processed", "tmp_1950_2020_crop_production.csv"))

tmp_1950_2010_crop_production <- readr::read_csv(here("processed", "tmp_1950_2020_crop_production.csv"))

# for 1950-1979, merge with crop_season by country and crop
tmp_1950_2010_crop_production <- as.data.frame(tmp_1950_2010_crop_production) %>% 
  mutate(country = rep(worldmap$NAME,4),
         abbrev = rep(worldmap$ISO_A2,4)) %>% 
  relocate(abbrev, country)


# mutate tmp_1950_1979_crop_production using repetitions of crop_monfreda values (4x)

tmp_1950_2010_crop_production <- tmp_1950_2010_crop_production %>% 
  mutate(crop_monfreda = rep(c("maize", "rice", "soybean", "wheat"), each = 243))

# calc weighted average growing season tmp (temp-specific) --------------------------------

# merge crop_season_country_tmp and tmp_1950_1979_crop_production by crop_monfreda values and country

avg_tmp_1950_2010_crop_production <- crop_season_country_dt %>% 
  left_join(tmp_1950_2010_crop_production) # automatically joins by country, abbrev and crop_monfreda

dim(avg_tmp_1950_2010_crop_production) # 3645 752


weights <- avg_tmp_1950_2010_crop_production %>% 
  dplyr::select(c(9:20))

n_growing_months <- avg_tmp_1950_2010_crop_production %>% 
  dplyr::select("n_growing_months")


# now mutate new columns for each year where 
# values = rowmeans of sum product/weighted average of growing season months and monthly temperatures 
# (ie only do weighted average on 12 cols at a time)

# calculate weighted average over every 12 columns (months of 61 years) from column 21 onwards

n_1950_2010 <- 21:ncol(avg_tmp_1950_2010_crop_production)
ind_1950_2010 <- matrix(c(n_1950_2010, rep(NA, 12 - ncol(avg_tmp_1950_2010_crop_production)%%12)), byrow = TRUE, ncol = 12)
ind_1950_2010 <- data.frame(t(na.omit(ind_1950_2010))) # this identifies the columns for 12 months in each year from 1950-2010

# then do matrix addition of those columns by growing month weights div no. growing months 
# to estimate weighted growing season precipitation for each year from 1950-2010
# assign to new df for crop growing season weighted average temperatures 
tmp_weighted_avg_1950_2010 <- do.call(cbind, lapply(ind_1950_2010, function(i) {
  weighted_avg <- Reduce(`+`, Map(`*`, avg_tmp_1950_2010_crop_production[,..i], weights))/n_growing_months}))

# rename all column names

colnames(tmp_weighted_avg_1950_2010) <- paste0("tmp", "_", c(1950:2010))

# add back in crop and country IDs

growing_season_tmp_1950_2010 <- tmp_weighted_avg_1950_2010 %>% 
  cbind(dplyr::select(avg_tmp_1950_2010_crop_production, country, abbrev, crop, crop_name)) %>% 
  relocate(country, abbrev, crop, crop_name)

dim(growing_season_tmp_1950_2010) # 3645 65

growing_season_tmp_1950_2010 %>%  readr::write_csv(here("processed", "growing_season_tmp_1950_2010.csv"))

saveRDS(growing_season_tmp_1950_2010, here("processed", "growing_season_tmp_1950_2010.Rdata")) 


# Baseline average growing season tmp - apply to agimpacts baseline periods --------
# clean data ------------------------------------------------------------ --

# going back to AG_BASELINE_PERIOD, check incomplete data points now
# we will be imputing these missing baseline start/end year values in script 06

# count number of NAs in each column

sapply(AGIMPACTS_MAIN, function(x) sum(is.na(x)))

# count point estimates with NA in at least one of Temp.Change, Precipitation.change, CO2.Change and Yield.Change

AGIMPACTS_MAIN %>% 
  dplyr::select(Temp.Change, Precipitation.change, CO2.Change, Yield.Change) %>% 
  is.na %>% 
  # `!` %>% 
  rowSums %>% 
  as_tibble() %>% 
  group_by(value) %>% 
  summarise(n=n()) # 4139 point estimates have 0 NAs across all four columns

# crop concordance between AGIMPACTS and SACKS/MIRCA crop varieties --------
# prepare concordance to merge with CGIAR data for interannual averaging of gs temp over baseline periods
# create concordance table for baseline_df$Crop and growing_season_tmp_1901_2010$crop - ie matching growing season crop variety to point estimate crop variety

unique(crop_season$crop) # 15

unique(AGIMPACTS_MAIN_COMPLETE$Crop) # 11, note Maize (Monsoon) and Maize (Winter) have been removed due to NAs from original crop list of 13

crop_season[crop_season$crop == "Soybeans", "crop"] <- "Soybean"

crop_season %>% readr::write_csv(here("processed", "crop_season.csv"))

growing_season_tmp_1950_2010[growing_season_tmp_1950_2010$crop_name == "Soybeans", "crop_name"] <- "Soybean"

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

# look up crop_concordance to join in growing_season_tmp_1901_2010

# first cut down the list of crops that will end up being used (i.e. represented in CGIAR data), to 11 


growing_season_tmp_production <- growing_season_tmp_1950_2010 %>% 
  filter(crop_name %in% crop_concordance$GROWING_SEASON_CROP) 

dim(growing_season_tmp_production) # 2187, down from 3645

unique(growing_season_tmp_production$crop_name) # 9 unique growing season crops from Sacks and MIRCA crop categories

growing_season_tmp_production <- growing_season_tmp_production %>% 
  left_join(crop_concordance, by = c("crop_name" = "GROWING_SEASON_CROP")) %>% 
  relocate(country, abbrev, crop_name, AGIMPACTS_BASELINE_CROP) # note this has increased the number of rows by replicating crop-dependent tmp for the other crop categories of AGIMPACTS_BASELINE_CROP, i.e. durum and spring wheat 

unique(growing_season_tmp_production$AGIMPACTS_BASELINE_CROP) # 11 unique crops in agimpacts 

# checks and cleaning ----------------------------
# dynamic range iteratively averaging rowwise over different columns (based on baseline years in baseline_df) and of single row (based on country2 x crop in baseline_df)
# row wise averaging over dynamic range of columns
# or average for all rows and then filter to the single row? inefficient though.
# requires concordance between baseline years in baseline_df and colnames in growing_season_tmp_1901_2010
# save 141 outputs from averaging in list, or as new column in baseline_df
# maybe define group of columns and then rowmean?

colnames(growing_season_tmp_production) # tmp_YEARS from 6:66, ie 61 years for 1950-2010

growing_season_tmp_production <- growing_season_tmp_production %>% 
  rename(baseline_crop = AGIMPACTS_BASELINE_CROP,
         season_crop = crop_name)


# check countries match
length(unique(baseline_df$Country2)) # 42
length(unique(growing_season_tmp_series$abbrev)) # 236

unique(baseline_df$Country2) %in% unique(growing_season_tmp_series$abbrev) # all 42 countries in baseline_df are matched in 236 countries in growing_season_tmp_series

unique(baseline_df$Crop) %in% unique(growing_season_tmp_series$baseline_crop) # all crops are matched as well

# note that when we run the loop below, rows 90 and 110 throw NaNs 
# row 90 relates to AU and the fact that two countries are coded with AU in growing_season_tmp_series, hence return 2x output rows
# row 110 relates to GG (Guernsey) for which there is no temperature data

# there are some country pairs for which the same abbrev is used - this produces two non-unique filtered row matches inside the loop

# look at full list of country abbrev codes

abbrev_group <- growing_season_tmp_production %>% 
  group_by(country, abbrev) %>% 
  summarise(n=n()) %>% 
  print(n=Inf)

# check for duplicates
n_occur <- data.frame(table(abbrev_group$abbrev))
n_occur[n_occur$Freq > 1,] # AU and PS are duplicated codes

abbrev_group %>% 
  filter(abbrev == "AU" | abbrev == "PS") # Gaza and West Bank are both coded PS; Australia and Ashmore and Cartier Is. both coded AU

# check whether these appear in baseline_df

"PS" %in% baseline_df$Country2 # no
"AU" %in% baseline_df$Country2 # yes

# remove Ashmore and Cartier Is. in growing_season_tmp_series

growing_season_tmp_production <- growing_season_tmp_production %>% 
  filter(!country == "Ashmore and Cartier Is.")

dim(growing_season_tmp_production) # 2651 66

growing_season_tmp_production %>% readr::write_csv(here("processed","growing_season_tmp_production.csv"))

saveRDS(growing_season_tmp_production, here("processed", "growing_season_tmp_production.Rdata")) 


# BRANCHING FROM HERE INTO 06_OLS AND 06_IMPUTE ---------------------------



