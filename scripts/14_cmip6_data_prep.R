
# read in cmip6 data from worldclim files ---------------------------------

# first read in 2021-2040, these are not monthly values but annual files because bc dataset
# we will be working with annual data, not monthly/seasonal
# bc variables: take only 2 from every 19 variables
# bio1 annual mean temperature 
# bio12 annual precipitation 
  # bio12 is a total, so need to divide by 12 for mm/month
  # if we decide to work with monthly/seasonal later, then need to download the [pr] monthly data (not a bioclimatic variable) 
  # however, lacking monthly average temperature data from worldclim, only monthly min, monthly max, and annual mean (bc)
  # unless combined with knmi monthly average temperature data - but the spatial resolution is too coarse :(
  # the issue is that it would in fact be better to work with monthly data because we could then calculate future growing season temperatures and precipitation for each year
  # so that temp.change & Pre.change is consistently calculated using growing season temperatures not annual mean temperature or annual mean precipitation...
  # can I get the average monthly temperature by taking the average of average monthly max and average monthly min temperatures? this is a very bad approach
  # monthly mean is generally calculated by adding daily mean temperatures for every day of the month & dividing by total number of days in month

# repeat as function

time_periods <- c("2021-2040", "2041-2060", "2061-2080", "2081-2100")

cmip6_pre_df <- lapply(1:4, function(i){
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
      rename(lon=x, lat=y, pre=layer)
  })
})

saveRDS(cmip6_pre_df, here("processed", "cmip6_pre_df.RData"))

cmip6_tmp_df <- lapply(1:4, function(i){
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
      rename(lon=x, lat=y, tmp=3)
  })
})

saveRDS(cmip6_tmp_df, here("processed", "cmip6_tmp_df.RData")) # this is now in the format [[time period]][[GCM]] with max index [[4]][[24]]
cmip6_tmp_df <- readRDS(here("processed", "cmip6_tmp_df.RData"))
cmip6_pre_df <- readRDS(here("processed", "cmip6_pre_df.RData"))

# check this by printing a plot
r <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, crs=4326, res=0.5)
zxy <- cmip6_tmp_df[[1]][[1]] # 2010-2040, GCM 1
coordinates(zxy) <- ~ lon + lat
gridded(zxy) <- TRUE
raster_zxy <- raster(zxy)
plot(raster_zxy) # so there is not much spatial variation but still more than once transformed into temp.changes below
# especially Chile, Tibetan Plateau, Siberia, north America
# we seem to be losing a lot of spatial variation in calculating the changes

# create CO2 prediction data ----------------------------------------------

# WorldClim does not provide Co2 data
# KNMI also only provides global mean CO2 for CMIP5, not CMIP6
# use previous method of 'predicting' global mean CO2 based on temperature for our prediction data
# alternatively, could just take annual mean CO2 for each time period and append to all of the cmip6_tmp_df data
# don't need to change to NA as model prediction will only work on complete cases

# read in CMIP5 CO2 data - annual not monthly

nc_CO2_rcp85 <- nc_open(here("data", "CMIP5 data", "iRCP85_CO2_1860_2100.nc"))

nc_CO2_var_rcp85 <- ncvar_get(nc_CO2_rcp85, attributes(nc_CO2_rcp85$var)$names[1])

nc_CO2_time_rcp85 <- ncvar_get(nc_CO2_rcp85,attributes(nc_CO2_rcp85$dim)$names[1]) # time, from 0-239

CO2_data <- data.frame(year = c(1860:2100), CO2 = nc_CO2_var_rcp85)

time_periods_years <- data.frame(start_year = c(2021, 2041, 2061, 2081), end_year = c(2040, 2060, 2080, 2100))

CO2_2015 <- CO2_data %>% filter(year==2015) %>% dplyr::select(CO2)

CO2_change <- lapply(1:4, function(i){ # Calculate CO2 change and F(CO2)
            CO2_data %>% 
            filter(year >= time_periods_years$start_year[[i]] & year <= time_periods_years$end_year[[i]]) %>% 
            summarise(CO2 = mean(CO2)) %>% 
            mutate(CO2_change = CO2 - CO2_2015$CO2,
                   f_CO2_C3 = CO2_change/(CO2_change + 100), # allows for declining marginal effect of CO2, from Moore
                   f_CO2_C4 = CO2_change/(CO2_change + 50))
          })

# create mean annual temp change and pre change for each future time period --------

# note these are variables that do not vary with crop

# go back to script 13 and run lines 46-105

# note that CRU data has more temperature NAs than CMIP6 data do
#sum(is.na(tmp_mean_2015$tmp)) # CRU annual mean temperatures for 2015
#[1] 191780

#sum(is.na(cmip6_tmp_df[[4]][[24]])) # CMIP6 list of [[time period]][[GCM]]
#[1] 169891

change_vars <-  lapply(1:4, function(j){ # i = time period
                  lapply(1:24, function(i, j){ # j = GCM 
                    cmip6_tmp_df[[j]][[i]] %>% 
                  left_join(cmip6_pre_df[[j]][[i]], by = c("lon", "lat")) %>% 
                  left_join(tmp_mean_2015, by = c("lon", "lat")) %>% 
                  left_join(pre_mean_2015, by = c("lon", "lat")) %>% 
                  mutate(Temp.Change = tmp - tmp_2015, # in degrees
                         Precipitation.change = (pre - pre_2015)/pre_2015*100) # in percentage terms, but same as yield change in that x100
                  }, j)
                })
# should be read change_vars[[j]][[i]] for change_vars[[1:4]][[1:24]]
saveRDS(change_vars, here("processed", "change_vars.RData"))

change_vars <- readRDS(here("processed", "change_vars.RData"))

# check this by printing a plot
r <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, crs=4326, res=0.5)
xyz <- change_vars[[1]][[1]][,c(1:2,7)] # 2010-2040, GCM 1, variable - repeat on all variables from 3:8 one by one
coordinates(xyz) <- ~ lon + lat
gridded(xyz) <- TRUE
raster_xyz <- raster(xyz)
plot(raster_xyz) # looks about right, mostly warming between 1-5 degrees, with a few places even warmer
# some places in Arctic warming 12-20 degrees in 2021-2040?
# this is relative to baseline of 2015

# there is a fair bit of spatial variation in tmp (WorldClim data), tmp_2015 (CRU data), but very little in Temp.Change
# and as a result, very little spatial variation in gridded predictions
# it makes sense that there would be this little variation in changes - changes are uniform spatially; also the scales
# again, raises the question of whether it might make sense to convert baseline:changes to levels
# and model on levels instead? what would the theoretical model then represent intuitively?
# also is this an unacceptable transformation of the raw data which are specific to change variables?

max(change_vars[[1]][[1]]$Precipitation.change[!is.na(change_vars[[1]][[1]]$Precipitation.change) & !is.infinite(change_vars[[1]][[1]]$Precipitation.change)])
# 42844.44 ???
# 428% increase in rainfall? surely this must be incorrect - somewhere in MENA?
which.max() # index number 39821

change_vars[[1]][[1]] %>% 
  ungroup() %>% 
  filter(!is.na(Precipitation.change) & !is.infinite(Precipitation.change)) %>% 
  mutate(ID=row_number()) %>% 
  filter(ID==39821)

# this is somewhere in Siwa desert in Egypt, where precipitation change in 2015 is 0.00833mm, also applies to surrounding pixels
# and pre by GCM 1 in 2021-2040 is 3.58mm

# read in bs_vars ---------------------------------------------------------

# note these are variables that vary with crop, so index i = crop; bs_2015_vars[[1:4]]

# pause - this needs to be redone because bs_2015_vars contains incorrect country labels, fix this in script 13

bs_2015_vars <- readRDS(here("processed", "bs_2015_vars.RData"))

# create third level 
# first level list of 4 crops - k
# second level list of 4 time periods- i
# third level list of 24 GCMs - j

prediction_data <- lapply(1:4, function(k){ # crops = k - highest level in three-level list; 1:4
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

saveRDS(prediction_data, here("processed", "prediction_data.RData"))


