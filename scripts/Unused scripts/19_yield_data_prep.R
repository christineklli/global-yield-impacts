

# Read in world boundaries/spatial polygons ------------------------------------------

worldmap <- rworldmap::getMap(resolution = "coarse")

worldmap_clean <- cleangeo::clgeo_Clean(worldmap) # some polygons are not closed, i.e. Canada and Western Sahara


r <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, crs=4326, res=0.5) # 191 countries

r_highres <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, crs=4326, res=0.05) # 240 countries

world <- worldmap_clean %>% 
  st_as_sf() %>% 
  mutate(country=as.numeric(as.factor(NAME)))

world_ras <- rasterize(world, r, field = 'country') # 1-243
plot(world_ras)

world_d <- as.data.frame(world_ras) %>% 
  mutate(cell=row_number()) %>% 
  rename(country='layer') %>% 
  #  dplyr::filter(!is.na(country)) %>% 
  as.data.table 


# join with country name

countries <- data.frame(
  country = as.numeric(as.factor(worldmap_clean@data$NAME)),
  name = worldmap_clean@data$NAME) %>% 
  as.data.table()

length(unique(countries$country))

world_d <- countries[world_d, on='country'] # this is now super highres - to knock it back to lowres would yield 191 countries
# unless we keep highres, disaggregate pooled_predictions_yield? would need to rasterise, disagg to 0.05, and then de-rasterise back to df
# need coordinates and then rasterFromXYZ



# Read in GAEZ production data -------------------------------------------------------

# 5 arcminutes, similar to MIRCA2000 - convert to 30 arcmins/0.5 degrees?
# units in 1000 tonnes per 5-arcmin grid cell - can try to calc x 1000
# convert NA values to NA - no fill value replacement needed

# how is production calculated from yield t/ha? yield = production/harvested area
# this will inform whether it is equivalent to calculate absolute annual production change or absolute yield change
# does it make sense to aggregate absolute yield change to the country level? when units are in t/ha? 
# interpretation would be average change in average yields
# but yield change percentages may only make sense when applied to yields, not to annual production?


crop_production_gaez <- c("GAEZAct2015_Production_Maize_Total.tif",
                          "GAEZAct2015_Production_Rice_Total.tif",
                          "GAEZAct2015_Production_Soybean_Total.tif",
                          "GAEZAct2015_Production_Wheat_Total.tif")


crop_production_gaez <- here("data", "GAEZ data", crop_production_gaez)

# rasterise and stack

crop_production_gaez_raster <- lapply(crop_production_gaez, raster)

crop_production_gaez_raster <- stack(crop_production_gaez_raster)

res.factor <- c(0.5, 0.5)/raster::res(crop_production_gaez_raster) # 0.5 0.5 / 0.083 0.083 
crop_production_gaez_stack <- raster::aggregate(crop_production_gaez_raster, fact = res.factor, fun = sum) 
# v large max values for each rasterlayer, check this

# turn crop_production_gaez_raster into df
# multiply pooled_predictions list element by element, by crop_production_gaez_raster

crop_production_gaez_dt <- lapply(1:4, function(i) {
  as.data.table(as.data.frame(crop_production_gaez_raster[[i]])) %>% 
    rename(production=1) %>% 
    mutate(cell=row_number())})

# check global sum totals
global_crop_production_gaez <- lapply(1:4, function(i) {
  apply(crop_production_gaez_dt[[i]], 2, function(x) {sum(x, na.rm=TRUE)})}) 

# do the same for aggregated gaez

# first unstack into list
crop_production_gaez_agg <- unstack(crop_production_gaez_stack)

crop_production_gaez_agg_dt <- lapply(1:4, function(i) {
  as.data.table(as.data.frame(crop_production_gaez_agg[[i]])) %>% 
    rename(production=1) %>% 
    mutate(cell=row_number())})

lapply(1:4, function(i) {
  apply(crop_production_gaez_agg_dt[[i]], 2, function(x) {sum(x, na.rm=TRUE)})}) # this is the same as global_crop_production_gaez


# Read in GAEZ yield data -------------------------------------------------

crop_yield_gaez <- c("GAEZAct2015_Yield_Maize_Mean.tif",
                     "GAEZAct2015_Yield_Rice_Mean.tif",
                     "GAEZAct2015_Yield_Soybean_Mean.tif",
                     "GAEZAct2015_Yield_Wheat_Mean.tif")


crop_yield_gaez <- here("data", "GAEZ data", crop_yield_gaez)

# rasterise and stack

crop_yield_gaez_raster <- lapply(crop_yield_gaez, raster)

crop_yield_gaez_raster <- stack(crop_yield_gaez_raster)

res.factor <- c(0.5, 0.5)/raster::res(crop_yield_gaez_raster) # 0.5 0.5 / 0.083 0.083 
crop_yield_gaez_stack <- raster::aggregate(crop_yield_gaez_raster, fact = res.factor, fun = mean) 
# v large max values for each rasterlayer, check this

# turn crop_production_gaez_raster into df
# multiply pooled_predictions list element by element, by crop_production_gaez_raster

crop_yield_gaez_dt <- lapply(1:4, function(i) {
  as.data.table(as.data.frame(crop_yield_gaez_raster[[i]])) %>% 
    rename(yield=1) %>% 
    mutate(cell=row_number())})

# first unstack into list
crop_yield_gaez_agg <- unstack(crop_yield_gaez_stack)

crop_yield_gaez_agg_dt <- lapply(1:4, function(i) {
  as.data.table(as.data.frame(crop_yield_gaez_agg[[i]])) %>% 
    rename(yield=1) %>% 
    mutate(cell=row_number())})

country_baseline_list_2 <- lapply(1:4, function(i) {
  
  crop_yield_gaez_agg_dt[[i]] %>% 
    left_join(world_d, by="cell") %>% 
    as.data.frame() %>% 
    group_by(country, name) %>% # only 191 countries
    summarise(mean_yield = mean(yield, na.rm=TRUE)) # sum grid cell predictions for country predicted absolute yield change
  
}) 

# Read in Monfreda yield data ---------------------------------------------


crop_yield_monf <- c("maize_YieldPerHectare.tif",
                     "rice_YieldPerHectare.tif",
                     "soybean_YieldPerHectare.tif",
                     "wheat_YieldPerHectare.tif")


crop_yield_monf <- here("data", "Monfreda data", crop_yield_monf)

# rasterise and stack

crop_yield_monf_raster <- lapply(crop_yield_monf, raster)

crop_yield_monf_raster <- stack(crop_yield_monf_raster)

res.factor <- c(0.5, 0.5)/raster::res(crop_yield_monf_raster) # 0.5 0.5 / 0.083 0.083 
crop_yield_monf_stack <- raster::aggregate(crop_yield_monf_raster, fact = res.factor, fun = mean) 
# v large max values for each rasterlayer, check this

# turn crop_production_gaez_raster into df
# multiply pooled_predictions list element by element, by crop_production_gaez_raster

crop_yield_monf_dt <- lapply(1:4, function(i) {
  as.data.table(as.data.frame(crop_yield_monf_raster[[i]])) %>% 
    rename(yield=1) %>% 
    mutate(cell=row_number())})

# first unstack into list
crop_yield_monf_agg <- unstack(crop_yield_monf_stack)

crop_yield_monf_agg_dt <- lapply(1:4, function(i) {
  as.data.table(as.data.frame(crop_yield_monf_agg[[i]])) %>% 
    rename(yield=1) %>% 
    mutate(cell=row_number())})


# Read GDHY yield data ---------------------------------------------------------------

# netcdf data, nc4
# need to read in all years from 1981-2016, for each crop
crops_lwr <- c("maize", "rice", "soybean", "wheat")

crop_yields_lists <- lapply(1:4, function(j){
  
  lapply(1:36, function(i,j) {
  
    crop_yields_ts <- list.files(here("data", "GHDY data", crops_lwr[[j]]), pattern = "^.*\\.(nc4)$")
  
    data <- nc_open(here("data", "GHDY data", crops_lwr[[j]], crop_yields_ts[[i]]))
    
    lon <- ncvar_get(data, "lon")
    lat <- ncvar_get(data, "lat")
    yields <- ncvar_get(data, "var")
    
    fillvalue <- ncatt_get(data, "var", "_FillValue")
    
    yields[yields == fillvalue$value] <- NA
    
    # set dimension names and values to corresponding lon and lat values
    dimnames(yields) <- list(lon = lon, lat = lat)
    
    # transform into dataframe
    colnames(yields) <- c(seq(-89.75, 89.75, 0.5)) # lat
    rownames(yields) <- c(seq(0.25, 359.75, 0.5)) # lon, this needs to be converted from -180 to 180 # -179.75, 179.75, 0.5
    
    # try converting to raster and performing exactextractr instead
    
    raster_yields <- raster(yields, xmn=-90, xmx=90, ymn=0, ymx=360, 
                            crs=4326) # xmn=0.25, xmx=359.75
    
    #plot(raster_yields)
    
    m_raster_yields <- flip(t(raster_yields), direction = 'y')
    
    rm_raster_yields <- rotate(m_raster_yields)
    #plot(rm_raster_yields)
  
    #rasterVis::levelplot(rm_raster_yields, 
    #                     col.regions = rev(terrain.colors(10000)))
                         # at = seq(-100,100))
    
    ghdy_country_yields <- exact_extract(rm_raster_yields, worldmap_clean, 'mean')
    
    ghdy_country_yields %>% 
      as.data.frame() %>%
      rename(mean_yield = 1) %>% 
      cbind(name = worldmap_clean@data$NAME,
            iso_a2 = worldmap_clean@data$ISO_A2)
    
    # this matches up well with UN FAO data  https://ourworldindata.org/crop-yields
  }, 
    j)}
)
 
# write csv 
lapply(1:4, function(j){
    lapply(1:36, function(i,j){
      crop_yields_lists[[j]][[i]] %>% 
      readr::write_csv(here("processed", 
                       paste0("crop_yields_lists_", crops[[j]], "_", i, ".csv")))}
           , j)
  })


#yields_df <- reshape2::melt(yields) 
# aggregate to country level using worldmap/world_d
#yields_df %>% 
# cbind(world_d) %>% 
#as.data.table() %>% 
#group_by(country, name) %>% 
#summarise(mean_yield=mean(value,na.rm=TRUE)) %>% # the countries with non-NA data are maize producers, but missing so many other countries
#print(n=Inf)
# somehow this does not capture most countries

# match up to CGIAR data baseline years, left_join by year and crop? 
# otherwise if left joining all, would be 144 cols - too many
# keep as list, structured as years from 1981-2016 (1:36)


