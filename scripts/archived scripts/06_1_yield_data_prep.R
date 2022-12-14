# incorporating yield analysis from the start means we don't need scripts 19 onwards 
# redo script 8 imputation with help from script 21 which imputes baseline yield data
# not sure what units the yield data are in?

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


# read in GHDY country crop yield time series data ------------------------

crop_yields_lists <- lapply(1:4, function(j){
  lapply(1:36, function(i,j){
    readr::read_csv(here("processed", paste0("crop_yields_lists_", crops[[j]], "_", i, ".csv")))},j)
})

# long dataframe by year 1:36

crop_yields_df <- lapply(1:4, function(i){
  rbindlist(crop_yields_lists[[i]], idcol = "year")})

crop_yields_dt <- rbindlist(crop_yields_df, idcol = "crop_pooled")

crop_numbers <- tribble(
  ~id_col , ~crop_pooled,
  "Maize", 1,
  "Rice", 2,
  "Soybean", 3,
  "Wheat", 4
)

crop_yields_dt <- crop_yields_dt %>%
  left_join(crop_numbers, by ='crop_pooled') %>% 
  mutate(Baseline.start = 1980+year) %>% 
  dplyr::select(!c("crop_pooled", "year")) %>% 
  rename(crop_pooled = "id_col") 


# merge with CGIAR data ---------------------------------------------------

# go back to start of script 08, run data cleaning section
# merge time series country crop yield data with CGIAR data and then re-impute 

# join by crop, baseline start year and country
AGIMPACTS_bs_tp_yields <- AGIMPACTS_bs_tp %>% 
  left_join(crop_yields_dt, 
            by = c("crop_pooled", "Baseline.start", "CountryName"="name", "Country2_fact"="iso_a2")) %>% 
  rename(Baseline_yield = mean_yield)

# check 

AGIMPACTS_bs_tp_yields %>% 
  group_by(crop_pooled) %>% 
  filter(crop_pooled =="Soybean") %>% 
  summarise(years = unique(Baseline.start))

crop_yields_dt %>% 
  group_by(crop_pooled) %>% 
  filter(crop_pooled =="Soybean") %>% 
  summarise(years = unique(Baseline.start))

# soybean baseline yield data isn't available for the years that are in the CGIAR dataset
# therefore all soybean baseline yield and absolute yield change data is imputed

saveRDS(AGIMPACTS_bs_tp_yields, here("processed", "AGIMPACTS_bs_tp_yields.Rdata")) 

# now to script 08 to impute all missing variables including baseline yields, borrow code from script 21 if necessary 

