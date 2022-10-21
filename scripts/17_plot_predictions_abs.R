
# read in pooled predictions ----------------------------------------------

predictions_level_2_abs <- readRDS(here("results",
                                    "predictions",
                                    "predictions_level_2_abs.RData"))


# global mean -------------------------------------------------------------

predictions_level_2_abs[[4]][[4]] %>% ungroup() %>% summarise(mean=mean(pred_bar,na.rm=TRUE))



# function to rasterise all predictions--------

# note we want to save space so should only extend grid to include NAs AFTER pooling and BEFORE plotting (in script 17)
# shift this code to script 17


predictions_level_2_abs_all_coords <- lapply(1:4, function(j){ # crop
  lapply(1:4, function(l,j){ # time period
    predictions <- data.frame(lon = coords_countries$lon, lat = coords_countries$lat) %>% # script 13
      left_join(dplyr::select(
        predictions_level_2_abs[[j]][[l]], # j (crop), l (time period)
        pred_bar,
        v_w,
        v_b,
        se_p,
        lwr_p,
        upr_p,
        lon, 
        lat), 
        by = c("lon", "lat")) 
  }, j) # time period
}) 


saveRDS(predictions_level_2_abs_all_coords, 
        here("results", "predictions", 
             "predictions_level_2_abs_all_coords.RData")) # predictions with all lon lat including NAs

predictions_level_2_abs_all_coords <- readRDS( 
  here("results", "predictions", 
       "predictions_level_2_abs_all_coords.RData")) 


# convert back to relative yield changes ----------------------------------

# need crop_yield_monf_dt from script 17 

crop_yield_monf_dt <- readRDS(here("processed",
                                   "crop_yield_monf_dt.RData"))

predictions_level_2_pct_all_coords <- lapply(1:4, function(j){ # crop
  lapply(1:4, function(l,j){ # time period
    predictions <- predictions_level_2_abs_all_coords[[j]][[l]] %>% # j (crop), l (time period), 
      left_join(crop_yield_monf_dt[[j]], by=c("lon", "lat")) %>% 
      group_by(lon, lat) %>% 
      mutate(bs_yield=ifelse(bs_yield==0, 0.01, bs_yield), # otherwise have 30822 pixels with infinite values
        Yield.Change=(pred_bar/bs_yield*100)) 
        
  }, j) # time period
}) 

saveRDS(predictions_level_2_pct_all_coords, 
        here("results", "predictions", 
             "predictions_level_2_pct_all_coords.RData"))

predictions_level_2_pct_all_coords <- readRDS( 
        here("results", "predictions", 
             "predictions_level_2_pct_all_coords.RData"))


# predictions_level_2_pct_all_coords
# crop_yield_monf_dt

sum(is.na(crop_yield_monf_dt[[1]]$bs_yield)) # 191812
sum(is.na(predictions_level_2_pct_all_coords[[1]][[1]]$pred_bar)) # 198085
sum(is.na(predictions_level_2_pct_all_coords[[1]][[1]]$Yield.Change)) # 198085
sum(is.na(predictions_level_2_pct_all_coords[[1]][[1]]$bs_yield)) # 191812
# bs_yield from crop_yield_monf_dt shouldn't be the reason why there are so many more NAs in the pct yield change results
# also there appear to be fewer NAs than in the other dataset - why isn't it plotting all?
# it may be that the NAs in the Monfreda data and the worldclim data do not really match up?
# but bs_yield is literally in the prediction data -> so if that were the issue then we would have more Nas in pred_bar too

# infinite values?
sum(is.infinite(predictions_level_2_pct_all_coords[[1]][[1]]$Yield.Change)) 

# rasterise ---------------------------------------------------------------

predictions_level_2_raster_abs <- lapply(1:4, function(j){ # crop
  lapply(1:4, function(l,j){ # time period
    xyz <- predictions_level_2_abs_all_coords[[j]][[l]][c(1:2,3)] # lon, lat, pred_bar 
    rasterFromXYZ(xyz) 
  }, j) # time period
}) 

predictions_level_2_raster_pct <- lapply(1:4, function(j){ # crop
  lapply(1:4, function(l,j){ # time period
    xyz <- predictions_level_2_pct_all_coords[[j]][[l]][c(1:2,10)] # lon, lat, Yield Change 
    rasterFromXYZ(xyz) 
  }, j) # time period
}) 


# plot gridded --------------------------------------------------------------------

# at least these look better than predictions_level_2_raster in script 17_plot_predictions.R across all crops
rasterVis::levelplot(predictions_level_2_raster_abs[[4]][[1]],  # [[j]][[l]]
                     #at=seq(-100,100),
                     col.regions = rev(terrain.colors(10000)),
                     margin=FALSE)

# in general absolute yield change predictions make more sense than percentage yield changes
# though note that restricting the ranges changes results visually
# note this looks completely different from old GAMM model gridded pooled predictions with same range
# how much of that is due to pooling? no - from prediction_level_3 plots, it is not an issue from pooling


# vary this manually, dynamically to plot individual plot
rasterVis::levelplot(predictions_level_2_raster_pct[[4]][[1]],  # [[j]][[l]]
                     at=seq(-300,400),
                     col.regions = rev(terrain.colors(10000)),
                     margin=FALSE)

plot(predictions_level_2_raster_abs[[4]][[4]])

predictions_level_2[[2]][[1]] %>% 
  filter(pred_bar > -50) %>% 
  ggplot() +
  geom_tile(aes(x=lon, y=lat, fill = pred_bar)) +
  scale_fill_gradientn(colours = rev(terrain.colors(10000))) +
  theme_bw() +
  ylab("Longitude") +
  xlab("Latitude") +
  labs(fill = "Pooled fit (%)")


predictions_level_2_pct_all_coords[[1]][[1]] %>% 
  filter(pred_bar > -100) %>% 
  ggplot() +
  geom_tile(aes(x=lon, y=lat, fill = Yield.Change)) +
  scale_fill_gradientn(colours = rev(terrain.colors(10000))) +
  theme_bw() +
  ylab("Longitude") +
  xlab("Latitude") +
  labs(fill = "Pooled fit (%)")


# where do the missing values come from? they come from infinite precipitation change values
# in the prediction data - need to change these in prediction to 0 

# the extremes are again around the Siwa desert

# aggregate to country predictions  --------------------

# get crop production rasters from script 05 line 74-81

# then aggregate crop_production_rasters to 0.5 0.5 
r <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, crs=4326, res=0.5)
res.factor <- raster::res(r)/raster::res(crop_production_rasters[[1]]) # 0.5 0.5 / 0.083 0.083 

crop_production_raster_agg <- lapply(1:4, function(i){
  raster::aggregate(crop_production_rasters[[i]], fact = res.factor, fun = sum)
}
)

predictions_level_2_country_abs <- lapply(1:4, function(j){ # crop
  lapply(1:4, function(l,j){ # time period
    
    prediction <- exactextractr::exact_extract(predictions_level_2_raster_abs[[j]][[l]], 
                                               worldmap_clean, 
                                               'weighted_mean', 
                                               weights = crop_production_raster_agg[[j]]) 
    
    data.frame(ADMIN=worldmap_clean@data$ADMIN,
               ISO_A2=worldmap_clean@data$ISO_A2,
               prediction=prediction)
    
  }, j) 
}) 


predictions_level_2_country_abs[[2]][[1]] %>% summarise(mean=mean(prediction,na.rm=TRUE))



predictions_level_2_country_pct <- lapply(1:4, function(j){ # crop
  lapply(1:4, function(l,j){ # time period
    
    prediction <- exactextractr::exact_extract(predictions_level_2_raster_pct[[j]][[l]], 
                                               worldmap_clean, 
                                               'weighted_mean', 
                                               weights = crop_production_raster_agg[[j]]) 
    
    data.frame(ADMIN=worldmap_clean@data$ADMIN,
               ISO_A2=worldmap_clean@data$ISO_A2,
               prediction=prediction)
    
  }, j) 
}) 

# why are these predictions expressed in 0.0 terms? seems to be /100, more similar to abs scale

predictions_level_2_country_pct[[2]][[1]] %>% summarise(mean=mean(prediction,na.rm=TRUE))

# compare against predictions_level_2_country[[2]][[1]] in script 17_plot_predictions
# even worse


# plot for single GCMs ----------------------------------------------------

# just do this for GCM=1 to see if patchiness is from pooling across GCMs or if patchiness is within GCM
# create percentage yield changes too
predictions_level_3_abs_all_coords <- lapply(1:4, function(j){ # crop
  lapply(1:4, function(l,j){ # time period
    lapply(1, function(i,l,j){# GCM
    predictions <- data.frame(lon = coords_countries$lon, lat = coords_countries$lat) %>% # script 13
      left_join(dplyr::select(
        predictions_level_3_abs[[j]][[l]][[i]], # j (crop), l (time period), i (GCM)
        fit_bar,
        v_w,
        v_b,
        se_p,
        lwr_p,
        upr_p,
        lon, 
        lat), 
        by = c("lon", "lat")) %>% 
      left_join(crop_yield_monf_dt[[j]], by=c("lon", "lat")) %>% 
      group_by(lon, lat) %>% 
      mutate(bs_yield=ifelse(bs_yield==0, 0.01, bs_yield), # otherwise have 30822 pixels with infinite values
             Yield.Change=(fit_bar/bs_yield)) }, l ,j)
  }, j) # time period
}) 


predictions_level_3_raster_pct <- lapply(1:4, function(j){ # crop
  lapply(1:4, function(l,j){ # time period
    lapply(1, function(i,l,j){
    xyz <- predictions_level_3_abs_all_coords[[j]][[l]][[i]][c(1:2,10)] # lon, lat, Yield Change 
    rasterFromXYZ(xyz) }, l,j)
  }, j) # time period
}) 


rasterVis::levelplot(predictions_level_3_raster_pct[[3]][[1]][[1]],  # [[j]][[l]]
                     at=seq(-100,100),
                     col.regions = rev(terrain.colors(10000)),
                     margin=FALSE)


# explore scale for precipitation change ----------------------------------

crop_imputed_data_restricted[[1]][[1]]%>% 
  ggplot(aes(x=Abs.Precipitation.Change)) +
  geom_histogram()

change_vars[[1]][[1]] %>% 
  ggplot(aes(x=Abs.Precipitation.Change)) +
  geom_histogram()

# these seem to have similar distributions and ranges

max(crop_imputed_data_restricted[[1]][[1]]$Abs.Precipitation.Change) # 641.0917

max(!is.na(change_vars[[1]][[1]]$Abs.Precipitation.Change)) # 1 ??

change_vars[[1]][[1]] %>% 
  filter(!is.na(Abs.Precipitation.Change)) %>% 
  summarise(max=max(Abs.Precipitation.Change)) # 419.6972

crop_imputed_data_restricted[[1]][[1]] %>% 
  filter(!is.na(Abs.Precipitation.Change)) %>% 
  summarise(max=max(Abs.Precipitation.Change)) # 641.0917
  
