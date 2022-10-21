
# read in pooled predictions ----------------------------------------------

predictions_level_2 <- readRDS(here("results",
                                  "predictions",
                                  "predictions_level_2.RData"))


# global mean -------------------------------------------------------------

predictions_level_2[[4]][[4]] %>% ungroup() %>% summarise(mean=mean(pred_bar,na.rm=TRUE))

# rasterise ---------------------------------------------------------------

predictions_level_2_raster <- lapply(1:4, function(j){ # crop
  lapply(1:4, function(l,j){ # time period
    xyz <- predictions_level_2[[j]][[l]][c(1:2,4)] # lon, lat, pred_bar 
    rasterFromXYZ(xyz) 
  }, j) # time period
}) 

# plot gridded --------------------------------------------------------------------

# vary this manually, dynamically to plot individual plot
rasterVis::levelplot(predictions_level_2_raster[[4]][[4]],  # [[j]][[l]]
                     at=seq(-100,100),
                     col.regions = rev(terrain.colors(10000)),
                     margin=FALSE)

plot(predictions_level_2_raster[[4]][[4]])

predictions_level_2[[2]][[3]] %>% 
  filter(pred_bar > -50) %>% 
  ggplot() +
  geom_tile(aes(x=lon, y=lat, fill = pred_bar)) +
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

predictions_level_2_country <- lapply(1:4, function(j){ # crop
  lapply(1:4, function(l,j){ # time period
    
    prediction <- exactextractr::exact_extract(predictions_level_2_raster[[j]][[l]], 
                                               worldmap_clean, 
                                               'weighted_mean', 
                                               weights = crop_production_raster_agg[[j]]) 
    
    data.frame(ADMIN=worldmap_clean@data$ADMIN,
               ISO_A2=worldmap_clean@data$ISO_A2,
               prediction=prediction)
    
  }, j) 
}) 


predictions_level_2_country[[2]][[3]] %>% summarise(mean=mean(prediction,na.rm=TRUE))

