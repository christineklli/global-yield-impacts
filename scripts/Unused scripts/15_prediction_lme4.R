

# run predictions on lmer crop models -------------------------------------

predict_gridded_level_5_lmer <- function(m,i,l,j,k){ # this order, together with the function below, 
  # ensures that the list goes k (model), j (crop), l (time period), i (GCM), m (imputed)
  predict(crop_models_lmer[[k]][[j]][[m]], # running predict.merMod under the hood
          prediction_data_complete_cases[[j]][[l]][[i]], 
          #exclude = c("s(Country2_fact)", "s(Reference_fact)"), 
          #re.form=~0, # this works if we exclude (1|Country2_fact) in crop_models_lmer in script 10-1
          re.form = ~(1|Country2_fact), # this doesn't work because country2_fact not present in original fit unless add country2_fact random intercepts to model
          #keep_prediction_data = TRUE,
          #newdata.guaranteed = TRUE,
          allow.new.levels=TRUE
  )
}

prediction_gridded_level_5_lmer <- lapply(1:2, function(k){ # k = model spec
  lapply(1:4, function(j, k){ # j = crop 
    lapply(1:4, function(l, j, k){ # l = time period 
      lapply(1, function(i, l , j, k){ # i = GCM
        lapply(1:5, predict_gridded_level_5_lmer, i, l, j, k) # m = imputed
      }, l, j, k)
    }, j, k) 
  }, k)
}) 


crop_imputed_data_restricted[[1]][[1]] %>% glimpse()
prediction_data_complete_cases[[2]][[2]][[1]] %>% glimpse()

unique(prediction_data_complete_cases[[2]][[2]][[5]]$Country2_fact)

saveRDS(prediction_gridded_level_5, here("prediction_gridded_level_5.RData")) # saved output from GCM=1

# rasterise this example prediction

# unfortunately lme4 does not provide option to keep prediction data
# so we have vectors of 60k+ predictions
# need to match this up to pixels 

# for only complete cases of lon and lat
predictions_lmer_coords <- data.frame(lon = prediction_data_complete_cases[[4]][[4]][[1]]$lon, # crop, time period, GCM
                                      lat = prediction_data_complete_cases[[4]][[4]][[1]]$lat, # need to now match prediction CROP as # NAs may differ across crops, diff order/place in index
                                      prediction = prediction_gridded_level_5_lmer[[1]][[4]][[4]][[1]][[1]]) #k (model), j (crop), l (time period), i (GCM), m (imputed))
# for full set of lon and lat  
predictions_lmer <- data.frame(lon=coords_countries$lon, 
                               lat=coords_countries$lat) %>% 
  left_join(predictions_lmer_coords, by = c("lon", "lat"))

raster_prediction_lmer <- rasterFromXYZ(predictions_lmer) 
# why are wheat reductions consistently larger than maize?
# only indication from response functions is for precipitation-change varying functions

rasterVis::levelplot(raster_prediction_lmer, 
                     at=seq(-100,100),
                     col.regions = rev(terrain.colors(10000)))


# limited spatial variation comes from the very low spatial variation in temp.change and precipitation.change in the prediction data
# go back to script 14 line 138 to see these plots
# though note this is a format thing; if we restrict range to [-100,100] that goes away

# wheat vs maize with these nested RE models seems to make more sense than the crossed RE models - less stark differences

# function to rasterise all predictions and plot  --------

add_coords_to_predictions <- function(m,i,l,j,k){
  # for only complete cases, a subset of total pixels
  a <- data.frame(
    lon = prediction_data_complete_cases[[j]][[l]][[i]]$lon, # crop, time period, GCM
    lat = prediction_data_complete_cases[[j]][[l]][[i]]$lat, # need to now match prediction CROP as # NAs may differ across crops, diff order/place in index
    prediction = prediction_gridded_level_5_lmer[[k]][[j]][[l]][[i]][[m]]) #k (model), j (crop), l (time period), i (GCM), m (imputed))
  # for full set of lon and lat  
  data.frame(lon=coords_countries$lon, 
             lat=coords_countries$lat) %>% 
    left_join(a, by = c("lon", "lat"))}

predictions_lmer_coords <- lapply(1:2, function(k){ # k = model spec
  lapply(1:4, function(j, k){ # j = crop 
    lapply(1:4, function(l, j, k){ # l = time period 
      lapply(1, function(i, l , j, k){ # i = GCM
        lapply(1:5, add_coords_to_predictions, i, l, j, k) # m = imputed
      }, l, j, k)
    }, j, k) 
  }, k)
}) 

saveRDS(predictions_lmer_coords, here("results", "predictions_lmer_coords.RData"))

predictions_lmer_raster <- lapply(1:2, function(k){ # k = model spec
  lapply(1:4, function(j, k){ # j = crop 
    lapply(1:4, function(l, j, k){ # l = time period 
      lapply(1, function(i, l , j, k){ # i = GCM
        lapply(1:5, function(m,i,l,j,k){
          rasterFromXYZ(predictions_lmer_coords[[k]][[j]][[l]][[i]][[m]])
        }, i, l, j, k) # m = imputed
      }, l, j, k)
    }, j, k) 
  }, k)
}) 

# vary this manually, dynamically to plot individual plot
rasterVis::levelplot(predictions_lmer_raster[[1]][[3]][[4]][[1]][[4]],  # [[k]][[j]][[l]][[i]][[m]]
                     at=seq(-100,100),
                     col.regions = rev(terrain.colors(10000)))


# aggregate to country predictions - using exactextractr --------------------

# get crop production rasters from script 05 line 74-81
# crop_production_rasters
# then aggregate crop_production_rasters to 0.5 0.5 
r <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, crs=4326, res=0.5)
res.factor <- raster::res(r)/raster::res(crop_production_rasters[[1]]) # 0.5 0.5 / 0.083 0.083 

crop_production_raster_agg <- lapply(1:4, function(i){
  raster::aggregate(crop_production_rasters[[i]], fact = res.factor, fun = sum)
}
)

# rename these and then rerun
predictions_country_method2 <- lapply(1, function(k){ # k = model spec
  lapply(2, function(j, k){ # j = crop 
    lapply(4, function(l, j, k){ # l = time period 
      lapply(1, function(i, l , j, k){ # i = GCM
        lapply(1, function(m,i,l,j,k){ # m = imputed
          
          prediction <- exactextractr::exact_extract(predictions_lmer_raster[[k]][[j]][[l]][[i]][[m]], 
                                                     worldmap_clean, 
                                                     'weighted_mean', 
                                                     weights = crop_production_raster_agg[[j]]) 
          
          data.frame(ADMIN=worldmap_clean@data$ADMIN,
                     ISO_A2=worldmap_clean@data$ISO_A2,
                     prediction=prediction)
          
        }, i, l, j, k) 
      }, l, j, k)
    }, j, k) 
  }, k)
}) 

predictions_country_method2[[1]][[1]][[1]][[1]][[1]] %>% summarise(mean=mean(prediction,na.rm=TRUE))

saveRDS(predictions_country_method2, here("results","lme4_country_predictions.RData"))
# in time period 3 (2061-2080), global mean yield loss for maize, GCM=1, m=1, is 14%
# in time period 4 (2081-2100), global mean yield loss for maize, GCM=1, m=1, is 18%
# this is still significantly less severe than NASA results
# NASA results: maize results from 2069-2099, maize decrease by 24%

# in time period 3 (2061-2080), global mean yield loss for wheat, GCM=1, m=1, is 15%
# in time period 4 (2081-2100), global mean yield loss for wheat, GCM=1, m=1, is 16%
# this is much worse than NASA results
# though in 2021-2040, a handful of countries see very small increases in wheat yields
# NASA results: wheat results from 2069-2099, wheat increase by 17%

# for GCM=1,m=1, maize results are less severe than NASA for similar time periods BUT wheat is much worse

# rice results for Japan under time period 4 see 21% increase, this must be wrong!
# soybean sees a global mean 22% increase in time period 4 - wrong?! soybean results are wild

# aggregate to country predictions - using crop_production_dt --------


crop_production_dt <- lapply(1:4, function(i){
  raster::as.data.frame(crop_production_raster_agg[[i]], xy=TRUE) %>% 
    rename(lon=x,lat=y,production=3) %>% 
    as.data.table()
})

crop_production_dt[[1]] %>% summarise(sum=sum(production,na.rm=TRUE)) # 593m tons of maize globally back in 2000
# this is an underestimate compared to FAOSTAT of global maize production in 2000 (698m) by around 100m
# but roughly acceptable
predictions_country <- lapply(1, function(k){ # k = model spec
  lapply(1, function(j, k){ # j = crop 
    lapply(1, function(l, j, k){ # l = time period 
      lapply(1, function(i, l , j, k){ # i = GCM
        lapply(1, function(m,i,l,j,k){
          
          predictions_lmer_coords[[k]][[j]][[l]][[i]][[m]] %>% 
            left_join(coords_countries, by=c("lon","lat")) %>% 
            left_join(crop_production_dt[[j]], by=c("lon","lat")) %>% 
            mutate(weighted_sum = prediction*production) %>%  # perform weighted average
            group_by(ADMIN, ISO_A2) %>% 
            summarise(sum_weights=sum(production,na.rm=TRUE), 
                      sum_weighted_prediction=sum(weighted_sum,na.rm=TRUE),
                      country_prediction=sum_weighted_prediction/sum_weights) 
          
        }, i, l, j, k) # m = imputed
      }, l, j, k)
    }, j, k) 
  }, k)
}) 

saveRDS(predictions_country, here("results", "predictions_country.RData"))

predictions_country[[1]][[1]][[1]][[1]][[1]]


# compare aggregation to country methods ----------------------------------


predictions_country_method2[[1]][[1]][[1]][[1]][[1]] %>% 
  left_join(dplyr::select(predictions_country[[1]][[1]][[1]][[1]][[1]], ADMIN, ISO_A2, country_prediction),
            by=c("ADMIN", "ISO_A2"))
# this looks similar enough that any differences are likely down to exact::extractr getting the within-pixel coverage fractions


# mask production areas ---------------------------------------------------


