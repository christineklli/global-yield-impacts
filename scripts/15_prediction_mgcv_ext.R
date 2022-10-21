# (5) For each crop, predict each of 4 model specifications x 5 imputed datasets on 24 GCMs x 4 time periods
# Pooling order:
# (4) For each crop, and each of 4 model specifications, 24 GCMs and 4 time periods, POOL OVER IMPUTATIONS (m)
# take avg of predictions across 5 imputed models - for predicted mean, sd and 2.5 and 97.5 pct
# there are special formulas for taking avg across imputed models - see 14 predict gridded lines ~480
# (3) For each crop, 4 model specifications, and 4 time periods, POOL OVER GCMs (i)
# take avg, sd, 10th and 90th percentile of mean predictions across 24 GCMs - and avg of sd, 2.5 and 97.5 pct predictions
# (2) Evaluate all 4 model specifications, comparing GLMM and GAMM, and % Yield Change vs Abs Yield Change - POOL OVER/SELECT MODEL SPEC [k]
# (2) Pick one response model (% vs Abs) and either a) GLMM or GAMM, or b) ensemble GLMM + GAMM by pooling, maintain between model variance
# (2) End up with predictions[[crop]][[time period]] - a 2-level list
# Name each set of predictions at each level according to how many levels

# read in preidcted models
# crop_models[[model]][[crop]][[imputation]]; [[k]][[j]][[i]]
crop_models_ext <- readRDS(here("results", "models", "crop_models_ext.RData"))

# read in prediction data
# prediction_data[[crop]][[time period]][[GCM]]; [[k]][[j]][[i]] - need to rearrange such that align index for crops in both lists to [[j]][[k]][[i]]
prediction_data <- readRDS(here("processed", "prediction_data.RData"))

# j = crop, l = time period, i = GCM (prediction_data)
# k = model specification, j = crop, m = imputed dataset (crop_models)

# lapply over 5 different elements?!

# create prediction complete cases data -----------------------------------

prediction_data_complete_cases <- lapply(1:4, function(k){ # crops = k - highest level in three-level list; 1:4
  lapply(1:4, function(j, k){ # time period = j; 1:4
    lapply(c(1:2,4:24), function(i,j,k){ # GCM = i; 1:24 -> however we need to exclude GCM=3, BCC (wrong)
      prediction_data[[k]][[j]][[i]] %>% 
        #mutate(ncell=row_number()) %>% # this is needed to keep track of which pixels the complete cases are, for rasterising predictions later
        filter(complete.cases(.)) # na.omit or drop_na also works 
    }, j, k) 
  }, k)}) 

saveRDS(prediction_data_complete_cases, here("processed", "prediction_data_complete_cases.RData"))

prediction_data_complete_cases <- readRDS(here("processed", "prediction_data_complete_cases.RData"))

# prediction function mgcv::gam()-----------------------------------------------------

predict_gridded_level_5_ext <- function(m,i,l,j,k){ # this order, together with the function below, 
  # ensures that the list goes k (model), j (crop), l (time period), i (GCM), m (imputed)
  gammit::predict_gamm(crop_models_ext[[k]][[j]][[m]],
                       prediction_data_complete_cases[[j]][[l]][[i]], 
                       #exclude = c("s(Country2_fact)", "s(Reference_fact)"), 
                       re_form = c("s(Country2_fact)"), 
                       keep_prediction_data = TRUE,
                       newdata.guaranteed = TRUE,
                       se.fit = TRUE 
  )
}

# run prediction mgcv::gam()----------------------------------------------------------

prediction_gridded_level_5_ext <- lapply(1, function(k){ # k = model spec : we want model 2
  lapply(1:4, function(j, k){ # j = crop 
    lapply(1:4, function(l, j, k){ # l = time period 
      lapply(1:23, function(i, l , j, k){ # i = GCM # new total excluding GCM=3
        lapply(1:5, predict_gridded_level_5_ext, i, l, j, k) # m = imputed
      }, l, j, k)
    }, j, k) 
  }, k)
}) 

# warning message that many factor levels were not in original fit; this makes sense because we only fit models
# on a limited subset of countries in the CGIAR data
saveRDS(prediction_gridded_level_5_ext, 
        here("results", "predictions", 
             "prediction_gridded_level_5_ext.RData")) # saved output from GCM=1:23

# function to rasterise all predictions and plot  --------


add_coords_to_predictions_mgcv_ext <- function(m,i,l,j,k){
  # for only complete cases, a subset of total pixels
  predictions <- data.frame(lon = coords_countries$lon, lat = coords_countries$lat) %>%
    left_join(dplyr::select(
      prediction_gridded_level_5_ext[[k]][[j]][[l]][[i]][[m]], #k (model), j (crop), l (time period), i (GCM), m (imputed)
      prediction.fit, lon, lat), 
      by = c("lon", "lat"))
}

predictions_mgcv_coords_ext  <- lapply(1, function(k){ # there's only 1 model spec now
  lapply(1:4, function(j, k){ # j = crop 
    lapply(1:4, function(l, j, k){ # l = time period 
      lapply(1, function(i, l , j, k){ # i = GCM
        lapply(1:5, add_coords_to_predictions_mgcv_ext, i, l, j, k) # m = imputed
      }, l, j, k)
    }, j, k) 
  }, k)
}) 

saveRDS(predictions_mgcv_coords_ext, 
        here("results", "predictions", 
             "predictions_mgcv_coords_ext.RData"))

predictions_mgcv_raster_ext <- lapply(1, function(k){ # k = model spec
  lapply(1:4, function(j, k){ # j = crop 
    lapply(1:4, function(l, j, k){ # l = time period 
      lapply(1, function(i, l , j, k){ # i = GCM
        lapply(1:5, function(m,i,l,j,k){
          rasterFromXYZ(predictions_mgcv_coords_ext[[k]][[j]][[l]][[i]][[m]])
        }, i, l, j, k) # m = imputed
      }, l, j, k)
    }, j, k) 
  }, k)
}) 

# vary this manually, dynamically to plot individual plot
rasterVis::levelplot(predictions_mgcv_raster_ext[[1]][[3]][[3]][[1]][[5]],  # [[k]][[j]][[l]][[i]][[m]]
                     at=seq(-100,100),
                     col.regions = rev(terrain.colors(10000)))
# still looks really weird for soybean


# compare having random intercepts only vs random intercepts + random slopes
rasterVis::levelplot(predictions_mgcv_raster_slopes[[1]][[2]][[4]][[1]][[5]],  # [[k]][[j]][[l]][[i]][[m]]
                     at=seq(-100,100),
                     col.regions = rev(terrain.colors(10000)))
# on a gridded level it appears that soybean has positive results with random slopes?


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

predictions_country_mgcv_ext <- lapply(1, function(k){ # k = model spec
  lapply(4, function(j, k){ # j = crop 
    lapply(4, function(l, j, k){ # l = time period 
      lapply(1, function(i, l , j, k){ # i = GCM
        lapply(5, function(m,i,l,j,k){ # m = imputed
          
          prediction <- exactextractr::exact_extract(predictions_mgcv_raster_ext[[k]][[j]][[l]][[i]][[m]], 
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

predictions_country_mgcv_ext[[1]][[1]][[1]][[1]][[1]] %>% summarise(mean=mean(prediction,na.rm=TRUE))

# REDO THESE COMMENTS
# in time period 3 (2061-2080), global mean yield loss for maize, GCM=1, m=1, is 13%
# in time period 4 (2081-2100), global mean yield loss for maize, GCM=1, m=1, is 21%
# this is less severe than NASA results
# NASA results: maize results from 2069-2099, maize decrease by 24%

# in time period 3 (2061-2080), global mean yield loss for wheat, GCM=1, m=1, is 13%
# in time period 4 (2081-2100), global mean yield loss for wheat, GCM=1, m=1, is 20%
# this is much worse than NASA results
# though in 2021-2040, a handful of countries see very small increases in wheat yields
# NASA results: wheat results from 2069-2099, wheat increase by 17%

# time period 3, global mean yield loss for rice, GCM=1, m=1, -0.8%
# time period 4, -20%, japan sees gain of 5%

# time period 3, global mean yield gain for soy, 20%
# time period 4, gain 26%


# simple mean country average ---------------------------------------------

#  create simple mean country average predictions to see how much of wheat results could be explained 
# by fixed growing areas assumed under production-weighted averages; 
# therefore not accounting for growing range expansion


predictions_country_mgcv_simple <- lapply(1, function(k){ # k = model spec
  lapply(1, function(j, k){ # j = crop 
    lapply(4, function(l, j, k){ # l = time period 
      lapply(1, function(i, l , j, k){ # i = GCM
        lapply(1, function(m,i,l,j,k){ # m = imputed
          
          prediction <- exactextractr::exact_extract(predictions_mgcv_raster[[k]][[j]][[l]][[i]][[m]], 
                                                     worldmap_clean, 
                                                     'mean') 
          
          data.frame(ADMIN=worldmap_clean@data$ADMIN,
                     ISO_A2=worldmap_clean@data$ISO_A2,
                     prediction=prediction)
          
        }, i, l, j, k) 
      }, l, j, k)
    }, j, k) 
  }, k)
}) 

predictions_country_mgcv_simple[[1]][[1]][[1]][[1]][[1]] %>% summarise(mean=mean(prediction,na.rm=TRUE))
# wheat, time period 4, -37%, still very severe! but doesn't entirely discount range expansion hypothesis
# maize, time period 4, -25%

# mask production areas ---------------------------------------------------


