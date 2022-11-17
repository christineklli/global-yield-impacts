
# read in predicted models
# crop_models[[model]][[crop]][[imputation]]; [[k]][[j]][[i]]
crop_models_abs <- readRDS(here("results", "models", "crop_models_abs.RData"))

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

predict_gridded_level_5_abs <- function(m,i,l,j,k){ # this order, together with the function below, 
  # ensures that the list goes k (model), j (crop), l (time period), i (GCM), m (imputed)
  gammit::predict_gamm(crop_models_abs[[k]][[j]][[m]],
                       prediction_data_complete_cases[[j]][[l]][[i]], 
                       #exclude = c("s(Country2_fact)", "s(Reference_fact)"), 
                       re_form = c("s(Country2_fact)"), 
                       keep_prediction_data = TRUE,
                       newdata.guaranteed = TRUE,
                       se.fit = TRUE 
  )
}

# run prediction mgcv::gam()----------------------------------------------------------

prediction_gridded_level_5_abs <- lapply(1, function(k){ # k = model spec : we want model 2
  lapply(1:4, function(j, k){ # j = crop 
    lapply(1:4, function(l, j, k){ # l = time period 
      lapply(1:23, function(i, l , j, k){ # i = GCM # new total excluding GCM=3
        lapply(1:5, predict_gridded_level_5_abs, i, l, j, k) # m = imputed
      }, l, j, k)
    }, j, k) 
  }, k)
}) 

# warning message that many factor levels were not in original fit; this makes sense because we only fit models
# on a limited subset of countries in the CGIAR data
saveRDS(prediction_gridded_level_5_abs, 
        here("results", "predictions", 
             "prediction_gridded_level_5_abs.RData")) # saved output from GCM=1:23
