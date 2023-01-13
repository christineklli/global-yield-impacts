
library(targets)

# Set target options:
tar_option_set(
  packages = c("tibble", "dplyr"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

get_data <- function(crop, time_period, gcm) {
  dplyr::tibble(x=runif(10), y=rnorm(10), crop=crop, time_period=time_period, gcm=gcm)
}

set.seed(1); dat <- replicate(64, dplyr::tibble(z=runif(10)), simplify=FALSE)
dat[[5]]$z <- dat[[5]]$z*2

list(
  tar_target(crops, paste0('crop_', c(1, 2, 3, 5))),
  tar_target(time_periods, paste0('time_', 1:4)),
  tar_target(gcms, paste0('gcm_', 1:4)),
  # tar_target(
  #   prediction_data,
  #   get_data(crops, time_periods, gcms),
  #   pattern=cross(crops, time_periods, gcms), iteration='list'
  # ),
  # tar_target(other_table, dat, iteration='list'),
  # tar_target(
  #   test,
  #   prediction_data %>% cbind(z=other_table),
  #   map(prediction_data, other_table), iteration='vector'
  #),
  tar_target(
    prediction_data,
    readRDS(newdata[[crops]][[time_periods]][[gcms]]),
    pattern=cross(crops, time_periods, gcms), iteration='list'
  ),
)

# 
# # run predictions and save in directories
# tar_target(crops, paste('crop', 1:5, '_')),
# tar_target(time_periods, paste('time', 1:4, '_')),
# tar_target(gcms, paste('gcm', 1:4, '_')),
# tar_target(prediction_data, 
#            # expression/function to create pred data
#            dplyr::tibble(crop=crops, time_period=time_period, ) # this only builds the current iteration
#            pattern=cross(crops, time_periods, gcms), iteration='list',
#            format='fst_tbl'
#           )


# possible way of doing this in a neater way without nested sapplys
list(crop=1:10, time=3:5, gcm=1:24) %>% 
  purrr::cross() %>% 
  lapply(function(i, j, k) {
    dat <- readRDS(newdata[[i]][[j]][[k]])
    pred <- list(spec=1:10, crop=3:5, imputation=1:24) %>% 
      purrr::cross() %>% 
      lapply(function(m, i, l) {
        m <- model[[m]][[l]][[i]]
        predict(m, dat)
      })
  })

predict_gridded_level_5 <- function(fit, newdata, outfile, ncores, model, crops, time_period, GCM, imputed){
    # show progress
  # progressr::with_progress({
  #p <- progressr::progressor(along = newdata) # for each model spec
  
  # Set future plan for parallelising
  ncores <- parallelly::availableCores(omit=1)
  future::plan(future::multisession, workers = 2)
  
  # create filepaths for prediction outfiles
  f <- future.apply::future_sapply(1:5, function(k){ # model spec ; 1:5 %>% map(function(i, j, k), c(1:4), c(1:4))?
    sapply(1:4, function(j){ # crop
      sapply(1:4, function(l){ # time period
        sapply(c(1:2,4:24), function(i){ # GCM - exclude BCC
          sapply(1:5, function(m){ # m
            # create outfile path 
            outfile <-  sprintf("processed/predictions/level5/%s/%s/%s/%s/%s/predictions.RData", 
                                model[k], crops[j], time_period[l], GCM[i], imputed[m])
            
            message("Creating prediction")
            
            # Create directory if it does not exists
            if(!dir.exists(dirname(outfile))) { 
              dir.create(path = dirname(outfile), recursive = TRUE)
            }
            
            # ONLY READ IN RELEVANT CROP-SPECIFIC NEWDATA HERE
            prediction_data <- readRDS(newdata[[j]][[l]][[i]]) 
            
            #   p(sprintf("x=%s", x)) # progress summary update
            
            out <- gammit::predict_gamm(fit[[k]][[j]][[m]], # this might be where the purrr:map should go?
                                        # need to match this up by  newdata crop index
                                        prediction_data, # read in data needs to go here
                                        re_form = c("s(Country2_fact)"), 
                                        keep_prediction_data = TRUE,
                                        newdata.guaranteed = TRUE,
                                        se.fit = TRUE)
            
            saveRDS(out, outfile) 
            outfile
          })
        })
      })
    })
    #  })
  }) 
  
  message("Prediction complete")
  v <- unlist(f)
  v
  #paths
  
}


# # pool across m

pool_across_m <- function(data, model, crops, time_period, GCM, imputed, ncores){
  
  
  paths <- sapply(1:4, function(k){ # k = model spec
    sapply(1:4, function(j){ # j = crop
      sapply(1:4, function(l){ # l = time period
        sapply(1:23, function(i){ # i = GCM
          sapply(1:5, function(m){ # m = imputation
            
            # create outfile path 
            
            outfile <-  sprintf("processed/predictions/level4/%s/%s/%s/%s/predictions.RData", 
                                model[k], crops[j], time_period[l], GCM[i])
            
            message("Pooling m predictions")
            
            # Create directory if it does not exists
            if(!dir.exists(dirname(outfile))) { # safer to do this
              dir.create(path = dirname(outfile), recursive = TRUE)
            }
            
            # select element from vector of filenames corresponding to the model/crop/time/GCM/m
            infile <- data[str_detect(data, sprintf("level5/%s/%s/%s/%s/%s/predictions.RData",
                                                    model[k], crops[j], time_period[l], GCM[i], imputed[m]))] 
            
            predictions <- readRDS(here("infile"))
            
            m <- 5 # note this cannot be same as idcol name! otherwise where m is computed will not work/collapse
            
            # read in level 5 prediction data
            out <- predictions %>%
              rbindlist(., idcol="imp") %>%  # this is equivalent to unnesting nested tibble of m prediction dfs
              group_by(lon, lat) %>%
              summarise(fit_bar = mean(prediction.fit, na.rm=TRUE), # pooled mean predictions
                        v_w     = mean(prediction.se.fit^2, na.rm=TRUE), # Rubin's Rules for pooling within-m variance
                        v_b     = sum(prediction.fit - fit_bar)^2 / (m - 1), # pooling between-m variance
                        v_p     = v_w + v_b * (1 + (1 / m)), # overall variance
                        se_p    = sqrt(v_p)) %>% # standard error
              # use the p_suffix to indicate these are pooled
              mutate(lwr_p = fit_bar - se_p * 1.96, # assuming normally distributed imputations
                     upr_p = fit_bar + se_p * 1.96)
            
            saveRDS(out, outfile) 
            outfile
            
          })
        })
      }) 
    })
  }) %>% 
    unlist()
  
  message("Pooling complete")
  paths
}
# 
# # relist level 4
# 
# relist_predictions_level_4 <- function(predictions_1, 
#                                predictions_2, 
#                                predictions_3, 
#                                predictions_4){
#   list(
#     predictions_1, # takes model k and 'crop' 1
#     predictions_2,
#     predictions_3,
#     predictions_4)
# }
# 
# 
# # pool across GCM
# 
# pool_gcm_predictions <- function(predictions, spec_no){
#   
#   lapply((spec_no), function(k){
#     lapply(1:4, function(j, k){ # crop
#       lapply(1:4, function(l, j, k){ # time period
#         
#         gcm <- 23 # note this cannot be same as idcol name! otherwise where m is computed will not work/collapse
#         
#         predictions[[k]][[j]][[1]][[l]] %>% # model spec; # j = crop; l = time period
#           rbindlist(., idcol="g_c_m") %>%  # this is equivalent to unnesting nested tibble of m prediction dfs 
#           dplyr::select(!c(v_w, v_b, v_p, lwr_p, upr_p)) %>% 
#           group_by(lon, lat, .groups ='drop') %>% 
#           summarise(pred_bar = mean(fit_bar, na.rm=TRUE), # pooled mean predictions
#                     v_w     = mean(se_p^2, na.rm=TRUE), # Rubin's Rules for pooling within-m variance
#                     v_b     = sum(fit_bar - pred_bar)^2 / (gcm - 1), # pooling between-m variance
#                     v_p     = v_w + v_b * (1 + (1 / gcm)), # overall variance
#                     se_p    = sqrt(v_p)) %>% # standard error  
#           # use the p_suffix to indicate these are pooled
#           mutate(lwr_p = pred_bar - se_p * 1.96, # assuming normally distributed imputations
#                  upr_p = pred_bar + se_p * 1.96) 
#         
#       }, j, k) # time period
#     }, k) 
#   })
#   
# }
# 
# # ADD MODEL SPECS K index
# 
# # finally add coordinates for NAs
# 
# add_coords_to_predictions <- function(predictions, coords_countries){
#   
#   
#     lapply(1:4, function(j){ # j = crop 
#       lapply(1:4, function(l, j){ # l = time period 
#                    # for only complete cases, a subset of total pixels
#                    data.frame(lon = coords_countries$lon, lat = coords_countries$lat) %>%
#                      left_join(predictions[[j]][[l]], #j (crop), l (time period)
#                        by = c("lon", "lat"))
#         }, j) 
#     })
#   }
#   
# # calculate global mean predictions for each crop and time period
# 
# calculate_global_mean_predictions <- function(predictions){
#   
#   lapply(1:4, function(j){
#     lapply(1:4, function(l,j){
#       predictions[[j]][[l]] %>% 
#         summarise(mean=mean(pred_bar,na.rm=TRUE))
#     }, j)
#   })
# }
# 
# # this is just for lm predictions
# calculate_global_mean_predictions_rm_inf <- function(predictions){
#   
#   lapply(1:4, function(j){
#     lapply(1:4, function(l,j){
#       predictions[[j]][[l]] %>% 
#         filter(!is.infinite(pred_bar)) %>% 
#         summarise(mean=mean(pred_bar,na.rm=TRUE))
#     }, j)
#   })
# }  