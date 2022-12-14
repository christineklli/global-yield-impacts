
cross_vars <- function(data, crops, imputations){
  
  lapply(1:4, function(j){
    lapply(1:5, function(m){
      data[[j]][[m]] %>% 
        mutate(crop = crops[j],
               imputation=imputations[m]) %>% 
        dplyr::select(!c("crop_factor",
                         "Pct.Precipitation.Change",
                         "Yield.Level",
                         "Abs.Yield.Change"))
    })
  })
  
}

name_list <- function(x, name){names(x) <- name
x}


  
cross_vars_future <- function(data, crops, time_periods, GCMs){
  lapply(1:4, function(j){
    lapply(1:4, function(l){
      lapply(c(1:2,4:24), function(i){ # THIS HAS CHANGED
        data[[j]][[l]][[i]] %>% 
          mutate(crop=crops[j],
                 time_period=time_periods[l],
                 gcm=GCMs[i]) %>% 
          as_tibble()
      })
    })
  })
}

flattenlist <- function(x){  
  morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){ 
    Recall(out)
  }else{
    return(out)
  }
}
# 
# rbind_predictions <- function(predictions){
#   predictions %>% 
#   rbindlist(.)
# }
# 
# pool_across_m_then_gcm <- function(predictions, coords_countries){
#   
#   imp <- 5 # note this cannot be same as idcol name! otherwise where m is computed will not work/collapse
#   gcm_no <- 23
#   
#   # read in level 5 prediction data
#   predictions %>%
#     #rbindlist(.) %>%  # this is equivalent to unnesting nested tibble of m prediction dfs
#     group_by(lon, lat, crop, model_spec, time_period, gcm) %>% # to identify iteration
#     summarise(fit_bar = mean(prediction.fit, na.rm=TRUE), # pooled mean predictions
#               v_w     = mean(prediction.se.fit^2, na.rm=TRUE), # Rubin's Rules for pooling within-m variance
#               v_b     = sum(prediction.fit - fit_bar)^2 / (imp - 1), # pooling between-m variance
#               v_p     = v_w + v_b * (1 + (1 / imp)), # overall variance
#               se_p    = sqrt(v_p)) %>% # standard error
#     # use the p_suffix to indicate these are pooled
#     mutate(lwr_p = fit_bar - se_p * 1.96, # assuming normally distributed imputations
#            upr_p = fit_bar + se_p * 1.96) %>% 
#     dplyr::select(!c(v_w, v_b, v_p, lwr_p, upr_p)) %>%
#     group_by(lon, lat, crop, model_spec, time_period, .groups ='drop') %>%
#     summarise(pred_bar = mean(fit_bar, na.rm=TRUE), # pooled mean predictions
#               v_w     = mean(se_p^2, na.rm=TRUE), # Rubin's Rules for pooling within-m variance
#               v_b     = sum(fit_bar - pred_bar)^2 / (gcm_no - 1), # pooling between-m variance
#               v_p     = v_w + v_b * (1 + (1 / gcm_no)), # overall variance
#               se_p    = sqrt(v_p)) %>% # standard error
#     # use the p_suffix to indicate these are pooled
#     mutate(lwr_p = pred_bar - se_p * 1.96, # assuming normally distributed imputations
#            upr_p = pred_bar + se_p * 1.96) %>% 
#     # add coords
#     left_join(data.frame(lon = coords_countries$lon, 
#                          lat = coords_countries$lat), #j (crop), l (time period)
#               by = c("lon", "lat"))
#     
#   
# }

predict_level_5 <- function(fit, data, ncores){
  future::plan(future::multisession, workers = ncores)
  
  p <- future.apply::future_lapply(1:5, function(i) { # imputation
    gammit::predict_gamm(fit[[1]][[i]], # 5 of these with 5 elements each (that we then rbind)
                         data[[1]], # only 4 of these with length 1405484 (23 gcm x 60k rows)
                         re_form = c("s(Country2_fact)"),
                         keep_prediction_data = TRUE,
                         newdata.guaranteed = TRUE,
                         se.fit = TRUE) %>% 
      mutate(model_spec=attr(fit[[1]][[i]], "model_specs"),
             m=attr(fit[[1]][[i]], "imputation")) 
    
    
  })
    rbindlist(p)
}


pool_across_m <- function(predictions){
  
  imp <- 5 # note this cannot be same as idcol name! otherwise where m is computed will not work/collapse
  
  # read in level 5 prediction data
  predictions %>%
    #rbindlist(.) %>%  # this is equivalent to unnesting nested tibble of m prediction dfs
    group_by(lon, lat, crop_pooled, model_spec, time_period, gcm) %>% # to identify iteration
    summarise(fit_bar = mean(prediction.fit, na.rm=TRUE), # pooled mean predictions
              v_w     = mean(prediction.se.fit^2, na.rm=TRUE), # Rubin's Rules for pooling within-m variance
              v_b     = sum(prediction.fit - fit_bar)^2 / (imp - 1), # pooling between-m variance
              v_p     = v_w + v_b * (1 + (1 / imp)), # overall variance
              se_p    = sqrt(v_p)) %>% # standard error
    # use the p_suffix to indicate these are pooled
    mutate(lwr_p = fit_bar - se_p * 1.96, # assuming normally distributed imputations
           upr_p = fit_bar + se_p * 1.96)
}

pool_across_gcm <- function(predictions, coords_countries){
  
  gcm_no <- 23 # note this cannot be same as idcol name! otherwise where m is computed will not work/collapse

        predictions %>% # this is equivalent to unnesting nested tibble of m prediction dfs
          ungroup() %>% 
          dplyr::select(!c(v_w, v_b, v_p, lwr_p, upr_p)) %>%
          group_by(lon, lat, crop_pooled, model_spec, time_period) %>%
          summarise(pred_bar = mean(fit_bar, na.rm=TRUE), # pooled mean predictions
                    v_w     = mean(se_p^2, na.rm=TRUE), # Rubin's Rules for pooling within-m variance
                    v_b     = sum(fit_bar - pred_bar)^2 / (gcm_no - 1), # pooling between-m variance
                    v_p     = v_w + v_b * (1 + (1 / gcm_no)), # overall variance
                    se_p    = sqrt(v_p)) %>% # standard error
          # use the p_suffix to indicate these are pooled
          mutate(lwr_p = pred_bar - se_p * 1.96, # assuming normally distributed imputations
                 upr_p = pred_bar + se_p * 1.96) %>% 
          # add coords
          left_join(data.frame(lon = coords_countries$lon, 
                               lat = coords_countries$lat), #j (crop), l (time period)
                                 by = c("lon", "lat"))
}

      
# 
# group_pool_across_m <- function(predictions){
#   
#   imp <- 5 # note this cannot be same as idcol name! otherwise where m is computed will not work/collapse
#   
#   # read in level 5 prediction data
#   predictions %>%
#     #rbindlist(.) %>%  # this is equivalent to unnesting nested tibble of m prediction dfs
#     #group_by(lon, lat, crop, model_spec, time_period, gcm) %>% # to identify iteration
#     summarise(fit_bar = mean(prediction.fit, na.rm=TRUE), # pooled mean predictions
#               v_w     = mean(prediction.se.fit^2, na.rm=TRUE), # Rubin's Rules for pooling within-m variance
#               v_b     = sum(prediction.fit - fit_bar)^2 / (imp - 1), # pooling between-m variance
#               v_p     = v_w + v_b * (1 + (1 / imp)), # overall variance
#               se_p    = sqrt(v_p)) %>% # standard error
#     # use the p_suffix to indicate these are pooled
#     mutate(lwr_p = fit_bar - se_p * 1.96, # assuming normally distributed imputations
#            upr_p = fit_bar + se_p * 1.96)
# }
# 
# 
# group_pool_across_gcm <- function(predictions, coords_countries){
#   
#   gcm_no <- 23 # note this cannot be same as idcol name! otherwise where m is computed will not work/collapse
#   
#   predictions %>% # this is equivalent to unnesting nested tibble of m prediction dfs
#     #dplyr::select(!c(v_w, v_b, v_p, lwr_p, upr_p)) %>%
#     #group_by(lon, lat, crop, model_spec, time_period, .groups ='drop') %>%
#     summarise(pred_bar = mean(fit_bar, na.rm=TRUE), # pooled mean predictions
#               v_w     = mean(se_p^2, na.rm=TRUE), # Rubin's Rules for pooling within-m variance
#               v_b     = sum(fit_bar - pred_bar)^2 / (gcm_no - 1), # pooling between-m variance
#               v_p     = v_w + v_b * (1 + (1 / gcm_no)), # overall variance
#               se_p    = sqrt(v_p)) %>% # standard error
#     # use the p_suffix to indicate these are pooled
#     mutate(lwr_p = pred_bar - se_p * 1.96, # assuming normally distributed imputations
#            upr_p = pred_bar + se_p * 1.96) %>% 
#     # add coords
#     left_join(data.frame(lon = coords_countries$lon, 
#                          lat = coords_countries$lat), #j (crop), l (time period)
#               by = c("lon", "lat"))
# }

  

# 
# 
# read_in_nested_data <- function(path){
#   crops <- list.dirs(path=here("processed/prediction_data"), recursive=F, full.names=F)
#   
#   lapply(crops,function(dir) {
#     time_periods <- list.dirs(path=here(sprintf("processed/prediction_data/%s", dir)),
#                               recursive=FALSE,full.names=FALSE)
#     time_periods_list <- lapply(time_periods,function(dir2) {
#       list.files(path=here(sprintf("processed/prediction_data/%s/%s", dir, dir2)), full.names=T)
#     })
#     time_periods_list
#   })
# }
# 
# # crops <- list.dirs(path=here("processed/prediction_data"), recursive=F, full.names=F)
# # 
# # prediction_data_nested_list <- lapply(crops,function(dir) {
# #   time_periods <- list.dirs(path=here(sprintf("processed/prediction_data/%s", dir)),
# #                             recursive=FALSE,full.names=FALSE)
# #   time_periods_list <- lapply(time_periods,function(dir2) {
# #     list.files(path=here(sprintf("processed/prediction_data/%s/%s", dir, dir2)), full.names=T)
# #   })
# #   time_periods_list
# # })
# # 
# 
# # run predictions and save in directories
# 
# predict_gridded_level_5 <- function(fit, newdata, outfile, ncores, model, crops, time_period, GCM, imputed){
#   
#   # show progress
#   # progressr::with_progress({
#   #p <- progressr::progressor(along = newdata) # for each model spec
#   
#   # Set future plan for parallelising
#   #ncores <- parallelly::availableCores(omit=1)
#   future::plan(future::multisession, workers = ncores)
#   
#   # create filepaths for prediction outfiles
#   f <- future.apply::future_sapply(1:5, function(k){ # model spec 1:5
#     sapply(1:4, function(j){ # crop 1:4
#       sapply(1:4, function(l){ # time period 1:4
#         sapply(c(1:2,4:24), function(i){ # GCM - exclude BCC c(1:2, 4:24)
#           sapply(1:5, function(m){ # m c(1:5)
#             # create outfile path 
#             outfile <-  here(sprintf("processed/predictions/level5/%s/%s/%s/%s/%s/predictions.RData", 
#                                      model[k], crops[j], time_period[l], GCM[i], imputed[m]))
#             
#             sink("log_predictions_gridded_level_5.txt", append=TRUE)
#             cat(paste("Creating prediction", k, j, l, i, m, "\n", sep=","))
#             sink()
#             
#             # Create directory if it does not exists
#             if(!dir.exists(dirname(outfile))) { 
#               dir.create(path = dirname(outfile), recursive = TRUE)
#             }
#             
#             # ONLY READ IN RELEVANT CROP-SPECIFIC NEWDATA HERE
#             # might be better just to cache this prediction data instead of reading in
#             # note if reading in prediction data as nested list there are 25
#             # these do not match up with the 24 in the GCM vector; there is a penultimate NA GCM as well
#             # however cached prediction_data_complete_cases contains same 24 as in the GCM vector
#             #prediction_data <- readRDS(newdata[[j]][[l]][[i]]) 
#             
#             #   p(sprintf("x=%s", x)) # progress summary update
#             
#             out <- gammit::predict_gamm(fit[[k]][[j]][[m]], 
#                                         # need to match this up by  newdata crop index
#                                         newdata[[j]][[l]][[i]], # cached data; if reading in from file, prediction_data
#                                         re_form = c("s(Country2_fact)"), 
#                                         keep_prediction_data = TRUE,
#                                         newdata.guaranteed = TRUE,
#                                         se.fit = TRUE)
#             
#             saveRDS(out, outfile) 
#             outfile
#           })
#         })
#       })
#     })
#     #  })
#   }) 
#   
#   message("Prediction complete")
#   v <- unlist(f)
#   v
#   #paths
#   
# }
# 
# # pool across m
# # 
# # pool_across_m <- function(data, model, crops, time_period, GCM, imputed, ncores){
# #   
# #   # Set future plan for parallelising
# #   future::plan(future::multisession, workers = ncores)
# #   
# #   # create filepaths for prediction outfiles
# #   f <- future.apply::future_sapply(1:5, function(k){ # model spec
# #     sapply(1:4, function(j){ # j = crop
# #       sapply(1:4, function(l){ # l = time period
# #         sapply(1:23, function(i){ # i = GCM
# #           sapply(1:5, function(m){ # m = imputation
# #             
# #             # create outfile path 
# #             
# #             outfile <-  here(sprintf("processed/predictions/level4/%s/%s/%s/%s/predictions.RData", 
# #                                      model[k], crops[j], time_period[l], GCM[i]))
# #             
# #             sink("log_predictions_gridded_level_4.txt", append=TRUE)
# #             cat(paste("Pooling predictions", k, j, l, i, m, "\n", sep=","))
# #             
# #             # Create directory if it does not exists
# #             #if(!dir.exists(dirname(outfile))) { # safer to do this
# #             #  dir.create(path = dirname(outfile), recursive = TRUE)
# #             #}
# #             
# #             # select element from vector of filenames corresponding to the model/crop/time/GCM/m
# #             infile <- data[str_detect(data, sprintf("level5/%s/%s/%s/%s/%s/predictions.RData",
# #                                                     model[k], crops[j], time_period[l], GCM[i], imputed[m]))] 
# #             
# #             predictions <- readRDS(here(infile))
# #             
# #             m <- 5 # note this cannot be same as idcol name! otherwise where m is computed will not work/collapse
# #             
# #             # read in level 5 prediction data
# #             out <- predictions %>%
# #               rbindlist(., idcol="imp") %>%  # this is equivalent to unnesting nested tibble of m prediction dfs
# #               group_by(lon, lat) %>%
# #               summarise(fit_bar = mean(prediction.fit, na.rm=TRUE), # pooled mean predictions
# #                         v_w     = mean(prediction.se.fit^2, na.rm=TRUE), # Rubin's Rules for pooling within-m variance
# #                         v_b     = sum(prediction.fit - fit_bar)^2 / (m - 1), # pooling between-m variance
# #                         v_p     = v_w + v_b * (1 + (1 / m)), # overall variance
# #                         se_p    = sqrt(v_p)) %>% # standard error
# #               # use the p_suffix to indicate these are pooled
# #               mutate(lwr_p = fit_bar - se_p * 1.96, # assuming normally distributed imputations
# #                      upr_p = fit_bar + se_p * 1.96)
# #             
# #             saveRDS(out, outfile) 
# #             outfile
# #             
# #           })
# #         })
# #       }) 
# #     })
# #   }) 
# #   
# #   message("Pooling complete")
# #   v <- unlist(f)
# #   v
# # }
# # 
# # # relist level 4
# # 
# # relist_predictions_level_4 <- function(predictions_1, 
# #                                predictions_2, 
# #                                predictions_3, 
# #                                predictions_4){
# #   list(
# #     predictions_1, # takes model k and 'crop' 1
# #     predictions_2,
# #     predictions_3,
# #     predictions_4)
# # }
# # 
# # 
# # # pool across GCM
# # 
# # pool_gcm_predictions <- function(predictions, spec_no){
# #   
# #   lapply((spec_no), function(k){
# #     lapply(1:4, function(j, k){ # crop
# #       lapply(1:4, function(l, j, k){ # time period
# #         
# #         gcm <- 23 # note this cannot be same as idcol name! otherwise where m is computed will not work/collapse
# #         
# #         predictions[[k]][[j]][[1]][[l]] %>% # model spec; # j = crop; l = time period
# #           rbindlist(., idcol="g_c_m") %>%  # this is equivalent to unnesting nested tibble of m prediction dfs 
# #           dplyr::select(!c(v_w, v_b, v_p, lwr_p, upr_p)) %>% 
# #           group_by(lon, lat, .groups ='drop') %>% 
# #           summarise(pred_bar = mean(fit_bar, na.rm=TRUE), # pooled mean predictions
# #                     v_w     = mean(se_p^2, na.rm=TRUE), # Rubin's Rules for pooling within-m variance
# #                     v_b     = sum(fit_bar - pred_bar)^2 / (gcm - 1), # pooling between-m variance
# #                     v_p     = v_w + v_b * (1 + (1 / gcm)), # overall variance
# #                     se_p    = sqrt(v_p)) %>% # standard error  
# #           # use the p_suffix to indicate these are pooled
# #           mutate(lwr_p = pred_bar - se_p * 1.96, # assuming normally distributed imputations
# #                  upr_p = pred_bar + se_p * 1.96) 
# #         
# #       }, j, k) # time period
# #     }, k) 
# #   })
# #   
# # }
# # 
# # # ADD MODEL SPECS K index
# # 
# # # finally add coordinates for NAs
# # 
# # add_coords_to_predictions <- function(predictions, coords_countries){
# #   
# #   
# #     lapply(1:4, function(j){ # j = crop 
# #       lapply(1:4, function(l, j){ # l = time period 
# #                    # for only complete cases, a subset of total pixels
# #                    data.frame(lon = coords_countries$lon, lat = coords_countries$lat) %>%
# #                      left_join(predictions[[j]][[l]], #j (crop), l (time period)
# #                        by = c("lon", "lat"))
# #         }, j) 
# #     })
# #   }
# #   
# # # calculate global mean predictions for each crop and time period
# # 
# # calculate_global_mean_predictions <- function(predictions){
# #   
# #   lapply(1:4, function(j){
# #     lapply(1:4, function(l,j){
# #       predictions[[j]][[l]] %>% 
# #         summarise(mean=mean(pred_bar,na.rm=TRUE))
# #     }, j)
# #   })
# # }
# # 
# # # this is just for lm predictions
# # calculate_global_mean_predictions_rm_inf <- function(predictions){
# #   
# #   lapply(1:4, function(j){
# #     lapply(1:4, function(l,j){
# #       predictions[[j]][[l]] %>% 
# #         filter(!is.infinite(pred_bar)) %>% 
# #         summarise(mean=mean(pred_bar,na.rm=TRUE))
# #     }, j)
# #   })
# # }  