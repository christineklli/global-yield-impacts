
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

        predictions %>% 
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

# investigate outliers
# 
# outliers <- tar_read(predictions_by_time_period) %>% 
#   group_by(model_spec, crop_pooled, time_period) %>% 
#   summarise(min=min(pred_bar,na.rm=TRUE)) %>% 
#   mutate(outlier=1)
#   #print(n=Inf)
# tar_read(predictions_by_time_period) %>% 
#   left_join(outliers, by=c("pred_bar"="min", "model_spec", "crop_pooled", "time_period")) %>% 
#   filter(outlier==1) %>% 
#   print(n=Inf) 



# only issue with this method is it will not catch all outliers, only the min per group
      # identified as the Siwa desert
# need to define a cut off threshold to make them NA