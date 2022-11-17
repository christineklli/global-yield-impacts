

# complete cases data

clean_prediction_data <- function(data){
  lapply(1:4, function(k){ # crops = k - highest level in three-level list; 1:4
  lapply(1:4, function(j, k){ # time period = j; 1:4
    lapply(1:24, function(i,j,k){ # GCM = i; 1:24
      data[[k]][[j]][[i]] %>% 
        #dplyr::select(!c(Baseline_yield)) %>% # is this what is causing no complete cases?
        #mutate(ncell=row_number()) %>% # this is needed to keep track of which pixels the complete cases are, for rasterising predictions later
        filter(complete.cases(.)) # na.omit or drop_na also works 
    }, j, k) 
  }, k)}) 
}

# run predictions

predict_gridded_level_5 <- function(models, data){
  
lapply(1, function(k){ # k = model spec
  lapply(1:4, function(j, k){ # j = crop 
    lapply(1:4, function(l, j, k){ # l = time period 
      lapply(1:23, function(i, l , j, k){ # i = GCM
        lapply(1:5, function(m,i,l,j,k){ # this order, together with the function below, 
                 # ensures that the list goes k (model), j (crop), l (time period), i (GCM), m (imputed)
                 gammit::predict_gamm(models[[k]][[j]][[m]],
                                      data[[j]][[l]][[i]], 
                                      #exclude = c("s(Country2_fact)", "s(Reference_fact)"), 
                                      re_form = c("s(Country2_fact)"), 
                                      keep_prediction_data = TRUE,
                                      newdata.guaranteed = TRUE,
                                      se.fit = TRUE)
                 }, i, l, j, k) # m = imputed
        }, l, j, k)
      }, j, k) 
    }, k)
  }) 
  
}

# pool across m

pool_across_m <- function(predictions, crop_no){
  
  lapply(1, function(k){ # there's only 1 model spec now
    lapply(crop_no, function(j, k){ # j = crop MAIZE
      lapply(1:4, function(l, j, k){ # l = time period 
        lapply(1:23, function(i, l , j, k){ 
                 
                 m <- 5 # note this cannot be same as idcol name! otherwise where m is computed will not work/collapse
                 
                 predictions[[k]][[j]][[l]][[i]] %>% 
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
                 
               }, l, j, k)
      }, j, k) 
    }, k)
  }) 
}

relist_predictions <- function(predictions_1, predictions_2, predictions_3, predictions_4){
                            list(
                            predictions_1[[1]][[1]], # takes model 1 and 'crop' 1
                            predictions_2[[1]][[1]],
                            predictions_3[[1]][[1]],
                            predictions_4[[1]][[1]])
}
  



# pool across GCM

pool_gcm_predictions <- function(predictions){
  
  lapply(1:4, function(j){ # crop
    lapply(1:4, function(l, j){ 
  
  gcm <- 23 # note this cannot be same as idcol name! otherwise where m is computed will not work/collapse
  
  predictions[[j]][[l]] %>% # j = crop; l = time period
    rbindlist(., idcol="g_c_m") %>%  # this is equivalent to unnesting nested tibble of m prediction dfs 
    dplyr::select(!c(v_w, v_b, v_p, lwr_p, upr_p)) %>% 
    group_by(lon, lat, .groups ='drop') %>% 
    summarise(pred_bar = mean(fit_bar, na.rm=TRUE), # pooled mean predictions
              v_w     = mean(se_p^2, na.rm=TRUE), # Rubin's Rules for pooling within-m variance
              v_b     = sum(fit_bar - pred_bar)^2 / (gcm - 1), # pooling between-m variance
              v_p     = v_w + v_b * (1 + (1 / gcm)), # overall variance
              se_p    = sqrt(v_p)) %>% # standard error  
    # use the p_suffix to indicate these are pooled
    mutate(lwr_p = pred_bar - se_p * 1.96, # assuming normally distributed imputations
           upr_p = pred_bar + se_p * 1.96) 
  
  }, j) # time period
  }) 
}


# finally add coordinates for NAs

add_coords_to_predictions <- function(predictions, coords_countries){
  
  
    lapply(1:4, function(j){ # j = crop 
      lapply(1:4, function(l, j){ # l = time period 
                   # for only complete cases, a subset of total pixels
                   data.frame(lon = coords_countries$lon, lat = coords_countries$lat) %>%
                     left_join(predictions[[j]][[l]], #j (crop), l (time period)
                       by = c("lon", "lat"))}, j) 
    })
  }
  
# calculate global mean predictions for each crop and time period

calculate_global_mean_predictions <- function(predictions){
  
  lapply(1:4, function(j){
    lapply(1:4, function(l,j){
      predictions[[j]][[l]] %>% 
        summarise(mean=mean(pred_bar,na.rm=TRUE))
    }, j)
  })
}

calculate_global_mean_predictions_rm_inf <- function(predictions){
  
  lapply(1:4, function(j){
    lapply(1:4, function(l,j){
      predictions[[j]][[l]] %>% 
        filter(!is.infinite(pred_bar)) %>% 
        summarise(mean=mean(pred_bar,na.rm=TRUE))
    }, j)
  })
}  