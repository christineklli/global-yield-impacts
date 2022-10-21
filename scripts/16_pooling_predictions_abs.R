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


# read in level 5 prediction data from 15_prediction_mgcv_ext.R ----------------------------------------
prediction_gridded_level_5_abs <- readRDS(here("results", 
                                    "predictions", 
                                    "prediction_gridded_level_5_abs.RData"))


# unlist then pool across m predictions -------------------------------------------------------------

# https://solomonkurz.netlify.app/post/2021-10-21-if-you-fit-a-model-with-multiply-imputed-data-you-can-still-plot-the-line/
# unlist level 5 across imputed predictions - rbind with idcol=m?
# group_by lon lat, summarise across imputed predictions

m <- 5 # note this cannot be same as idcol name! otherwise where m is computed will not work/collapse

pooling_m_predictions_abs <- function(i, l , j, k){ 
  
  prediction_gridded_level_5_abs[[k]][[j]][[l]][[i]] %>% 
    rbindlist(., idcol="imp") %>%  # this is equivalent to unnesting nested tibble of m prediction dfs 
    group_by(lon, lat, .groups = 'drop') %>% 
    summarise(fit_bar = mean(prediction.fit, na.rm=TRUE), # pooled mean predictions
              v_w     = mean(prediction.se.fit^2, na.rm=TRUE), # Rubin's Rules for pooling within-m variance
              v_b     = sum(prediction.fit - fit_bar)^2 / (m - 1), # pooling between-m variance
              v_p     = v_w + v_b * (1 + (1 / m)), # overall variance
              se_p    = sqrt(v_p)) %>% # standard error  
    # use the p_suffix to indicate these are pooled
    mutate(lwr_p = fit_bar - se_p * 1.96, # assuming normally distributed imputations
           upr_p = fit_bar + se_p * 1.96) 
  
}

# not enough memory to run all crops or even all time periods within a crop
# run one crop and time period separately and then relist(list) for nesting later
# 16 different groups


# maize pool across m -----------------------------------------------------

predictions_level_4_maize_abs <- lapply(1, function(k){ # there's only 1 model spec now
  lapply(1, function(j, k){ # j = crop 
    lapply(1:4, function(l, j, k){ # l = time period 
      lapply(1:23, pooling_m_predictions_abs, l, j, k)
    }, j, k) 
  }, k)
}) 


saveRDS(predictions_level_4_maize_abs, 
        here("results",
             "predictions",
             "predictions_level_4_maize_abs.RData"))

predictions_level_4_rice_abs <- lapply(1, function(k){ # there's only 1 model spec now
  lapply(2, function(j, k){ # j = crop 
    lapply(1:4, function(l, j, k){ # l = time period 
      lapply(1:23, pooling_m_predictions_abs, l, j, k)
    }, j, k) 
  }, k)
}) 

saveRDS(predictions_level_4_rice_abs, 
        here("results",
             "predictions",
             "predictions_level_4_rice_abs.RData"))


predictions_level_4_soybean_abs <- lapply(1, function(k){ # there's only 1 model spec now
  lapply(3, function(j, k){ # j = crop 
    lapply(1:4, function(l, j, k){ # l = time period 
      lapply(1:23, pooling_m_predictions_abs, l, j, k)
    }, j, k) 
  }, k)
}) 

saveRDS(predictions_level_4_soybean_abs, 
        here("results",
             "predictions",
             "predictions_level_4_soybean.RData_abs"))


predictions_level_4_wheat_abs <- lapply(1, function(k){ # there's only 1 model spec now
  lapply(4, function(j, k){ # j = crop 
    lapply(1:4, function(l, j, k){ # l = time period 
      lapply(1:23, pooling_m_predictions_abs, l, j, k)
    }, j, k) 
  }, k)
}) 

saveRDS(predictions_level_4_wheat_abs, 
        here("results",
             "predictions",
             "predictions_level_4_wheat_abs.RData"))


# pool across GCMs --------------------------------------------------------

# do this for each crop separately

# read in crop m-pooled predictions

predictions_level_4_maize_abs <- readRDS( 
  here("results",
       "predictions",
       "predictions_level_4_maize_abs.RData"))


predictions_level_4_rice_abs <- readRDS( 
  here("results",
       "predictions",
       "predictions_level_4_rice_abs.RData"))

predictions_level_4_soybean_abs <- readRDS( 
  here("results",
       "predictions",
       "predictions_level_4_soybean_abs.RData"))


predictions_level_4_wheat_abs <- readRDS( 
  here("results",
       "predictions",
       "predictions_level_4_wheat_abs.RData"))


# relist these
# drop 'model' level, bringing down to 3 levels
# note that all crops only have 1 index in place 2, as only crop in the crop list
predictions_level_3_abs <- list(predictions_level_4_maize_abs[[1]][[1]], # takes model 1 and 'crop' 1
                            predictions_level_4_rice_abs[[1]][[1]],
                            predictions_level_4_soybean_abs[[1]][[1]],
                            predictions_level_4_wheat_abs[[1]][[1]])


# pool across gcms - use function dynamically to change level 4 prediction df
gcm <- 23 # note this cannot be same as idcol name! otherwise where m is computed will not work/collapse

pooling_gcm_predictions_abs <- function(l, j){ 
  
  predictions_level_3_abs[[j]][[l]] %>% # j = crop; l = time period
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
  
}

predictions_level_2_abs <- lapply(1:4, function(j){ # crop
  lapply(1:4, pooling_gcm_predictions_abs, j) # time period
}) 

saveRDS(predictions_level_2_abs, here("results",
                                  "predictions",
                                  "predictions_level_2_abs.RData"))
