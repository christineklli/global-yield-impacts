
tar_option_set(packages = packages,
               memory = "transient", # activate transient memory
               garbage_collection = TRUE, # activate garbage collection
               format = "qs" # efficient storage format, need to first install qs
) 

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

targets_prediction <- list(
  # clean prediction data for only complete cases
  # this is resulting in empty prediction data; suggesting that something about the prediction data
  # has been done differently
  tar_target(prediction_data_complete_cases,
             clean_prediction_data(
               data=prediction_data
             )),
  # run prediction at the 5 level index
  tar_target(prediction_gridded_level_5,
             predict_gridded_level_5(
               models=models,
               data=prediction_data_complete_cases
             )),
  # pool across m for maize
  tar_target(prediction_gridded_level_4, # maize
             pool_across_m(
               predictions=prediction_gridded_level_5,
               crop_no=1
             )),
  # pool across m for rice
  tar_target(prediction_gridded_level_4_rice,
             pool_across_m(
               predictions=prediction_gridded_level_5,
               crop_no=2
             )),
  # pool across m for soy
  tar_target(prediction_gridded_level_4_soy,
             pool_across_m(
               predictions=prediction_gridded_level_5,
               crop_no=3
             )),
  # pool across m for wheat
  tar_target(prediction_gridded_level_4_wheat,
             pool_across_m(
               predictions=prediction_gridded_level_5,
               crop_no=4
             )),
  # relist predictions/reindex
  tar_target(predictions_gridded_level_3,
             relist_predictions(
               predictions_1=prediction_gridded_level_4, # maize
               predictions_2=prediction_gridded_level_4_rice,
               predictions_3=prediction_gridded_level_4_soy,
               predictions_4=prediction_gridded_level_4_wheat
             )),
  # pool across 23 GCMs
  tar_target(predictions_gridded_level_2,
             pool_gcm_predictions(
               predictions=predictions_gridded_level_3
             )),
  # join full coordinates to predictions
  tar_target(predictions_gridded,
             add_coords_to_predictions(
               predictions=predictions_gridded_level_2,
               coords_countries=coords_countries
             ))
  
)

# predict using lm model 
targets_prediction_lm <- list(
  # run prediction at the 5 level index
  tar_target(prediction_gridded_level_5_lm,
             predict_gridded_level_5(
               models=lm_models,
               data=prediction_data_complete_cases
             )),
  # pool across m for maize
  tar_target(prediction_gridded_level_4_maize_lm, # maize
             pool_across_m(
               predictions=prediction_gridded_level_5_lm,
               crop_no=1
             )),
  # pool across m for rice
  tar_target(prediction_gridded_level_4_rice_lm,
             pool_across_m(
               predictions=prediction_gridded_level_5_lm,
               crop_no=2
             )),
  # pool across m for soy
  tar_target(prediction_gridded_level_4_soy_lm,
             pool_across_m(
               predictions=prediction_gridded_level_5_lm,
               crop_no=3
             )),
  # pool across m for wheat
  tar_target(prediction_gridded_level_4_wheat_lm,
             pool_across_m(
               predictions=prediction_gridded_level_5_lm,
               crop_no=4
             )),
  # relist predictions/reindex
  tar_target(predictions_gridded_level_3_lm,
             relist_predictions(
               predictions_1=prediction_gridded_level_4_maize_lm, # maize
               predictions_2=prediction_gridded_level_4_rice_lm,
               predictions_3=prediction_gridded_level_4_soy_lm,
               predictions_4=prediction_gridded_level_4_wheat_lm
             )),
  # pool across 23 GCMs
  tar_target(predictions_gridded_level_2_lm,
             pool_gcm_predictions(
               predictions=predictions_gridded_level_3_lm
             )),
  # join full coordinates to predictions
  tar_target(predictions_gridded_lm,
             add_coords_to_predictions(
               predictions=predictions_gridded_level_2_lm,
               coords_countries=coords_countries
             )),
  # calculate global mean predictions for gam models
  tar_target(global_mean_predictions_gam,
             calculate_global_mean_predictions(
               predictions=predictions_gridded
             )),
  # calculate global mean predictions for lm models
  tar_target(global_mean_predictions_lm,
             calculate_global_mean_predictions_rm_inf (
               predictions=predictions_gridded_lm
             ))
)

#tar_read(predictions_gridded) %>% saveRDS(here("processed", "predictions_gridded.RData"))
# tar_read(global_mean_predictions_gam) %>% saveRDS(here("processed", "global_mean_predictions_gam.RData"))
# tar_read(global_mean_predictions_lm) %>% saveRDS(here("processed", "global_mean_predictions_lm.RData"))
