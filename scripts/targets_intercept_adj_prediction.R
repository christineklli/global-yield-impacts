
tar_option_set(packages = packages,
               memory = "transient", # activate transient memory
               garbage_collection = TRUE, # activate garbage collection
               format = "qs" # efficient storage format, need to first install qs
) 

# dynamic branching targets
future::plan(future::multisession, workers = 5)

targets_intercept_adj_prediction <- list(
  # create zero change prediction data (reflects yield change intercept results i.e. at zero temp, precip & CO2 change)
  # exactly same structure as data_future_crop_nested
  tar_target(int_adj_prediction_data,
             create_int_adj_prediction_data(
               prediction_data=prediction_data_complete_cases,
               time_periods=time_periods, 
               GCMs=GCMs
             )),
  # now create data future crop targets for same structure as original
  tar_target(int_adj_future_maize,
             int_adj_prediction_data[[1]]),
  tar_target(int_adj_future_rice,
             int_adj_prediction_data[[2]]),
  tar_target(int_adj_future_soy,
             int_adj_prediction_data[[3]]),
  tar_target(int_adj_future_wheat,
             int_adj_prediction_data[[4]]),
  # fit model prediction
  # follow exactly the same structure as before
  # 20 branches x 7m rows
  tar_target(int_adj_prediction_level_5_maize, 
             predict_level_5(
               fit=fit_maize_nested,
               data=int_adj_future_maize,
               ncores=5
             ),
             pattern=cross(fit_maize_nested, 
                           int_adj_future_maize),
             iteration='list',
             format='fst_tbl'
  ),
  
tar_target(int_adj_prediction_level_5_rice, 
           predict_level_5(
             fit=fit_rice_nested,
             data=int_adj_future_rice,
             ncores=5
           ),
           pattern=cross(fit_rice_nested, 
                         int_adj_future_rice),
           iteration='list',
           format='fst_tbl'
),

tar_target(int_adj_prediction_level_5_soy, 
           predict_level_5(
             fit=fit_soy_nested,
             data=int_adj_future_soy,
             ncores=5
           ),
           pattern=cross(fit_soy_nested, 
                         int_adj_future_soy),
           iteration='list',
           format='fst_tbl'
),

tar_target(int_adj_prediction_level_5_wheat,
           predict_level_5(
             fit=fit_wheat_nested,
             data=int_adj_future_wheat,
             ncores=5
           ),
           pattern=cross(fit_wheat_nested, 
                         int_adj_future_wheat),
           iteration='list',
           format='fst_tbl'
),

# critical that prediction_level_5_maize and int_adj_prediction_level_5_maize are in same order
# in terms of model spec by list element/branch
# and in terms of m by row number
# so that we can map subtraction functions in sequence - 
# ie mapping gam RS prediction unadjusted to gam RS prediction at zero climate change
tar_target(adj_prediction_level_5_maize,
           adjust_prediction_level_5(
             prediction_og=prediction_level_5_maize,
             prediction_adj=int_adj_prediction_level_5_maize
           ),
           pattern=map(int_adj_prediction_level_5_maize,
                       prediction_level_5_maize),
           iteration='list',
           format='fst_tbl'),

tar_target(adj_prediction_level_5_rice,
           adjust_prediction_level_5(
             prediction_og=prediction_level_5_rice,
             prediction_adj=int_adj_prediction_level_5_rice
           ),
           pattern=map(int_adj_prediction_level_5_rice,
                       prediction_level_5_rice),
           iteration='list',
           format='fst_tbl'),

tar_target(adj_prediction_level_5_soy,
           adjust_prediction_level_5(
             prediction_og=prediction_level_5_soy,
             prediction_adj=int_adj_prediction_level_5_soy
           ),
           pattern=map(int_adj_prediction_level_5_soy,
                       prediction_level_5_soy),
           iteration='list',
           format='fst_tbl'),

tar_target(adj_prediction_level_5_wheat,
           adjust_prediction_level_5(
             prediction_og=prediction_level_5_wheat,
             prediction_adj=int_adj_prediction_level_5_wheat
           ),
           pattern=map(int_adj_prediction_level_5_wheat,
                       prediction_level_5_wheat),
           iteration='list',
           format='fst_tbl'),

# then pool across m for level 3-4 predictions

tar_target(adj_prediction_level_4_maize, # 20 branches x 1.4m rows (7m / 5 imputation)
           pool_across_m_int_adj(
             predictions=adj_prediction_level_5_maize
           ),
           pattern=map(adj_prediction_level_5_maize), # iterates over each branch 
           iteration='list',
           format='fst_tbl'),

tar_target(adj_prediction_level_3_maize, # 20 branches x 60k rows (1.4m / 23 gcm)
           pool_across_gcm(
             predictions=adj_prediction_level_4_maize,
             coords_countries=coords_countries
           ),
           pattern=map(adj_prediction_level_4_maize),
           iteration='list',
           format='fst_tbl'),

tar_target(adj_prediction_level_4_rice, # 20 branches x 1.4m rows (7m / 5 imputation)
           pool_across_m_int_adj(
             predictions=adj_prediction_level_5_rice
           ),
           pattern=map(adj_prediction_level_5_rice), # iterates over each branch 
           iteration='list',
           format='fst_tbl'),

tar_target(adj_prediction_level_3_rice, # 20 branches x 60k rows (1.4m / 23 gcm)
           pool_across_gcm(
             predictions=adj_prediction_level_4_rice,
             coords_countries=coords_countries
           ),
           pattern=map(adj_prediction_level_4_rice),
           format='fst_tbl'),

tar_target(adj_prediction_level_4_soy, # 20 branches x 1.4m rows (7m / 5 imputation)
           pool_across_m_int_adj(
             predictions=adj_prediction_level_5_soy
           ),
           pattern=map(adj_prediction_level_5_soy), # iterates over each branch 
           iteration='list',
           format='fst_tbl'),

tar_target(adj_prediction_level_3_soy, # 20 branches x 60k rows (1.4m / 23 gcm)
           pool_across_gcm(
             predictions=adj_prediction_level_4_soy,
             coords_countries=coords_countries
           ),
           pattern=map(adj_prediction_level_4_soy),
           format='fst_tbl'),

tar_target(adj_prediction_level_4_wheat, # 20 branches x 1.4m rows (7m / 5 imputation)
           pool_across_m_int_adj(
             predictions=adj_prediction_level_5_wheat
           ),
           pattern=map(adj_prediction_level_5_wheat), # iterates over each branch 
           iteration='list',
           format='fst_tbl'),

tar_target(adj_prediction_level_3_wheat, # 20 branches x 60k rows (1.4m / 23 gcm)
           pool_across_gcm(
             predictions=adj_prediction_level_4_wheat,
             coords_countries=coords_countries
           ),
           pattern=map(adj_prediction_level_4_wheat),
           format='fst_tbl'),
# prepare for plotting
# because this was 'iteration=list', need to rbind
tar_target(adj_prediction_level_3_maize_df,
           rbindlist(adj_prediction_level_3_maize)),
tar_target(adj_predictions_by_time_period,
           {
             t <- rbind(adj_prediction_level_3_maize_df,
                        adj_prediction_level_3_rice,
                        adj_prediction_level_3_soy,
                        adj_prediction_level_3_wheat)
             
             # create concordance for model_spec and gam_RI / gam_RS / glm_RI / glm_RS / LM etc
             df <- data.frame(model_spec=model_specs,
                              model=c("gam_RS",
                                      "gam_RI",
                                      "glm_RS",
                                      "glm_RI",
                                      "lm"))
             t %>% left_join(df, by=c("model_spec")) %>% 
               relocate(model) %>% 
               relocate(model_spec, .after=last_col())
             
           }
),
tar_target(adj_predictions_nested,
           {
             one_level <- split(adj_predictions_by_time_period, adj_predictions_by_time_period$model)
             # split by crop
             two_level <- lapply(1:5, function(i){
               split(one_level[[i]], 
                     one_level[[i]]$crop_pooled
               )      })
             two_level
             
           }
           
),
# calculate global production weighted predictions
tar_target(adj_predictions_global_wt_mean,
           create_global_predictions_tbl(
             predictions=adj_predictions_gridded_raster,
             crop_production_raster_agg=crop_production_raster_agg,
             worldmap_clean=worldmap_clean,
             model_spec_alphabetical=model_spec_alphabetical,
             crops=crops,
             path="results/tables/adjusted/adjusted_global_weighted_predictions.csv"
           ))

)