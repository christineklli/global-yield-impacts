
tar_option_set(packages = packages,
               memory = "transient", # activate transient memory
               garbage_collection = TRUE, # activate garbage collection
               format = "qs" # efficient storage format, need to first install qs
) 

future::plan(future::multisession, workers = 5)

targets_uncertainty <- list(
  # create df of lon, lat, production (note that each production column is named differently)
  tar_target(crop_production_df,
             create_crop_production_df(
    raster=crop_production_raster_agg)),
# calculate global weighted mean predictions using crop production volumes as weights
  tar_target(adj_global_prediction_level_5_maize,
             calc_global_weighted_mean_prediction(
               prediction=adj_prediction_level_5_maize,
               crop_production_df=crop_production_df,
               crop_no=1,
               model_specs=model_specs
             ),
             pattern=map(adj_prediction_level_5_maize), # do this iteratively
             format="fst_tbl" # rbindlist instead of iteration="list"
  ),
tar_target(adj_global_prediction_level_5_rice,
           calc_global_weighted_mean_prediction(
             prediction=adj_prediction_level_5_rice,
             crop_production_df=crop_production_df,
             crop_no=2,
             model_specs=model_specs
           ),
           pattern=map(adj_prediction_level_5_rice), 
           format="fst_tbl" 
),
tar_target(adj_global_prediction_level_5_soy,
           calc_global_weighted_mean_prediction(
             prediction=adj_prediction_level_5_soy,
             crop_production_df=crop_production_df,
             crop_no=3,
             model_specs=model_specs
           ),
           pattern=map(adj_prediction_level_5_soy), 
           format="fst_tbl" 
),
tar_target(adj_global_prediction_level_5_wheat,
           calc_global_weighted_mean_prediction(
             prediction=adj_prediction_level_5_wheat,
             crop_production_df=crop_production_df,
             crop_no=4,
             model_specs=model_specs
           ),
           pattern=map(adj_prediction_level_5_wheat), 
           format="fst_tbl" 
),
# list all of them
tar_target(adj_global_prediction_level_5_all_crops,
           list(
             adj_global_prediction_level_5_maize,
             adj_global_prediction_level_5_rice,
             adj_global_prediction_level_5_soy,
             adj_global_prediction_level_5_wheat
           )),
# plot density distributions by model spec (model uncertainty) & imputation (missing data uncertainty)
# distributions of each model spec x m pooled across 23 GCMs
# i.e. shows prediction data model uncertainty
tar_target(
  model_m_distribution_plot,
  plot_model_m_distributions(
    predictions=adj_global_prediction_level_5_all_crops,
    time_periods=time_periods,
    crops=crops,
    path="results/figures/adjusted_model_m_distributions_%s.png"
  )
),
# distributions of each model spec pooled across 5 m x 23 GCMs
# ie shows both missing data & prediction data model uncertainty
tar_target(
  model_pooled_m_distribution_plot,
  plot_model_pooled_m_distributions(
    predictions=adj_global_prediction_level_5_all_crops,
    time_periods=time_periods,
    crops=crops,
    path="results/figures/adjusted_model_pooled_m_distributions_%s.png"
  )
),
# sampling uncertainty - block bootstrapping
tar_target(
  block_bootstrap_samples,
  create_block_bootstrap_samples(
   data=crop_imputed_rst_data,
   ncores=3
  ) 
), 
# unnest so that it is list of 20 models not 4
tar_target(
  block_bootstrap_samples_unnested,
  {flattenlist(block_bootstrap_samples)}
), 
tar_target(
  block_bootstrap_fit, # for some reason model nested column doesn't appear??
  fit_block_bootstrap(
    block_bootstrap_samples_unnested=block_bootstrap_samples_unnested,
    model_specs=model_specs,
    ncores=5
  ),
  pattern=cross(model_specs, block_bootstrap_samples_unnested),
  iteration='list'),
# only keep fit estimates and identifiers for model spec, crop and m
# trying this so that we don't have to renest fit estimates later on
# tar_target(block_bootstrap_fit_nested,
#            fit_block_bootstrap_nested(
#              block_bootstrap_samples=block_bootstrap_samples,
#              model_specs=model_specs,
#              ncores=5
#            ),
#            pattern=cross(model_specs, block_bootstrap_samples),
#            iteration='list' ),
tar_target(
  block_bootstrap_fit_only,
  {t <- block_bootstrap_fit %>% dplyr::select(!c(data, splits))
    
    df <- data.frame(model_specs=model_specs,
               model_spec=c("gam_RS",
                       "gam_RI",
                       "glm_RS",
                       "glm_RI",
                       "lm"))
    t %>% left_join(df, by=c("model_specs")) %>% 
      relocate(id, model_spec) %>% 
      dplyr::select(!model_specs)  
    
    },
  pattern=map(block_bootstrap_fit)
),
# prediction
tar_target(block_bootstrap_prediction_maize, 
           predict_block_bootstrap(
             fit=block_bootstrap_fit_only,
             data_og=data_future_maize_nested[[4]], # only predict on 2081-2100 data
             data_adj=int_adj_future_maize[[4]]#,
             #ncores=5
             ),
             pattern=slice(block_bootstrap_fit_only, index=c(1:25)),
             format='fst_tbl', # this may need to be removed
             iteration='list'
           )

# now cross / fit to future data for each 25 batches by crop
# need of course to triple check that it is pulling the right crops, but should be in order
# i.e. first 1:25 is maize; 26:50 is rice; 51:75 is soy, 76:100 is wheat

# nest so that we can split up this target by crop
# tar_target(block_bootstrap_fit_nested,
#            
#            { future::plan(future::multisession, workers = 5)
#              
#              c <-  split(block_bootstrap_fit_only, block_bootstrap_fit_only$crop)
#              d <- future.apply::future_lapply(1:4, function(i){
#                split(c[[i]], 
#                      interaction(c[[i]]$model_spec, c[[i]]$imputation)
#                )      })
#            }
#              
# )
)
# predict with adjustments for intercept, 
# pattern=cross(block_bootstrap_fit(20 of these),prediction_data(only do 2081-2100, for all GCMs))
# use furrr::future_map(splits) and mutate predictions
# lastly replace model_specs with model_name and deselect model_specs

