
tar_option_set(packages = packages,
               memory = "transient", # activate transient memory
               garbage_collection = TRUE, # activate garbage collection
               format = "qs" # efficient storage format, need to first install qs
) 


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
)
)