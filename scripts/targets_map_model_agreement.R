
tar_option_set(packages = packages,
               memory = "transient", # activate transient memory
               garbage_collection = TRUE, # activate garbage collection
               format = "qs" # efficient storage format, need to first install qs
) 

# dynamic branching targets
future::plan(future::multisession, workers = 5)

targets_map_model_agreement <- list(
  
  tar_target(model_agreement_sf, # nested list of sf [[crop]][[time]] 
             calc_model_agreement(
               predictions=adj_predictions_by_time_period,
               threshold=100, # vary this; model agreement threshold (80% = 4/5 models agree on the sign)
               ncores=5
             )
  ),
  # pool across models and rasterise
  tar_target(adj_pooled_model_predictions_raster,
             rasterise_pooled_model_predictions(
               predictions=adj_predictions_by_time_period,
               time_periods=time_periods,
               ncores=5
             )),
  # map
  tar_target(model_agreement_map,
             plot_model_agreement(
               predictions=adj_pooled_model_predictions_raster, 
               model_agreement_sf=model_agreement_sf, 
               time_periods=time_periods, 
               crops=crops, 
               path="results/figures/adjusted_predictions_gridded/adjusted_model_agreement_100_%s.png", 
               returnStack=T, 
               World=World,
               ncores=5
             ))
)




