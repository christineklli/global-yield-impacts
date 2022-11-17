
tar_option_set(packages = packages,
               memory = "transient", # activate transient memory
               garbage_collection = TRUE, # activate garbage collection
               format = "qs" # efficient storage format, need to first install qs
) 

targets_plot_predictions_gam <- list(
  # some of these are really high, need to check against predictions_gridded
  tar_target(predictions_gridded_gam_raster, rasterise_predictions_gridded(
    predictions=predictions_gridded
  )),
  # plot predictions
  tar_target(plots_predictions_gridded_gam,
             plot_predictions_gridded(
               predictions=predictions_gridded
             )),
  # save
  tar_target(saved_plots_predictions,
             save_plots_predictions(
               plots=plots_predictions_gridded_gam,
               path="results/figures/predictions_gridded_gam"
             ))
)

targets_plot_predictions_lm <- list(
  # some of these are really high, need to check against predictions_gridded
  #tar_target(predictions_gridded_lm_raster, rasterise_predictions_gridded(
  #  predictions=predictions_gridded
  #)),
  # plot predictions
  tar_target(plots_predictions_gridded_lm,
             plot_predictions_gridded(
               predictions=predictions_gridded_lm
             )),
  # save
  tar_target(saved_plots_predictions_lm,
             save_plots_predictions(
               plots=plots_predictions_gridded_lm,
               path="results/figures/predictions_gridded_lm"
             ))
)