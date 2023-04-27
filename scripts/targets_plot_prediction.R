
tar_option_set(packages = packages,
               memory = "transient", # activate transient memory
               garbage_collection = TRUE, # activate garbage collection
               format = "qs" # efficient storage format, need to first install qs
) 



targets_plot_predictions <- list(
  # note that the order of models here is not the order defined in model_specs
  # instead, they are in alphabetical order
  # so: gam_RI, gam_RS, glm_RI, glm_RS, lm
  # each plot contains all 4 time periods, there should be 20=5 model spec * 4 crop plots
  
  tar_target(predictions_gridded_raster, # list of 20 rasters
             rasterise_predictions_gridded(
               predictions=predictions_nested,
               time_periods=time_periods
             )),
  tar_target(World, # rnaturalearth
             # # spatial object from sf package
             # https://jakubnowosad.com/spData/reference/world.html
             st_read(system.file("shapes/world.gpkg", package="spData"))
  ), 
  tar_target(model_spec_alphabetical, c(
    "gamRI", "gamRS", "glmRI", "glmRS", "lm"
  )
             ),
  # plot and save
  tar_target(predictions_gridded_plot,
             plot_predictions_gridded(
               predictions=predictions_gridded_raster,
               returnStack=TRUE,
               World=World,
               model_spec_alphabetical=model_spec_alphabetical,
               crops=crops,
               path="results/figures/unadjusted_predictions_gridded/unadjusted_predictions_gridded_%s_%s.png"
             )),
  
  # plot country weighted predictions, also 20 plots with 4 time periods in each plot
  
  tar_target(crop_production_raster_agg, # production weights
             create_crop_production_raster_agg(
               crop_production_rasters=crop_production_rasters
             )),
  tar_target(country_predictions_tbl, # list of 4 crop tbls with 4 columns for time period weighted mean              create_country_predictions_tbl(
             create_country_predictions_tbl(
               predictions=predictions_gridded_raster,
               crop_production_raster_agg=crop_production_raster_agg,
               worldmap_clean=worldmap_clean,
               model_spec_alphabetical=model_spec_alphabetical,
               crops=crops,
               path="results/tables/unadjusted/unadjusted_country_weighted_predictions_%s_%s.csv"
               )
  ),
  tar_target(predictions_country_plot,
             plot_country_predictions(
               predictions=predictions_gridded_raster,
               worldmap_clean=worldmap_clean,
               crop_production_raster_agg=crop_production_raster_agg,
               returnStack=TRUE,
               World=World,
               model_spec_alphabetical=model_spec_alphabetical,
               crops=crops,
               path="results/figures/unadjusted_predictions_country_weighted/unadjusted_predictions_country_weighted_%s_%s.png"
             )
  ),
  
  # REPEAT FOR ADJUSTED (BY INTERCEPT)
  
  # plot gridded predictions
  tar_target(adj_predictions_gridded_raster, # list of 20 rasters
             rasterise_predictions_gridded(
               predictions=adj_predictions_nested,
               time_periods=time_periods
             )),
  tar_target(adj_predictions_gridded_plot,
             plot_predictions_gridded(
               predictions=adj_predictions_gridded_raster,
               returnStack=TRUE,
               World=World,
               model_spec_alphabetical=model_spec_alphabetical,
               crops=crops,
               path="results/figures/adjusted_predictions_gridded/adjusted_predictions_gridded_%s_%s.png"
             )),
  
  # calculate country weighted predictions and write as csv
  tar_target(adj_country_predictions_tbl, # list of 4 crop tbls with 4 columns for time period weighted mean              create_country_predictions_tbl(
             create_country_predictions_tbl(
               predictions=adj_predictions_gridded_raster,
               lwr_bound=adj_lwr_gridded_raster,
               upr_bound=adj_upr_gridded_raster,
               crop_production_raster_agg=crop_production_raster_agg,
               worldmap_clean=worldmap_clean,
               model_spec_alphabetical=model_spec_alphabetical,
               crops=crops,
               path="results/tables/adjusted/adjusted_country_weighted_predictions_%s_%s_RCP8.5.csv"
             )
  ),
  
  # plot country weighted predictions
  tar_target(adj_predictions_country_plot,
             plot_country_predictions(
               predictions=adj_predictions_gridded_raster,
               worldmap_clean=worldmap_clean,
               crop_production_raster_agg=crop_production_raster_agg,
               returnStack=TRUE,
               World=World,
               model_spec_alphabetical=model_spec_alphabetical,
               crops=crops,
               path="results/figures/adjusted_predictions_country_weighted/adjusted_predictions_country_weighted_%s_%s.png"
             )
  ),

  # plot adjusted gridded predictions with dots showing spatial distribution of crop production
  tar_target(crop_sf,
             create_crop_production_sf(
               raster=crop_production_raster_agg
             )),
  tar_target(adj_predictions_gridded_plot_dots,
             plot_with_dots(
               predictions=adj_predictions_gridded_raster, 
               crop_sf=crop_sf, 
               model_spec_alphabetical=model_spec_alphabetical, 
               crops=crops, 
               path="results/figures/adjusted_predictions_gridded/adjusted_predictions_gridded_dots_%s_%s.png", 
               returnStack=TRUE,
               World=World,
               time_periods=time_periods
             ))
)
