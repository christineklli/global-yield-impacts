
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
  # plot and save
  tar_target(predictions_gridded_plot,
             plot_predictions_gridded(
               predictions=predictions_gridded_raster,
               returnStack=TRUE,
               World=World
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
               worldmap_clean=worldmap_clean
               )
  ),
  tar_target(predictions_country_plot,
             plot_country_predictions(
               predictions=predictions_gridded_raster,
               worldmap_clean=worldmap_clean,
               crop_production_raster_agg=crop_production_raster_agg,
               returnStack=TRUE,
               World=World
             )
  )
)


# now combine the model_spec gridded predictions for each crop to produce model agreement maps
# there should now be 4 crop map plots with 4 time periods in each plot


# repeat with country weighted predictions - also 4 crop map plots with 4 time periods in each plot