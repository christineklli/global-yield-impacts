library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.

# start from script 2, i.e. after webscraping
# note that we no longer use here() for relative file paths
# but write that in directly with forward slash
# files are located relative to the file _targets.R 
# which has to be in the main working directory outside of the scripts folder

library(targets)
source("scripts/functions_data.R") 
source("scripts/targets_data.R")
source("scripts/functions_model.R")
source("scripts/targets_model.R")
source("scripts/functions_prediction_data.R")
source("scripts/targets_prediction_data.R")
source("scripts/functions_prediction.R")
source("scripts/targets_prediction.R")
source("scripts/functions_plot_prediction.R")
source("scripts/targets_plot_prediction.R")
source("scripts/functions_cross_validation.R")
source("scripts/targets_cross_validation.R")
source("scripts/functions_intercept_adj_prediction.R")
source("scripts/targets_intercept_adj_prediction.R")
source("scripts/functions_uncertainty.R")
source("scripts/targets_uncertainty.R")
source("scripts/functions_map_model_agreement.R")
source("scripts/targets_map_model_agreement.R")
source("scripts/targets_food_security.R")

# need to define packages in this file
packages <- c("rvest", "dplyr", "tidyr", "janitor", "forecast","data.table", "ggplot2","readxl", 
              "Hmisc", "zoo", "lubridate", "networkD3", "viridis", "maptools","ggmap", 
              "maps", "sp", "sf", "geojsonio",  "rgdal", "broom", "plotly", "htmlwidgets", "gridExtra",
              "raster", "gridBase", "ggthemes", "grid", "tidync", "ncmeta","stars", "devtools", 
              "RNetCDF", "here", "rworldmap", "R.utils", "stringr", "compareDF", "utils",
              "ncdf4", "mice", "miceadds", "Rcpp", "VIM", "cvms", "groupdata2", "exactextractr",
              "cleangeo", "rworldxtra", "rasterize", "ggeffects", "ggExtra", "GGally",
              "forestplot", "metafor", "itsadug", "renv", "targets", "stringr",
              "readr", "terra", "qs", "rlang", "future", "purrr", "parallelly", "fst", "tmap",
              "cowplot",  "ggpubr", "rsample", "furrr", "gammit", "spData")


tar_option_set(packages = packages,
               memory = "transient", # activate transient memory
               garbage_collection = TRUE, # activate garbage collection
               format = "qs", # efficient storage format, need to first install qs
               storage="worker",
               retrieval="worker"
               ) 

# targets pipeline - eventually modularise this by modelling stage

# note that we skip create targets for web scraping - retain script 1
# not sure how targets() interacts with json/web environments, try later
#tar_target(file, "processed/agimpacts_full.csv", format = "file"),
#tar_target(CGIAR_data, get_data_from_csv(file)),
#tar_target(CGIAR_cleaned, remove_na(CGIAR_data)),
# note that we skip targets for the manual data validation - retain script 2
# because it is just too hard to rewrite...for now
# thus start from scraped + validated data agimpacts_final.csv


list(
  targets_data_production,
  targets_data_crop_calendar,
  targets_data_tmp_precip,
  targets_data_yields,
  targets_impute,
  targets_model_gam_glm,
  targets_prediction_data_cru,
  targets_prediction_data_monf,
  targets_prediction_data_misc,
  targets_prediction_data_cmip,
  targets_prediction,
  targets_plot_predictions,
  targets_cross_validation,
  targets_intercept_adj_prediction,
  targets_uncertainty,
  targets_map_model_agreement,
  targets_food_security
)
      

# note these commands must be commented out or will be recursive
# since file _targets.R runs all code in this script automatically

# check pipeline & dependencies
#tar_manifest()
#tar_visnetwork()

#  make outputs!
# tar_make() 

# inspect output
#tar_read(CGIAR_data)
