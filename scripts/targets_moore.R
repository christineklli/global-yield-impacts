
packages <- c("dplyr", "tidyr", "data.table", "lubridate", "viridis", "sp", "sf",
              "rgdal", "broom", "raster", "ncdf4", "here", "rworldmap", "R.utils",
              "stringr", "mice", "cleangeo", "renv", "targets",
              "readr", "terra", "qs", "rlang", "future", "purrr", "parallelly", "fst",
              "tmap", "cowplot", "ggpubr", "rsample", "furrr", "spData")

tar_option_set(packages=packages,
               memory="transient",
               garbage_collection=TRUE,
               format="qs")

# somehow we have to read the data into global env one at a time
# as they have the same names so superseded each other

base::load(here::here("data", "Moore data", "Maize_AdaptTRUE_CO2TRUE_Revised.Rdat"))
moore_maize <- yieldchange_errorbars
base::load(here::here("data", "Moore data", "Rice_AdaptTRUE_CO2TRUE_Revised.Rdat"))
moore_rice <- yieldchange_errorbars
base::load(here::here("data", "Moore data", "Soy_AdaptTRUE_CO2TRUE_Revised.Rdat"))
moore_soy <- yieldchange_errorbars
base::load(here::here("data", "Moore data", "Wheat_AdaptTRUE_CO2TRUE_Revised.Rdat"))
moore_wheat <- yieldchange_errorbars


targets_moore <- list(
  tar_target(moore_maize_data, 
             moore_maize),
  tar_target(moore_rice_data,
             moore_rice),
  tar_target(moore_soy_data,
             moore_soy),
  tar_target(moore_wheat_data,
             moore_wheat),
  tar_target(moore_data, # aggregate into list, but only central estimates
             list(moore_maize_data[[2]],
                  moore_rice_data[[2]],
                  moore_soy_data[[2]],
                  moore_wheat_data[[2]])),
  # plot 1-3 degrees
  tar_target(moore_plots,
             plot_moore_gridded(
               data=moore_data,
               World=World,
               crops=crops,
               path="results/figures/moore_predictions_%s.png"
             ))
)