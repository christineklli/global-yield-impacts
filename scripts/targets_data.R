# data prep targets file
# need to define packages in this file
packages <- c("rvest", "dplyr", "tidyr", "janitor", "forecast","data.table", "ggplot2","readxl", 
              "Hmisc", "zoo", "lubridate", "networkD3", "viridis", "maptools","ggmap", 
              "maps", "sp", "sf", "geojsonio",  "rgdal", "broom", "plotly", "htmlwidgets", "gridExtra",
              "raster", "gridBase", "ggthemes", "grid", "tidync", "ncmeta","stars", "devtools", 
              "RNetCDF", "here", "rworldmap", "R.utils", "stringr", "compareDF", "utils",
              "ncdf4", "mice", "miceadds", "Rcpp", "VIM", "cvms", "groupdata2", "exactextractr",
              "cleangeo", "rworldxtra", "rasterize", "ggeffects", "ggExtra", "GGally",
              "forestplot", "metafor", "itsadug", "renv", "targets", "stringr",
              "readr", "terra", "qs")

tar_option_set(packages = packages,
               memory = "transient", # activate transient memory
               garbage_collection = TRUE, # activate garbage collection
               format = "qs" # efficient storage format, need to first install qs
) 

# note that we skip create targets for web scraping - retain script 1
# not sure how targets() interacts with json/web environments, try later
#tar_target(file, "processed/agimpacts_full.csv", format = "file"),
#tar_target(CGIAR_data, get_data_from_csv(file)),
#tar_target(CGIAR_cleaned, remove_na(CGIAR_data)),
# note that we skip targets for the manual data validation - retain script 2
# because it is just too hard to rewrite...for now
# thus start from scraped + validated data agimpacts_final.csv


# CGIAR and Monfreda data prep --------------------------------------------

# corresponds to script 3 in agimpacts-precip repo
targets_data_monfreda <- list(
  tar_target(ag, "processed/agimpacts_final.csv", format = "file"),
  tar_target(agimpacts, get_data_from_csv(ag)),
  # note we need to convert some variables from chr to factor later
  # work with Monfreda data FIRST and THEN make all changes to agimpacts in one function
  tar_target(monfreda_prod_files, c("data/Monfreda data/maize_Production.tif",
                                    "data/Monfreda data/rice_Production.tif",
                                    "data/Monfreda data/soybean_Production.tif",
                                    "data/Monfreda data/wheat_Production.tif"), format = "file"),
  tar_target(worldmap, rworldmap::getMap(resolution = "coarse")),
  tar_target(worldmap_clean, cleangeo::clgeo_Clean(worldmap)),
  # this takes a long time
  tar_target(crop_production_country, extract_country_crop_production(
    files=monfreda_prod_files,
    map_boundaries=worldmap_clean)
  ), 
  tar_target(crop_production_names, add_crop_production_country_name(
    data=crop_production_country,
    map_boundaries=worldmap_clean
  )),
  tar_target(multicountrycrop, unwrap_multicountry(agimpacts)),
  tar_target(multicountrydf, unlist_multicountry(multicountrycrop)),
  tar_target(s, strsplit(multicountrydf$Country, split = ",")), 
  tar_target(multicountrycrop_long, make_long_multicountry(
    data=multicountrydf,
    s=s)),
  tar_target(country_crop_rank, join_multicountry(
    data=multicountrycrop_long,
    crop_production_names=crop_production_names)
  ),
  tar_target(y, rank_country_by_crop(
    data=country_crop_rank,
    crop_production_names=crop_production_names
  )),
  tar_target(a, country_crop_ranked_df(y)),
  tar_target(multi_index, join_country_crop_ranked(
    data=multicountrydf,
    a=a)),
  tar_target(AGIMPACTS, clean_cgiar_data(agimpacts)),
  tar_target(AGIMPACTS_MAIN, join_cgiar_data_country_rank(
    data=AGIMPACTS,
    multi_index=multi_index)),
  # note that the format of these rasters has changed slightly
  # for CRU temperature country extraction, need to specify raster list index[[1]]
  tar_target(crop_production_rasters, rasterise_files(monfreda_prod_files))
  
)


# Crop calendar (Sacks and MIRCA) data prep ---------------------------------------------------------

# corresponds to script 04 in agimpacts-precip repo
targets_data_crop_calendar <- list(
  tar_target(file_sacks, nc_folder_path("Sacks data")),
  tar_target(crop_calendar_list, read_sacks(file_sacks)),
  tar_target(crop_calendar, rbind_crop_list(crop_calendar_list)),
  tar_target(crop_calendar_df, ready_crop_calendar_for_joining(crop_calendar)),
  tar_target(mirca_30mn, read_mirca_30mn(
    "CELL_SPECIFIC_CROPPING_CALENDARS_30MN.txt")),
  tar_target(mirca_df, wrangle_mirca_data(mirca_30mn)),
  tar_target(crop_season_df, create_crop_calendar(crop_calendar_df, mirca_df)),
  tar_target(raster_crop, rasterise_crop_calendar_data(crop_season_df)),
  tar_target(raster_crop_season, extend_raster_crop_season(raster_crop)),
  tar_target(crop_season_country, extract_country_crop_season(raster=raster_crop_season, 
                                                              map_boundaries=worldmap_clean)),
  tar_target(crop_season_concordance, create_crop_season_concordance(crop_season_df)),
  tar_target(crop_season_country_dt, create_dt_country_crop_season(
    list=crop_season_country,
    data=crop_season_concordance
  ))
)


# CRU temperature and precipitation data prep -----------------------------

targets_data_cru <- list(
  # first - work with CRU temperature data
  # corresponds to script 05_1 in agimpacts-precip repo
  tar_target(cru_data_tmp, read_cru_data("cru_ts4.05.1901.2020.tmp.dat.nc")),
  # this takes a long time
  tar_target(cru_country_list_tmp, extract_country_cru_data(
    raster=cru_data_tmp,
    map_boundaries=worldmap_clean,
    weights=crop_production_rasters
  )),
  tar_target(cru_country_df_tmp, create_df_country_cru_data(
    list=cru_country_list_tmp,
    map_boundaries=worldmap_clean)),
  tar_target(growing_season_tmp_production,
             prep_baseline_gs_vars(
               data1=crop_season_country_dt,
               data2=cru_country_df_tmp,
               var="tmp"
             )),
  # second - work with CRU precipitation data
  # corresponds to script 05_2 in agimpacts-precip repo
  # simply recode using same target functions as above ^_^
  tar_target(cru_data_pre, read_cru_data("cru_ts4.05.1901.2020.pre.dat.nc")),
  # this takes a long time
  tar_target(cru_country_list_pre, extract_country_cru_data(
    raster=cru_data_pre,
    map_boundaries=worldmap_clean,
    weights=crop_production_rasters
  )),
  tar_target(cru_country_df_pre, create_df_country_cru_data(
    list=cru_country_list_pre,
    map_boundaries=worldmap_clean)),
  tar_target(growing_season_pre_production,
             prep_baseline_gs_vars(
               data1=crop_season_country_dt,
               data2=cru_country_df_pre,
               var="pre"
             ))
)


