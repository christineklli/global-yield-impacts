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


# CGIAR and Monfreda data prep --------------------------------------------

targets_data_production <- list(
  tar_target(ag, "processed/agimpacts_final.csv", format = "file"),
  tar_target(agimpacts, get_data_from_csv(ag)),

  tar_target(monfreda_prod_files, c("data/Monfreda data/maize_Production.tif",
                                    "data/Monfreda data/rice_Production.tif",
                                    "data/Monfreda data/soybean_Production.tif",
                                    "data/Monfreda data/wheat_Production.tif"), format = "file"),
  tar_target(worldmap, rworldmap::getMap(resolution = "coarse")),
  tar_target(worldmap_clean, cleangeo::clgeo_Clean(worldmap)),
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

  tar_target(crop_production_rasters, rasterise_files(monfreda_prod_files))
  
)


# Crop calendar (Sacks and MIRCA) data prep ---------------------------------------------------------

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

targets_data_tmp_precip <- list(
  tar_target(cru_data_tmp, read_cru_data("cru_ts4.05.1901.2020.tmp.dat.nc")),
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
 
  tar_target(cru_data_pre, read_cru_data("cru_ts4.05.1901.2020.pre.dat.nc")),

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
             )),
  # calculate baseline cgiar temperature & precipitation data

  tar_target(baseline_periods, filter_baseline_periods_cgiar(
    data=AGIMPACTS_MAIN
  )),
  # calculate cgiar bs avg tmp
  tar_target(baseline_avg_tmp, create_dt_baseline_gs_var(
    data=baseline_periods,
    var="tmp",
    gs_production_data=growing_season_tmp_production
  )),
  # calculate cgiar bs avg pre
  tar_target(baseline_avg_pre, create_dt_baseline_gs_var(
    data=baseline_periods,
    var="pre",
    gs_production_data=growing_season_pre_production
  )),
  tar_target(country_production_volume, create_crop_country_volume_df(
    raster=crop_production_rasters,
    map_boundaries=worldmap_clean
  )),
  tar_target(AGIMPACTS_bs_tp,
             join_baseline_gs_vars_to_cgiar(
               data1=AGIMPACTS_MAIN,
               data2=baseline_avg_tmp,
               data3=baseline_avg_pre,
               data4=country_production_volume
             ))
)


# yields data prep and joining with cgiar data --------------------------------------------------------

targets_data_yields <- list(
  tar_target(country_yields_list, extract_country_yields(
    folder="GDHY data",
    map_boundaries=worldmap_clean)
  ),
  tar_target(crop_yields_dt, wrangle_country_yields(country_yields_list)),
  tar_target(AGIMPACTS_bs, join_crop_yields_cgiar(
    data1=AGIMPACTS_bs_tp,
    data2=crop_yields_dt
  ))
)


# impute missing data -----------------------------------------------------

# final data related step

targets_impute <- list(
  tar_target(imputed_data, impute_data(data=AGIMPACTS_bs)), 
 
  tar_target(imputed_plots, plot_imputed_data(
    imp=imputed_data,
    path1="results/figures/imp_convergence_plot.png"

  ), format="file"),

  tar_target(crop_list_imp, wrangle_imputed_data(imputed_data)),
  tar_target(crop_imputed_data, coalesce_imputed_data(crop_list_imp)),
  tar_target(crop_imputed_rst_data, clean_imputed_data(crop_imputed_data))
)


