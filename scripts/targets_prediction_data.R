
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

# pipeline to create prediction data


targets_prediction_data_cru <- list(
  tar_target(crop_season_extended_subset,
             manipulate_crop_season(
               data=crop_calendar, 
               raster=raster_crop_season
             )),
  # create CRU temp prediction data 
  tar_target(tmp_2011_2020_df,
             create_cru_prediction_data(
               file="cru_ts4.05.2011.2020.tmp.dat.nc",
               var_name="tmp"
             )),
  # create CRU pre prediction data
  tar_target(pre_2011_2020_df,
             create_cru_prediction_data(
               file="cru_ts4.05.2011.2020.pre.dat.nc",
               var_name="pre"
             )),
  # calculate mean annual temp for 2015
  tar_target(tmp_mean_2015,
             mean_cru_year_var(
               data=tmp_2011_2020_df, 
               var="tmp",
               var_2015=tmp_2015
             )),
  # calculate mean annual precip for 2015
  tar_target(pre_mean_2015,
             mean_cru_year_var(
               data=pre_2011_2020_df,
               var="pre",
               var_2015=pre_2015
             )),
  # calculate baseline average growing season temp and precip for each year of 2015-2020 from CRU data 
  tar_target(bs_years_tmp,
             calc_bs_years_vars(
               bs_years=bs_years,
               calc_bs_var=calc_bs_var,
               data=tmp_2011_2020_df,
               var="tmp",
               crop_season_extended_subset=crop_season_extended_subset 
             )),
  tar_target(bs_years_pre,
             calc_bs_years_vars(
               bs_years=bs_years,
               calc_bs_var=calc_bs_var,
               data=pre_2011_2020_df,
               var="pre",
               crop_season_extended_subset=crop_season_extended_subset
             )),
  tar_target(bs_years_tmp_pre,
             combine_bs_years_tmp_pre(
               data1=bs_years_tmp,
               data2=bs_years_pre
             )),
  tar_target(bs_2015_tmp_pre_list,
             extract_bs_2015_tmp_pre(
               data=bs_years_tmp_pre
             )),
  # plot baseline 2015 tmp faceted by crop
  tar_target(bs_2015_tmp_plot,
             plot_bs_2015_tmp_pre(
               data=bs_2015_tmp_pre_list,
               var="bs_gs_tmp",
               title='Temperature (°C)',
               palette='YlOrBr',
               World=World,
               outfile="processed/bs_2015_tmp.png"
             )),
  
  # plot baseline 2015 pre faceted by crop
  tar_target(bs_2015_pre_plot,
             plot_bs_2015_tmp_pre(
               data=bs_2015_tmp_pre_list,
               var="bs_gs_pre",
               title='Precipitation (mm)',
               palette='BrBG',
               World=World,
               outfile="processed/bs_2015_pre.png"
             ))
  
)


# create yield data
targets_prediction_data_monf <- list(
  tar_target(monfreda_yield_files,
             c("data/Monfreda data/maize_YieldPerHectare.tif",
               "data/Monfreda data/rice_YieldPerHectare.tif",
               "data/Monfreda data/soybean_YieldPerHectare.tif",
               "data/Monfreda data/wheat_YieldPerHectare.tif"),
             format="file"),
  # rasterise
  tar_target(crop_yield_monf_agg,
             rasterise_crop_yield_monf(
               files=monfreda_yield_files
             )),
  # convert to dt
  tar_target(crop_yield_monf_dt,
             make_crop_yield_monf_dt(
               raster=crop_yield_monf_agg
             )),
  # merge tmp, precip and yield data for baseline year 2015
  tar_target(bs_2015_tmp_pre_yld,
             merge_cru_monf_prediction_data(
               data1=bs_2015_tmp_pre_list,
               data2=crop_yield_monf_dt
             ))
)

targets_prediction_data_misc <- list(
  # add country and adapt dummy 
  tar_target(points,
             identify_points(
               data=bs_2015_tmp_pre_yld
             )),
  tar_target(coords_admin,
             coords2admin(points)),
  tar_target(coords_iso,
             coords2iso(points)),
  tar_target(coords_countries,
             combine_coords(
               data1=points,
               data2=coords_admin,
               data3=coords_iso
             )),
  tar_target(bs_2015_vars,
             create_bs_2015_vars(
               data1=bs_2015_tmp_pre_yld,
               data2=coords_countries
             ))
)


targets_prediction_data_cmip <- list(
  tar_target(time_periods,
             c("2021-2040", "2041-2060", "2061-2080", "2081-2100")),
  # get future precip projections from worldclim
  tar_target(cmip6_pre_df,
             create_cmip6_pre_df(time_periods)),
  # get future tmp projections from worldclim
  tar_target(cmip6_tmp_df,
             create_cmip6_tmp_df(time_periods)),
  # get cmip5 co2 data
  tar_target(CO2_data,
             get_CO2_data(
               file="iRCP85_CO2_1860_2100.nc")),
  # get baseline co2 data from 2015
  tar_target(CO2_2015,
             extract_CO2_2015(
               data=CO2_data)),
  tar_target(time_periods_years,
             data.frame(start_year = c(2021, 2041, 2061, 2081), 
                        end_year = c(2040, 2060, 2080, 2100))),
  # calc change in global co2 since 2015 
  tar_target(CO2_change,
             create_CO2_change(
               period=time_periods_years,
               CO2_data=CO2_data,
               CO2_2015=CO2_2015)),
  # create mean annual temp change and pre change for each future time period
  tar_target(change_vars,
             create_change_vars(
               cmip6_tmp_df=cmip6_tmp_df, 
               cmip6_pre_df=cmip6_pre_df, 
               tmp_mean_2015=tmp_mean_2015, 
               pre_mean_2015=pre_mean_2015
             )),
  # create final prediction set
  tar_target(prediction_data,
             create_prediction_data(
               bs_2015_vars=bs_2015_vars, 
               change_vars=change_vars, 
               CO2_change=CO2_change
             )),

  # name GCMs
  
  tar_target(GCMs,
             c("ACCESS-CM2","ACCESS-ESM1-5", 
               "BCC-CSM2-MR", # this one is incorrect and will need to be excluded
               "CanESM5",
               "CanESM5-CanOE","CMCC-ESM2","CNRM-CM6-1","CNRM-CM6-1-HR", 
               "CNRM-ESM2-1","EC-Earth3-Veg","EC-Earth3-Veg-LR",
               "FIO-ESM-2-0","GISS-E2-1-G","GISS-E2-1-H",
               "HadGEM3-GC31-LL","INM-CM4-8","INM-CM5-0","IPSL-CM6A-LR",
               "MIROC-ES2L","MIROC6","MPI-ESM1-2-HR","MPI-ESM1-2-LR",
               "MRI-ESM2-0","UKESM1-0-LL")),
  

  # clean prediction data for complete cases
  # this results in ~63k rows per batch
  tar_target(prediction_data_complete_cases,
             clean_prediction_data(
               data=prediction_data
             )),

  # write out output files for input into cloned RCP 4.5 and 2.6 projects
  tar_target(write_tmp_mean_2015, 
             saveRDS(tmp_mean_2015, "processed/tmp_mean_2015.rds")),
  tar_target(write_pre_mean_2015,
             saveRDS(pre_mean_2015, "processed/pre_mean_2015.rds")),
  tar_target(write_bs_2015_vars,
             saveRDS(bs_2015_vars, "processed/bs_2015_vars.rds")),
  tar_target(write_coords_countries,
             saveRDS(coords_countries, "processed/coords_countries.rds")),
  tar_target(write_World,
             saveRDS(World, "processed/World.rds")),
  tar_target(write_crops,
             saveRDS(crops, "processed/crops.rds")),
  # this is from targets_prediction.R but has already been built
  tar_target(write_data_current,
             saveRDS(data_current, "processed/data_current.rds")),
  # this is from targets_data.R but has already been built
  tar_target(write_crop_production_rasters,
             saveRDS(crop_production_rasters, "processed/crop_production_rasters.rds")),
  tar_target(write_worldmap_clean,
             saveRDS(worldmap_clean, "processed/worldmap_clean.rds"))
  
  
)



