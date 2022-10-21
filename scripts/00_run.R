
# Run script for agimpacts project

# PACKAGES ----------------------------------------------------------------

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("rvest", "dplyr", "tidyr", "janitor", "forecast","data.table", "ggplot2","readxl", 
              "Hmisc", "zoo", "lubridate", "networkD3", "viridis", "maptools","ggmap", 
              "maps", "sp", "sf", "geojsonio",  "rgdal", "broom", "plotly", "htmlwidgets", "gridExtra",
              "raster", "gridBase", "ggthemes", "grid", "tidync", "ncmeta","stars", "devtools", 
              "RNetCDF", "here", "rworldmap", "R.utils", "stringr", "compareDF", "utils",
              "ncdf4", "mice", "miceadds", "Rcpp", "VIM", "cvms", "groupdata2", "exactextractr",
              "cleangeo", "rworldxtra", "rasterize", "ggeffects", "ggExtra", "GGally",
              "forestplot", "metafor")
ipak(packages)


# SET RELATIVE PATH -------------------------------------------------------

here::i_am("README.md")

here()

# here() starts at C:/Users/chris/OneDrive - The University of Melbourne/Damages/agimpacts

# PRELIMINARIES -----------------------------------------------------------
# Control which scripts run
run_01_cgiar_data_scrape <- 1
run_02_cgiar_data_clean_validate <- 1
run_03_monfreda_data_prep <- 1
run_04_sacks_mirca_data_prep <- 1
run_05_cru_data_prep <- 1
run_06_gamm_calc_baseline_temp <- 1
run_07_plot_missing_data <- 1
run_08_impute_data <- 1
run_09_meta_analysis <- 1
run_10_model_cross_validation <- 1
run_11_model_fit <- 1
run_12_plot_regression_results <- 1
run_13_cmip5_prediction_data <- 1
run_14_predict_gridded <- 1
run_15_predict_country <- 1
run_16_map_to_gtap <- 1

run_x6_ols_calc_baseline_temp <- 0
run_x7_ols_model_fit <- 0
run_x8_ols_plot_regression_results <- 0
run_x9_predict_gridded <- 0

run_SI_soybean_oilseed_share <- 0
run_SI_roson_sartori <- 0


# RUN SCRIPTS -----------------------------------------------------------

# 01 Scrape point estimate data from CGIAR website
if (run_01_cgiar_data_scrape) source(here("scripts", "01_cgiar_data_scrape.R"), encoding = "UTF-8")
# INPUTS
# http://www.ag-impacts.org
# OUTPUTS
here("processed", "agimpacts_full.csv")
here("processed", "agimpacts_compact.csv")

# 02 Run data validation workbook (following manual review to replace and update data) with some data cleaning
if (run_02_cgiar_data_prep) source(here("scripts", "02_cgiar_data_prep.R"), encoding = "UTF-8")
# INPUTS
here("processed", "agimpacts_full.csv")
here("data", "Agimpacts Data Validation.xlsx")
# OUTPUTS
here("processed", "agimpacts_final.csv") # git ignored, too large

# 03 Extract country level production data, and assign country with highest production in list to CGIAR multi-country point estimates
if (run_03_monfreda_data_prep) source(here("scripts", "03_monfreda_data_prep.R"), encoding = "UTF-8")
# INPUTS
here("processed", "agimpacts_final.csv")
here("data", "Monfreda data") # subfolder containing 4 TIF files
# OUTPUTS
# AGIMPACTS_MAIN 03 # this was not written to csv 

# 04 Load and wrangle Sachs and MIRCA crop calendar data; append and create crop seasons/growing months
if (run_04_sacks_mirca_data_prep) source(here("scripts", "04_sacks_mirca_data_prep.R"), encoding = "UTF-8")
# INPUTS
here("data", "Sacks data") # SACHS
here("data", "MIRCA data", "CELL_SPECIFIC_CROPPING_CALENDARS_30MN.txt") # MIRCA
# OUTPUTS
here("processed", "crop_season_04.csv")

# 05 Calculate baseline temperature
if (run_05_cru_data_prep) source(here("scripts", "05_cru_data_prep.R"), encoding = "UTF-8")
# INPUTS
# AGIMPACTS_MAIN from 03
here("processed", "crop_season_04.csv")
# OUTPUTS
# AGIMPACTS_MAIN 05
here("processed", "crop_season.csv")
here("processed", "tmp_1950_2020_crop_production.csv")

########################################################################################################
# From here, we branch into the OLS and extension parts of the project.
# Rather than branching with git, we name and run both OLS and extension scripts in the main branch.

########################################################################################################
# GAMM BRANCH 
########################################################################################################

# 06 Finish calculating baseline temperature with imputation for GAMM branch
if (run_06_gamm_calc_baseline_temp) source(here("scripts", "06_gamm_calc_baseline_temp.R"), encoding = "UTF-8")
# INPUTS
# AGIMPACTS_MAIN from 05
# OUTPUTS
here("processed", "AGIMPACTS_MAIN_INCOMPLETE.csv")

# 07 Graph patterns of missingness and correlations between data variables before imputation
if (run_07_plot_missing_data) source(here("scripts", "07_plot_missing_data.R"), encoding = "UTF-8")
# INPUTS
here("processed", "AGIMPACTS_MAIN_INCOMPLETE.csv")
# AGIMPACTS_MAIN from 06
# OUTPUTS
here("results", "figures", "correlations-reweighted.png") # find code that generated this
here("results", "figures", "missing*") # png files

# 08 Impute data for GAMM branch and create data sets near final for modelling
if (run_08_impute_data) source(here("scripts", "07_impute_data.R"), encoding = "UTF-8")
# INPUTS
here("processed", "AGIMPACTS_MAIN_INCOMPLETE.csv")
# OUTPUTS
here("processed", paste0("crop_data_", crops[[j]], "_", i, ".csv")) # 20 datasets, for 4 crops x 5 imputed datasets

# 09 Meta-analysis 
if (run_09_meta_analysis) source(here("scripts", "09_meta_analysis.R"), encoding = "UTF-8")
# INPUTS
here("processed", paste0("crop_data_", crops[[j]], "_", i, ".csv")) # 20 datasets, for 4 crops x 5 imputed datasets
# OUTPUTS
sink(here("results", "text files")) # 4 files
here("results", "figures", "meta_m1_var_comp_crops.png")

# 10 K-fold cross validation for different model specifications to see which model has lowest RMSE
if (run_10_model_cross_validation) source(here("scripts", "10_model_cross_validation.R"), encoding = "UTF-8")
# INPUTS
here("processed", paste0("crop_data_", crops[[j]], "_", i, ".csv")) # 20 datasets, for 4 crops x 5 imputed datasets # note same as 09
# OUTPUTS
here("results", "tables") # 5 files

# 11 Fit model regression only
if (run_11_model_fit) source(here("scripts", "11_model_fit.R"), encoding = "UTF-8")
# INPUTS
# meta_m1_data_crops_restricted from 09
# OUTPUTS
# fit_weighted_[[crops]]_restricted

# 12 Plot regression results - 1) diagnostics, 2) response functions, 3) regression tables
if (run_12_plot_regression_results) source(here("scripts", "12_plot_regression_results.R"), encoding = "UTF-8")
# INPUTS
# fit_weighted_restricted_m1_select from 11
# fit_weighted_restricted_list from 11
# crop specific fits from 11
# AGIMPACTS_FINAL_INCOMPLETE from 08
# OUTPUTS
here("results", "figures")
here("results", "text files", "summary_fit_m1_reweighted.txt")

# 13 Create prediction data 
if(run_13_cmip5_prediction_data) source(here("scripts", "13_cmip5_prediction_data.R"), encoding = "UTF-8")
# INPUTS
# raster_extended_crop_season from 05
# crop_season from 05
# crop_calendar from 04
here("data", "cmip5_tas_Amon_modmean_rcp85_1_mean_ave.nc")
here("data", "global-mean-warming-mod-mean-tas-rcp85.csv")
here("data", "cmip5_tas_Amon_modmean_rcp45_1_mean_ave.nc")
here("data", "global-mean-warming-mod-mean-tas-rcp45.csv")
here("data", "iRCP85_CO2_1860_2100.nc")
here("data", "iRCP45_CO2_1860_2100.nc")
here("data", "cru_ts4.05.2011.2020.tmp.dat.nc")
# OUTPUTS
# crops_prediction_data # not outputted to csv
here("processed", paste0("avg_baseline_tmps_2015_2020_", i, ".csv"))
here("processed", "baseline_tmp_grid_df_2015_2020.csv")
here("processed", paste0("crops_prediction_data_", crops[[j]], "_", i, ".csv")) # prediction data in 48 datasets, 4 lists of 12

# 14 Predict global gridded predictions, pooled predictions and confidence intervals
if(run_14_predict_gridded) source(here("scripts", "14_predict_gridded.R"), encoding = "UTF-8")
# INPUTS
here("processed", paste0("crops_prediction_data_", crops[[j]], "_", i, ".csv")) # prediction data in 48 datasets, 4 lists of 12
# fit_weighted_restricted_list from 11
# OUTPUTS
here("processed", paste0("pooled_predictions_", crops[[i]], ".csv"))
here("results", "figures")

# 15 Predict pooled country production-weighted-average predictions and confidence intervals
if(run_15_predict_country) source(here("scripts", "15_predict_country.R"), encoding = "UTF-8")
# INPUTS
here("processed", paste0("pooled_predictions_", crops[[i]], ".csv"))
# OUTPUTS
here("results", "figures")

# 16 Map country predictions to GTAP regions
if(run_16_map_to_gtap) source(here("scripts", "16_map_to_gtap.R"), encoding = "UTF-8")
# INPUTS
here("data", "GTAP data", "mapping.csv")
here("processed", country_results_csv[[i]])
# OUTPUTS
here("processed", paste0("gtap_region_", crops[[i]], ".csv"))


########################################################################################################
# OLS BRANCH 
########################################################################################################

# 06 Finish calculating baseline temperature without imputation for OLS branch
if (run_x6_ols_calc_baseline_temp) source(here("scripts", "x6_ols_calc_baseline_temp.R"), encoding = "UTF-8")
# INPUTS
# AGIMPACTS_MAIN from 05
# OUTPUTS 
here("processed", "agimpacts_main_completed_weighted_v2.csv")
# OUTPUTS

# 07 Fit OLS model and model specification variations including creating new datasets
if (run_x7_ols_model_fit) source(here("scripts", "x7_ols_model_fit.R"), encoding = "UTF-8")

# 08 Plot OLS response functions, diagnostics plots and regression tables
if (run_x8_ols_plot_regression_results) source(here("scripts", "x8_ols_plot_regression_results.R"), encoding = "UTF-8")

# 09 Create bootstrapped global gridded predictions, including central and 95% estimates
if (run_x9_predict_gridded) source(here("scripts", "x9_ols_predict_gridded.R"), encoding = "UTF-8")


########################################################################################################
# Supplementary Information 
########################################################################################################

# Calculate production of soybeans as a share of oilseeds sector for each country
if (run_SI_soybean_oilseed_share) source(here("scripts", "SI_soybean_oilseed_share.R"), encoding = "UTF-8")

# Rasterise Roson and Sartori figures
if (run_SI_roson_sartori) source(here("scripts", "SI_roson_sartori.R"), encoding = "UTF-8")