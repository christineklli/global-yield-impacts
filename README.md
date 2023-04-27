# About this repository
This repository contains all code used to generate SSP5-RCP8.5 estimates for the manuscript, "Predicting changes in agricultural yields from climate change scenarios and their implications for global food security" (April 27, 2023). All analysis was performned in the R.4.2.2. environment.

## Scripts

This repository uses the `targets` framework for reproducible workflow developed by [William Landau](https://books.ropensci.org/targets/). The script `_targets.R` describes the targets pipeline as a modular list of smaller, nested target pipelines that are stored inside all scripts with `targets_` prefixes in the `scripts/` folder. The target pipeline as a whole describes the dependencies between targets (i.e. cached objects). 
We recommend that users become familiar with the targets pipeline and associated `targets` commands in the link provided, but users only need to use `tar_make()` to run the pipeline and build targets, or `tar_make_future()` to build targets using parallelisation. As the targets pipeline takes care of dependencies, users can create the entire targets pipeline with `tar_make()` or create individual targets `x` with `tar_make(x)`. 

Users should first run `scripts/init.R` to install packages and set the relative file path dependencies to the root project folder.

## Data
The validated CGIAR dataset is saved in `processed/agimpacts_final.csv`. Certain data inputs, such as observational and projected temperature, precipitation and CO2 data, spatial crop production, calendar information and food security data exceed Github's repository size limits. Please refer to the manuscript to download data directly from cited sources and deposit these in the directory structure that has been set up and outlined in the targets pipeline or contact me for assistance. 

## SSP1-RCP2.6 and SSP2-RCP4.5
The separate repositories used to estimate outputs for SSP1-RCP2.6 and SSP2-RCP4.5 can be made available upon request. 