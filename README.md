---
editor_options: 
  markdown: 
    wrap: 72
---

# About this repository

This repository contains all code used to generate SSP5-RCP8.5 estimates
for the manuscript, "Predicting changes in agricultural yields from
climate change scenarios and their implications for global food
security" (April 27, 2023). The separate repositories used to estimate
outputs for SSP1-RCP2.6 and SSP2-RCP4.5 can be made available upon
request. All analysis was performed in the R.4.2.2. environment.

## Scripts

This repository uses the `targets` framework for reproducible workflow
developed by [William Landau](https://books.ropensci.org/targets/). The
script `_targets.R` describes the targets pipeline as a modular list of
smaller, nested target pipelines that are stored inside all scripts with
`targets_` prefixes in the `scripts/` folder. The target pipeline as a
whole describes the dependencies between targets (i.e. cached objects).
We recommend that users become familiar with the targets pipeline and
associated `targets` commands in the link provided, but users only need
to use `tar_make()` to run the pipeline and build targets, or
`tar_make_future()` to build targets using parallelisation. As the
targets pipeline takes care of dependencies, users can create the entire
targets pipeline with `tar_make()` or create individual targets `x` with
`tar_make(x)`.

Users should first run `scripts/init.R` to install packages and set the
relative file path dependencies to the root project folder.

## Data

The validated CGIAR dataset is saved in `processed/agimpacts_final.csv`.
Certain data inputs, such as observational and projected temperature,
precipitation and CO2 data, spatial crop production, calendar
information and food security data exceed Github's repository size
limits. Please refer to the list below and additional details in the
manuscript to download data directly from cited sources and deposit
these in the directory structure that has been set up and outlined in
the targets pipeline or contact me for assistance.

### Historical data

**Spatial production data c. 2000, 0.5 degrees/30 minutes spatial
resolution**

Files not provided: `X_Production.tiff` where X is crop
`Maize, Rice, Soybean, Wheat`

> Monfreda, C., Ramankutty, N. & Foley, J. A. Farming the planet: 2.
> Geographic distribution of crop areas, yields, physiological types,
> and net primary production in the year 2000. Global Biogeochemical
> Cycles 22 (2008).

**Spatial yields and hectares harvested data c. 2015, 5 minutes spatial
resolution**

Files not provided: `GAEZAct2015HarvArea_X_Total.tiff` and
`GAEZAct2015Yield_X_Mean.tiff` where X is crop
`Maize, Rice, Soybean, Wheat`

> Grogan, D., Frolking, S., Wisser, D., Prusevich, A. & Glidden, S.
> Global gridded crop harvested area, production, yield, and monthly
> physical area data circa 2015. Scientific Data 9. Number: 1 Publisher:
> Nature Publishing Group, 15 (2022).

**Spatial crop calendar data c. 2000, 0.5 degrees/30 minutes spatial
resolution**

Files not provided: `X.crop.calendar.fill.nc` where X is crop
`Maize, Maize.2, Rice, Rice.2, Soybeans, Wheat, Wheat.Winter`

> Sacks, W. J., Deryng, D., Foley, J. A. & Ramankutty, N. Crop planting
> dates: an analysis of global patterns. Global Ecology and Biogeography
> 19, 607--620 (2010).

File provided at
`data/MIRCA data/CELL_SPECIFIC_CROPPING_CALENDARS_30MN.txt`

> Portmann, F. T., Siebert, S. & Döll, P. MIRCA2000---Global monthly
> irrigated and rainfed crop areas around the year 2000: A new
> high-resolution data set for agricultural and hydrological modeling.
> Global Biogeochemical Cycles 24 (2010).

**Monthly spatial temperature and precipitation observations 1901--2020,
0.5 degrees/30 minutes spatial resolution**

Files not provided: `CRU TS 4.05.1901.2020.tmp.dat.nc`,
`CRU TS 4.05.1901.2020.tmp.dat`, `CRU TS 4.05.1901.2020.tmp.stn`,
`CRU TS 4.05.1901.2020.pre.dat.nc`, `CRU TS 4.05.1901.2020.pre.dat`,
`CRU TS 4.05.1901.2020.pre.stn`

> Harris, I., Osborn, T. J., Jones, P. & Lister, D. Version 4 of the CRU
> TS monthly high-resolution gridded multivariate climate dataset.
> Scientific Data 7, 109 (2020).

**Annual global CO2 observations 1959--2021**

File provided at `data/co2_annmean_mlo.csv`

> Accessed from \url{gml.noaa.gov/ccgg/trends/}.

**FAOSTAT Food Balance Sheets data 2014\--2016, country level**

Files accessed from FAOSTAT Food Balance Sheets
<https://www.fao.org/faostat/en/#data/FBS> (2023):

-   Production Quantity, Import Quantity, Export Quantity and crop
    allocation data for staple products, files provided in
    `data/Food security data/FAOSTAT_FBS_staple_201416/` folder

-   Production Quantity for staple products, file provided in
    `data/Food security data/` folder

-   Food Supply (kcal) for all products, file provided in
    `data/Food security data/` folder

-   Annual Dietary Energy Requirements, file provided in
    `data/Food security data/` folder

**FAOSTAT Population 2014\--2016 data, country level**

File provided in `data/Food security data/` folder

> FAOSTAT. Food Balance Sheets
> <https://www.fao.org/faostat/en/#data/FBS> (2023).

**FAOSTAT Detailed Trade Matrix 2014\--2016 data, country level**

Files provided in `data/Food security data/FAOSTAT_trade_data_201416`
folder

> FAOSTAT. Detailed Trade Matrix
> <https://www.fao.org/faostat/en/#data/TM> (2023).

### Future data

**CMIP6 individual GCM data, 10 minutes**

Files not provided

> WorldClim. Future climate, 10 minutes spatial resolution --- WorldClim
> 1 documentation
> <https://www.worldclim.org/data/cmip6/cmip6_clim10m.html> (2023).

**CMIP5 multi-model mean data, 0.5 degrees/30 minutes spatial
resolution**

Files not provided: `iRCP3PD_CO2_1860_2100.nc`,
`iRCP45_CO2_1860_2100.nc`, `iRCP85_CO2_1860_2100.nc`

> KNMI, C. E. Monthly CMIP5 scenario runs
> <https://climexp.knmi.nl/start.cgi> (2023).

**Population projections, country level**

File provided at `data/Food security data/iamc_db.xslx` and accessed
from the International Institute of Applied Systems Analysis (IIASA).

> KC, S. & Lutz, W. The human core of the shared socioeconomic pathways:
> Population scenarios by age, sex and level of education for all
> countries to 2100. Global Environmental Change 42, 181--192 (2017).
