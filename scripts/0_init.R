
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
              "forestplot", "metafor", "itsadug", "renv", "targets", "stringr")
ipak(packages)


# SET RELATIVE PATH -------------------------------------------------------

here::i_am("README.md")

here()


# INITIALISE PACKAGES ENVIRONMENT -----------------------------------------

renv::init()

renv::snapshot()
