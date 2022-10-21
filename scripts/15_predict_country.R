
# READ IN POOLED PREDICTIONS DATA----------------------------------------------

crops <- c("Maize", "Rice", "Soybean", "Wheat")


pooled_predictions <- lapply(1:4, function(i){
  
  readr::read_csv(here("processed", paste0("pooled_predictions_re_", crops[[i]], ".csv")),
  col_types = readr::cols(.default = "c"))})


pooled_predictions <- lapply(1:4, function(i){
  
  pooled_predictions[[i]][,c(1:18)] <- lapply(pooled_predictions[[i]][,c(1:18)], as.numeric)
  
  pooled_predictions[[i]]
  
})



# READ IN CROP PRODUCTION STACK DATA --------------------------------------

# Go to script 03 and run lines 41-48, or here:

crop_production_files <- c("maize_Production.tif",
                           "rice_Production.tif",
                           "soybean_Production.tif",
                           "wheat_Production.tif")


crop_production_files2 <- here("data", "Monfreda data", crop_production_files)


# Go to script 05 and run line 126-129, or here:

crop_production_rasters <- lapply(crop_production_files2, raster)

# stack em
crop_production_stack <- stack(crop_production_rasters)


r_object <- raster(res = c(0.5, 0.5), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")


# predict at country level ------------------------------------------------

# run no_re and re versions dynamically, change output file name only
# careful - csv files embedded in function
# Save as csv files separately otherwise will be overwritten

pooled_country_dc <- function(i){
  
  raster_0dc <- raster(ncol = 720, nrow = 360) 
  values(raster_0dc) <-  pooled_predictions[[i]][["mean_y_0"]]
  
  raster_1dc <- raster(ncol = 720, nrow = 360) 
  values(raster_1dc) <- pooled_predictions[[i]][["mean_y_1"]]
  
  raster_2dc <- raster(ncol = 720, nrow = 360) 
  values(raster_2dc) <- pooled_predictions[[i]][["mean_y_2"]]
  
  raster_3dc <- raster(ncol = 720, nrow = 360) 
  values(raster_3dc) <- pooled_predictions[[i]][["mean_y_3"]]
  
  raster_4dc <- raster(ncol = 720, nrow = 360) 
  values(raster_4dc) <- pooled_predictions[[i]][["mean_y_4"]]
  
  raster_5dc <- raster(ncol = 720, nrow = 360) 
  values(raster_5dc) <- pooled_predictions[[i]][["mean_y_5"]]
  
  raster_alldc_stack <- stack(raster_0dc, raster_1dc, raster_2dc, raster_3dc, raster_4dc, raster_5dc)
  
  
  predicted_yield_all_dc_country <- exactextractr::exact_extract(raster_alldc_stack, worldmap_clean, 'weighted_mean', weights = crop_production_stack[[i]])
  
  predicted_yield_all_dc_country <- predicted_yield_all_dc_country %>% rename("0 degrees" = 1, "1 degree" = 2, "2 degrees" = 3,
                                                                              "3 degrees" = 4, "4 degrees" = 5, "5 degrees" = 6)
  
  worldmap_copy <- worldmap
  worldmap_copy@data <- worldmap_copy@data %>% cbind(predicted_yield_all_dc_country)
  
  predicted_yield_all_dc_country %>% 
    cbind(worldmap_copy@data$NAME) %>%
    cbind(worldmap_copy@data$ADMIN.1) %>% 
    cbind(worldmap_copy@data$ISO_A2) %>% 
    rename("Country" = 7,
           "Admin" = 8,
           "ISO_A2" = 9) %>% write.csv(here("processed", paste0("predicted_yield_country_no_re", "_", crops[[i]], ".csv")))
  
  predicted_yield_country_0dc <- rasterize(worldmap_copy, r_object, field = "0 degrees")
  predicted_yield_country_1dc <- rasterize(worldmap_copy, r_object, field = "1 degree")
  predicted_yield_country_2dc <- rasterize(worldmap_copy, r_object, field = "2 degrees")
  predicted_yield_country_3dc <- rasterize(worldmap_copy, r_object, field = "3 degrees")
  predicted_yield_country_4dc <- rasterize(worldmap_copy, r_object, field = "4 degrees")
  predicted_yield_country_5dc <- rasterize(worldmap_copy, r_object, field = "5 degrees")
  
  predicted_yield_country_alldc_raster <- stack(predicted_yield_country_0dc,
                                                predicted_yield_country_1dc,
                                                predicted_yield_country_2dc,
                                                predicted_yield_country_3dc,
                                                predicted_yield_country_4dc,
                                                predicted_yield_country_5dc)
  
  rasterVis::levelplot(predicted_yield_country_alldc_raster, 
                       col.regions = rev(terrain.colors(10000)),
                      #  at = seq(-100,100),
                       names.attr = c("0 degrees warming",
                                      "1 degree warming",
                                      "2 degrees warming",
                                      "3 degrees warming",
                                      "4 degrees warming",
                                      "5 degrees warming"))
  
}


pooled_country_predictions <- lapply(1:4, pooled_country_dc)


lapply(1:4, function(i){
  
  mypath <- file.path(here("results", "figures"),
                      paste("country_pooled_predictions", "re", crops[[i]], ".png", sep = "_"))
  
  png(file = mypath)
  
  plot(pooled_country_predictions[[i]])
  
  dev.off()
  
})




# 2.5% percentile
pooled_country_conf_lwr_dc <- function(i){
  
  raster_0dc <- raster(ncol = 720, nrow = 360) 
  values(raster_0dc) <-  pooled_predictions[[i]][["conf_lwr_0"]]
  
  raster_1dc <- raster(ncol = 720, nrow = 360) 
  values(raster_1dc) <- pooled_predictions[[i]][["conf_lwr_1"]]
  
  raster_2dc <- raster(ncol = 720, nrow = 360) 
  values(raster_2dc) <- pooled_predictions[[i]][["conf_lwr_2"]]
  
  raster_3dc <- raster(ncol = 720, nrow = 360) 
  values(raster_3dc) <- pooled_predictions[[i]][["conf_lwr_3"]]
  
  raster_4dc <- raster(ncol = 720, nrow = 360) 
  values(raster_4dc) <- pooled_predictions[[i]][["conf_lwr_4"]]
  
  raster_5dc <- raster(ncol = 720, nrow = 360) 
  values(raster_5dc) <- pooled_predictions[[i]][["conf_lwr_5"]]
  
  raster_alldc_stack <- stack(raster_0dc, raster_1dc, raster_2dc, raster_3dc, raster_4dc, raster_5dc)
  
  
  predicted_yield_all_dc_country <- exactextractr::exact_extract(raster_alldc_stack, worldmap_clean, 'weighted_mean', weights = crop_production_stack[[i]])
  
  predicted_yield_all_dc_country <- predicted_yield_all_dc_country %>% rename("0 degrees" = 1, "1 degree" = 2, "2 degrees" = 3,
                                                                              "3 degrees" = 4, "4 degrees" = 5, "5 degrees" = 6)
  
  worldmap_copy <- worldmap
  worldmap_copy@data <- worldmap_copy@data %>% cbind(predicted_yield_all_dc_country)
  
  predicted_yield_country_0dc <- rasterize(worldmap_copy, r_object, field = "0 degrees")
  predicted_yield_country_1dc <- rasterize(worldmap_copy, r_object, field = "1 degree")
  predicted_yield_country_2dc <- rasterize(worldmap_copy, r_object, field = "2 degrees")
  predicted_yield_country_3dc <- rasterize(worldmap_copy, r_object, field = "3 degrees")
  predicted_yield_country_4dc <- rasterize(worldmap_copy, r_object, field = "4 degrees")
  predicted_yield_country_5dc <- rasterize(worldmap_copy, r_object, field = "5 degrees")
  
  predicted_yield_country_alldc_raster <- stack(predicted_yield_country_0dc,
                                                predicted_yield_country_1dc,
                                                predicted_yield_country_2dc,
                                                predicted_yield_country_3dc,
                                                predicted_yield_country_4dc,
                                                predicted_yield_country_5dc)
  
  rasterVis::levelplot(predicted_yield_country_alldc_raster, 
                       col.regions = rev(terrain.colors(10000)),
                      # at = seq(-100,100),
                       names.attr = c("0 degrees warming",
                                      "1 degree warming",
                                      "2 degrees warming",
                                      "3 degrees warming",
                                      "4 degrees warming",
                                      "5 degrees warming"))
  
}


pooled_country_predictions_conf_lwr <- lapply(1:4, pooled_country_conf_lwr_dc)


plot_country_pooled_predictions_conf_lwr <- function(i){
  
  mypath <- file.path(here("results", "figures"),
                      paste("country_pooled_predictions_conf_lwr_reweighted", crops[[i]], ".png", sep = "_"))
  
  png(file = mypath)
  
  plot(pooled_country_predictions_conf_lwr[[i]])
  
  dev.off()
  
}

country_pooled_predictions_conf_lwr_plots <- lapply(1:4, plot_country_pooled_predictions_conf_lwr)


# 97.5% percentile
pooled_country_conf_upr_dc <- function(i){
  
  raster_0dc <- raster(ncol = 720, nrow = 360) 
  values(raster_0dc) <-  pooled_predictions[[i]][["conf_upr_0"]]
  
  raster_1dc <- raster(ncol = 720, nrow = 360) 
  values(raster_1dc) <- pooled_predictions[[i]][["conf_upr_1"]]
  
  raster_2dc <- raster(ncol = 720, nrow = 360) 
  values(raster_2dc) <- pooled_predictions[[i]][["conf_upr_2"]]
  
  raster_3dc <- raster(ncol = 720, nrow = 360) 
  values(raster_3dc) <- pooled_predictions[[i]][["conf_upr_3"]]
  
  raster_4dc <- raster(ncol = 720, nrow = 360) 
  values(raster_4dc) <- pooled_predictions[[i]][["conf_upr_4"]]
  
  raster_5dc <- raster(ncol = 720, nrow = 360) 
  values(raster_5dc) <- pooled_predictions[[i]][["conf_upr_5"]]
  
  raster_alldc_stack <- stack(raster_0dc, raster_1dc, raster_2dc, raster_3dc, raster_4dc, raster_5dc)
  
  
  predicted_yield_all_dc_country <- exactextractr::exact_extract(raster_alldc_stack, worldmap_clean, 'weighted_mean', weights = crop_production_stack[[i]])
  
  predicted_yield_all_dc_country <- predicted_yield_all_dc_country %>% rename("0 degrees" = 1, "1 degree" = 2, "2 degrees" = 3,
                                                                              "3 degrees" = 4, "4 degrees" = 5, "5 degrees" = 6)
  
  worldmap_copy <- worldmap
  worldmap_copy@data <- worldmap_copy@data %>% cbind(predicted_yield_all_dc_country)
  
  predicted_yield_country_0dc <- rasterize(worldmap_copy, r_object, field = "0 degrees")
  predicted_yield_country_1dc <- rasterize(worldmap_copy, r_object, field = "1 degree")
  predicted_yield_country_2dc <- rasterize(worldmap_copy, r_object, field = "2 degrees")
  predicted_yield_country_3dc <- rasterize(worldmap_copy, r_object, field = "3 degrees")
  predicted_yield_country_4dc <- rasterize(worldmap_copy, r_object, field = "4 degrees")
  predicted_yield_country_5dc <- rasterize(worldmap_copy, r_object, field = "5 degrees")
  
  predicted_yield_country_alldc_raster <- stack(predicted_yield_country_0dc,
                                                predicted_yield_country_1dc,
                                                predicted_yield_country_2dc,
                                                predicted_yield_country_3dc,
                                                predicted_yield_country_4dc,
                                                predicted_yield_country_5dc)
  
  rasterVis::levelplot(predicted_yield_country_alldc_raster, 
                       col.regions = rev(terrain.colors(10000)),
                      # at = seq(-100,100),
                       names.attr = c("0 degrees warming",
                                      "1 degree warming",
                                      "2 degrees warming",
                                      "3 degrees warming",
                                      "4 degrees warming",
                                      "5 degrees warming"))
  
}


pooled_country_predictions_conf_upr <- lapply(1:4, pooled_country_conf_upr_dc)


plot_country_pooled_predictions_conf_upr <- function(i){
  
  mypath <- file.path(here("results", "figures"),
                      paste("country_pooled_predictions_conf_upr_reweighted", crops[[i]], ".png", sep = "_"))
  
  png(file = mypath)
  
  plot(pooled_country_predictions_conf_upr[[i]])
  
  dev.off()
  
}

country_pooled_predictions_conf_upr_plots <- lapply(1:4, plot_country_pooled_predictions_conf_upr)
