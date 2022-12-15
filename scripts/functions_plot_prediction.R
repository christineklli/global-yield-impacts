# rasterise
# note that the order of models here is not the order defined in model_specs
# instead, they are in alphabetical order
# so: gam_RI, gam_RS, glm_RI, glm_RS, lm

rasterise_predictions_gridded <- function(predictions, time_periods){ # predictions_nested - this produces a raster stack?
  lapply(1:5, function(model){ 
    lapply(1:4, function(crop){  
      
      x <- predictions[[model]][[crop]]
      
      x_list <- lapply(1:4, function(i){
        
        r <- x[x$time_period==time_periods[[i]], c("lon", "lat", "pred_bar")] 
        r <- raster::rasterFromXYZ(r)
        crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
        names(r) <- time_periods[[i]]
        r
      })
      
      raster::stack(x_list)
      
      })
      
      # these don't span entire globe? ymin -55 only not -90!!?
      # terra::rast(r_2021_2040, "xyz") throws an error
      #https://future.futureverse.org/articles/future-4-non-exportable-objects.html#packages-that-rely-on-external-pointers
    }) 
  
  
}


plot_predictions_gridded <- function(predictions, returnStack, World){
  lapply(1:5, function(model){
    lapply(1:4, function(crop){
    
      tmap::tmap_mode("plot")
      
      
      plot <- tmap::tm_shape(predictions[[model]][[crop]]) + 
        tmap::tm_raster("pred_bar", title="Pooled fit (%)",
                  style = "cont",
                  breaks=seq(-100,100,10), 
                  # ^ this is chosen manually to disregard skewing/scaling effects of Siwa desert outliers
                  # remove range restriction when we remove outliers in the prediction data
                  palette = rev(terrain.colors(100))) +
        tmap::tm_shape(World) +
        tmap::tm_borders("grey", lwd =1) + 
        tmap::tm_layout(panel.labels = c('2021-2040', '2041-2060', '2061-2080', '2081-2100'))
 
      # define filename
      outfile <- sprintf("results/figures/predictions_gridded/predictions_gridded_%s_%s.png", 
                         paste0("model_", model),
                         paste0("crop_", crop))
      
      # save
      tmap::tmap_save(plot, filename=outfile, height=4, width=10, asp=0)
      
      # show predictions again
      if(isTRUE(returnStack)) {
        plot
      } else {
        outfile
      }
      
    })
  })
}


# aggregate to country


create_crop_production_raster_agg <- function(crop_production_rasters){
  lapply(1:4, function(i){
    
    r <- raster::raster(xmn=-180, xmx=180, ymn=-90, ymx=90, crs=4326, res=0.5)
    res.factor <- raster::res(r)/raster::res(crop_production_rasters[[1]]) # 0.5 0.5 / 0.083 0.083 
    raster::aggregate(crop_production_rasters[[i]], fact = res.factor, fun = sum)
  })
}


create_country_predictions_tbl <- function(predictions, crop_production_raster_agg, worldmap_clean){
  lapply(1:5, function(model){ #
    lapply(1:4, function(crop){ # time period
      
      prediction <- exactextractr::exact_extract(predictions[[model]][[crop]], # this is a stack
                                                 worldmap_clean, 
                                                 'weighted_mean', 
                                                 weights = crop_production_raster_agg[[crop]]) 
      
      data.frame(ADMIN=worldmap_clean@data$ADMIN,
                 ISO_A2=worldmap_clean@data$ISO_A2,
                 prediction)
      
    }) 
  }) 
}

# create rasters of the country weighted average
# then plot and save

plot_country_predictions <- function(predictions, worldmap_clean, crop_production_raster_agg, World, returnStack){
  lapply(1:5, function(model){ #
    lapply(1:4, function(crop){ # time period
      
      prediction <- exactextractr::exact_extract(predictions[[model]][[crop]], # this is a stack
                                                 worldmap_clean, 
                                                 'weighted_mean', 
                                                 weights = crop_production_raster_agg[[crop]]) 
      
      worldmap_clean@data <- worldmap_clean@data %>% cbind(prediction)
      
      r_object <- raster(res = c(0.5, 0.5), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
      
      p_2021_2040 <- rasterize(worldmap_clean, r_object, field = "weighted_mean.X2021.2040")
      p_2041_2060 <- rasterize(worldmap_clean, r_object, field = "weighted_mean.X2041.2060")
      p_2061_2080 <- rasterize(worldmap_clean, r_object, field = "weighted_mean.X2061.2080")
      p_2081_2100 <- rasterize(worldmap_clean, r_object, field = "weighted_mean.X2081.2100")
      
      country_pred <- stack(p_2021_2040,
                            p_2041_2060,
                            p_2061_2080,
                            p_2081_2100)
      
      plot <- tmap::tm_shape(country_pred) + 
        tmap::tm_raster("pred_bar", title="Pooled fit (%)",
                        style = "cont",
                        breaks=seq(-100,100,10), 
                        # ^ this is chosen manually to disregard skewing/scaling effects of Siwa desert outliers
                        # remove range restriction when we remove outliers in the prediction data
                        palette = rev(terrain.colors(100))) +
        tmap::tm_shape(World) +
        tmap::tm_borders("grey", lwd =1) + 
        tmap::tm_layout(panel.labels = c('2021-2040', '2041-2060', '2061-2080', '2081-2100'))
      
      # define filename
      outfile <- sprintf("results/figures/predictions_country_weighted/predictions_country_weighted_%s_%s.png", 
                         paste0("model_", model),
                         paste0("crop_", crop))
      
      # save
      tmap::tmap_save(plot, filename=outfile, height=4, width=10, asp=0)
      
      # show predictions again
      if(isTRUE(returnStack)) {
        plot
      } else {
        outfile
      }
      
    }) 
  }) 
}