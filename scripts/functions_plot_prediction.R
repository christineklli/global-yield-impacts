# rasterise

rasterise_predictions_gridded <- function(predictions){ # predictions_gridded
  lapply(1:4, function(j){ # crop
  lapply(1:4, function(l,j){ # time period
    xyz <- predictions[[j]][[l]][c(1:2,4)] # lon, lat, pred_bar # reselect pre_bar column index
    raster::rasterFromXYZ(xyz) 
  }, j) # time period
}) 
}

# plot gridded

# vary this manually, dynamically to plot individual plot
plot_predictions_gridded_basic <- function(raster){
  lapply(1:4, function(j){
    lapply(1:4, function(l,j){
      rasterVis::levelplot(raster[[j]][[l]],  # [[j]][[l]]
                     at=seq(-100,100),
                     col.regions = rev(terrain.colors(10000)),
                     margin=FALSE)
},j)
    })
  }
  

plot_predictions_gridded <- function(predictions){
  lapply(1:4, function(j){
    lapply(1:4, function(l,j){
      
      predictions[[j]][[l]] %>% 
         filter(pred_bar < 200 & pred_bar > -200) %>% 
        ggplot() +
        geom_tile(aes(x=lon, y=lat, fill = pred_bar)) +
        scale_fill_gradientn(colours = rev(terrain.colors(10000))) +
        theme_bw() +
        ylab("Longitude") +
        xlab("Latitude") +
        labs(fill = "Pooled fit (%)")
      
    },j)
  })
}

save_plots_predictions <- function(plots, path){
  lapply(1:4, function(j){
    lapply(1:4, function(l,j){
      
      # save plots as png
      #png(paste(path,j,l, ".png", sep="_"))
      
      ggsave(filename=paste(path,j,l,".png",sep="_"),
            plot=plots[[j]][[l]],
            width=7, height=6)
          
      
          #dev.off()
      #paste(path,j,l, ".png", sep="_")
      
    },j)
  })
}



# aggregate to country


crop_production_raster_agg <- function(crop_production_rasters){
  lapply(1:4, function(i){
  
  r <- raster::raster(xmn=-180, xmx=180, ymn=-90, ymx=90, crs=4326, res=0.5)
  res.factor <- raster::res(r)/raster::res(crop_production_rasters[[1]]) # 0.5 0.5 / 0.083 0.083 
  raster::aggregate(crop_production_rasters[[i]], fact = res.factor, fun = sum)
  })
}

aggregate_predictions_to_country <- function(predictions){
  lapply(1:4, function(j){ # crop
  lapply(1:4, function(l,j){ # time period
    
    prediction <- exactextractr::exact_extract(predictions[[j]][[l]], 
                                               worldmap_clean, 
                                               'weighted_mean', 
                                               weights = crop_production_raster_agg[[j]]) 
    
    data.frame(ADMIN=worldmap_clean@data$ADMIN,
               ISO_A2=worldmap_clean@data$ISO_A2,
               prediction=prediction)
    
  }, j) 
}) 
}

