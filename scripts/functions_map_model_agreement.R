
calc_model_agreement <- function(predictions, threshold, ncores){
  
  future::plan(future::multisession, workers = ncores)
  
  mean <- predictions %>% 
    group_by(lon, lat, crop_pooled, time_period) %>% 
    summarise(mean_pred_bar = mean(pred_bar)) # model mean
  
  df <-  predictions %>%
    dplyr::select(!c("model_spec","v_w","v_b","v_p","se_p","lwr_p","upr_p")) %>% 
    left_join(mean, by=c("lon","lat","crop_pooled","time_period"))
  
  tbl <- df %>% 
    group_by(lon,lat,crop_pooled,time_period) %>% 
    # if agree with sign then mutate variable = 20
    mutate(sign=ifelse(
      (mean_pred_bar <0 & pred_bar <0) | (mean_pred_bar > 0 & pred_bar > 0), 20, 0)) %>% 
    summarise(model_agreement = sum(sign)) %>% 
    mutate(agreement = ifelse(model_agreement >= threshold, 1, 0)) # can inspect after this 
  
  # split df into nested list by crop
  
  crop_tbl <- split(tbl, tbl$crop_pooled)
  
  crop_time_period_tbl <- lapply(1:4, function(crop){
    split(crop_tbl[[crop]], 
          crop_tbl[[crop]]$time_period
    )      })
  
  future.apply::future_lapply(1:4, function(crop){
    lapply(1:4, function(time){
      points <- crop_time_period_tbl[[crop]][[time]] %>% 
        filter(agreement==1) # keep only pixels with agreement
      
      st_as_sf(points, coords=c("lon","lat"), 
               crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    })
  })
  
}


# pool level 3 predictions across model and rasterise


rasterise_pooled_model_predictions <- function(predictions, time_periods, ncores){ 
  
  tbl <- predictions %>% 
    group_by(lon, lat, crop_pooled, time_period) %>% 
    summarise(mean_pred_bar = mean(pred_bar))
  
  # split into nested form
  
  crop_tbl <- split(tbl, tbl$crop_pooled)
  
  
  future::plan(future::multisession, workers = ncores)
  
  future.apply::future_lapply(1:4, function(crop){
    
    x <- crop_tbl[[crop]] # two levels only
    
    x_list <- lapply(1:4, function(time){ # for each time period
    
    r <- x[x$time_period==time_periods[[time]], c("lon", "lat", "mean_pred_bar")] 
    r <- raster::rasterFromXYZ(r)
    crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
    names(r) <- time_periods[[time]]
    r
  })
  
  raster::stack(x_list) # stack time period raster layers for each crop
    
  })
  
  
  
}





plot_model_agreement <- function(predictions, model_agreement_sf, time_periods, 
                                 crops, path, returnStack, World, ncores){
  
  future::plan(future::multisession, workers = ncores)
  
  future.apply::future_lapply(1:4, function(crop){
    
    columns <- c("X2021.2040", "X2041.2060", "X2061.2080", "X2081.2100")
    
    plot_list <- lapply(1:4, function(time){ # time period recursively
      tmap::tm_shape(predictions[[crop]][[time]]) + 
        tmap::tm_raster(columns[[time]], title="Pooled fit (%)",
                        style = "cont",
                        breaks=seq(-100,100,10), 
                        # ^ this is chosen manually to disregard skewing/scaling effects of Siwa desert outliers
                        # remove range restriction when we remove outliers in the prediction data
                        palette = rev(terrain.colors(100))) +
        tmap::tm_shape(model_agreement_sf[[crop]][[time]]) +
        tmap::tm_dots(size=0.00000001,shape=1, alpha=0.05) + # open circle shape
        tmap::tm_shape(World) +
        tmap::tm_borders("grey", lwd =1) + 
        tmap::tm_layout(panel.labels = c(time_periods[[time]])) 
      
      
    })
    
    outfile <- sprintf(path, 
                       crops[[crop]])
    
    plot <- tmap::tmap_arrange(plot_list[[1]],
                               plot_list[[2]],
                               plot_list[[3]],
                               plot_list[[4]],
                               nrow=2, ncol=2)
    
    tmap::tmap_save(plot, filename=outfile)
    
    # show predictions again
    if(isTRUE(returnStack)) {
      plot
    } else {
      outfile
    }
    
  })
} 