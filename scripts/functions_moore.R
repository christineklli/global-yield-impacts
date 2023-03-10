
plot_moore_gridded <- function(data, World, path, crops){
  
  lapply(1:4, function(crop){
    
    tmap::tmap_mode("plot")
    
    plot <- tmap::tm_shape(data[[crop]]) +
      tmap::tm_raster(title = "Yield change (%)",
                      style = "cont",
                      breaks = seq(-100,100,10),
                      palette = rev(terrain.colors(100))) +
      tmap::tm_shape(World) +
      tmap::tm_borders("grey", lwd = 1) +
      tmap::tm_layout(panel.labels = c("1 degree", "2 degrees", "3 degrees"))
    
    outfile <- sprintf(path,
                       crops[[crop]])
    
    tmap::tmap_save(plot, filename = outfile, height = 4, width = 10, asp = 0)
  })
}