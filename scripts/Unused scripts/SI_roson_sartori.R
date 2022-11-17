# Roson and Sartori - rasterise -------------------------------------------

roson_sartori <- "C:/Users/chris/OneDrive - The University of Melbourne/Damages/Roson and Sartori ag damages.xlsx" 

roson_sartori_crops <- c("Maize", "Wheat", "Rice")

roson_sartori_sheets <- lapply(roson_sartori_crops, function(x) {read_excel(path = roson_sartori, sheet = x)})

roson_sartori_df <- rbindlist(roson_sartori_sheets, idcol = 'Crop') %>% 
  mutate(Crop = replace(Crop, Crop == 1, "Maize"),
         Crop = replace(Crop, Crop == 2, "Wheat"),
         Crop = replace(Crop, Crop == 3, "Rice")) %>% 
  rename(degree_1 = 4,
         degree_2 = 5,
         degree_3 = 6,
         degree_4 = 7,
         degree_5 = 8)

# GTAP country code seems to corresponds with rworldmap ISO A3 labels

# use rnaturalearth -----------------------------------------------

          
          world <- rnaturalearth::ne_countries(returnclass='sf') %>% 
            mutate(country=as.numeric(as.factor(admin)))
          
          world_rs <- world %>% left_join(roson_sartori_df, by = c("iso_a3" = "Code"))
          
          world_crops_rs <- lapply(roson_sartori_crops, function(x) world_rs %>% filter(Crop == x))
          
          r_object <- raster(res = c(0.5, 0.5), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
          
          # degrees <- c("+1?C","+2?C","+3?C","+4?C","+5?C")
          degrees <- c("degree_1", "degree_2", "degree_3", "degree_4", "degree_5")
          
          rasterise_rs_crops <- function(i){
            lapply(degrees, function(j) {rasterize(world_crops_rs[[i]], r_object, field = j)})}
          
          rasterised_rs_crops <- lapply(1:3, rasterise_rs_crops)
          
          stacked_rs_crops <- lapply(1:3, function(x) stack(rasterised_rs_crops[[x]]))
          
          # maize
          rasterVis::levelplot(stacked_rs_crops[[1]], names.attr = c("+1?C","+2?C","+3?C","+4?C","+5?C"))
          # wheat
          rasterVis::levelplot(stacked_rs_crops[[2]], names.attr = c("+1?C","+2?C","+3?C","+4?C","+5?C"))
          # rice
          rasterVis::levelplot(stacked_rs_crops[[3]], names.attr = c("+1?C","+2?C","+3?C","+4?C","+5?C"))
          
      
         
          