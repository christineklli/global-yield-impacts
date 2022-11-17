
# A note about scale / units ----------------------------------------------

# In the CGIAR database, yield changes are expressed in percentage changes x 100
# E.g. a 17% decline is entered as '17', not '0.17'
# Therefore predictions are also generated in the same unit
# GAM model estimates scale invariant?
# Converting to absolute and then back to percentage changes therefore does not require multiplying by 100
# E.g. y_1/y_0 - 1

# READ IN POOLED PREDICTIONS DATA----------------------------------------------

crops <- c("Maize", "Rice", "Soybean", "Wheat")


pooled_predictions <- lapply(1:4, function(i){
  
  readr::read_csv(here("processed", paste0("pooled_predictions_re_", crops[[i]], ".csv")),
                  col_types = readr::cols(.default = "c"))})


pooled_predictions <- lapply(1:4, function(i){
  
  pooled_predictions[[i]][,c(1:18)] <- lapply(pooled_predictions[[i]][,c(1:18)], as.numeric)
  
  pooled_predictions[[i]]
  
})


# Convert predicted yield changes to absolute production change --------------------------------------


pooled_predictions_production <- lapply(1:4, function(i){
  
  production <- lapply(pooled_predictions[[i]], "*", crop_production_gaez_agg_dt[[i]]$production) # assumes aggregated cell numbers same
  
  as.data.table(production)
  
})


country_predictions_production <- lapply(1:4, function(i) {
  
  pooled_predictions_production[[i]] %>% 
    cbind(world_d) %>% 
    as.data.frame() %>% 
    group_by(country, name) %>% # only 191 countries
    summarise(across(1:18, ~sum(.x,na.rm=TRUE))) # sum grid cell predictions for country predicted absolute yield change
  
}) 


country_baseline_list_prod <- lapply(1:4, function(i) {
  
  crop_production_gaez_agg_dt[[i]] %>% 
    left_join(world_d, by="cell") %>% 
    as.data.frame() %>% 
    group_by(country, name) %>% # only 191 countries
    summarise(total_production = sum(production, na.rm=TRUE)) # sum grid cell predictions for country predicted absolute yield change
  
}) 
# join baseline and predictions by country

country_production_predictions <- lapply(1:4, function(i){
  
  country_predictions_production[[i]] %>% 
    left_join(country_baseline_list_3[[i]], by = c("name"))
  
}) # this gives absolute production changes (in t)

country_production_predictions[[1]] %>% dplyr::select(country.x, name, total_production) # this gives maize baseline yield

# for every crop dataset, calculate as percentage of Y

country_predicted_production_pct <- lapply(1:4, function(i) {
  
  production_pct <- lapply(country_production_predictions[[i]][3:20], function(x) 
  {x/country_production_predictions[[i]]$total_production} # remember x is delta, not y_1
  )
  
  production_pct %>% as.data.table() %>% 
    cbind(country = country_production_predictions[[i]]$country.x,
          name = country_production_predictions[[i]]$name) %>% 
    relocate(country, name)
  
})


lapply(1:4, function(i) {
  
  country_predicted_production_pct[[i]] %>% 
    readr::write_csv(here("processed", paste0("country_predicted_production_pct_", crops[[i]], ".csv")))
  
})

conv_production_predictions <- lapply(1:4, function(i) {
  
  country_pred <- world %>% 
    left_join(country_predicted_production_pct[[i]], by = "country")
  
  mean_y_0 <- rasterize(country_pred, r, field = "mean_y_0")
  mean_y_1 <- rasterize(country_pred, r, field = "mean_y_1")
  mean_y_2 <- rasterize(country_pred, r, field = "mean_y_2")
  mean_y_3 <- rasterize(country_pred, r, field = "mean_y_3")
  mean_y_4 <- rasterize(country_pred, r, field = "mean_y_4")
  mean_y_5 <- rasterize(country_pred, r, field = "mean_y_5")
  
  predicted_yield_country <- stack(mean_y_0,
                                   mean_y_1,
                                   mean_y_2,
                                   mean_y_3,
                                   mean_y_4,
                                   mean_y_5)
  
  rasterVis::levelplot(predicted_yield_country, 
                       col.regions = rev(terrain.colors(10000)),
                       #  at = seq(-100,100),
                       names.attr = c("0 degrees warming",
                                      "1 degree warming",
                                      "2 degrees warming",
                                      "3 degrees warming",
                                      "4 degrees warming",
                                      "5 degrees warming"))
  
})



plot_conv_production_predictions <- function(i){
  
  mypath <- file.path(here("results", "figures"),
                      paste("country_abs_predicted_production", crops[[i]], ".png", sep = "_"))
  
  png(file = mypath)
  
  plot(conv_production_predictions[[i]])
  
  dev.off()
  
}

conv_production_predictions_plots <- lapply(1:4, plot_conv_production_predictions)

# Convert predicted yield changes to absolute yield change ----------------------


# start with pooled_predictions, which are now saved as including country RE


# Convert gridded predictions to absolute yield changes before production-weighted averaging to country level

# do this by multiplying pooled_predictions by crop_production_gaez_agg_dt, vector by vector

pooled_predictions_yield <- lapply(1:4, function(i){
  
  yield <- lapply(pooled_predictions[[i]], "*", crop_yield_gaez_agg_dt[[i]]$yield) # assumes aggregated cell numbers same
  
  as.data.table(yield)
  
})

# check
pooled_predictions_yield[[1]] %>% as.data.frame() %>% filter(!is.na(mean_y_0) & mean_y_0 != 0) %>% as.data.table()

# sum columns for country level absolute yield change

# this needs to be done by country

country_predictions_yield <- lapply(1:4, function(i) {
  
  pooled_predictions_yield[[i]] %>% 
    cbind(world_d) %>% 
    as.data.frame() %>% 
    group_by(country, name) %>% # only 191 countries
    summarise(across(1:18, ~mean(.x,na.rm=TRUE))) # sum grid cell predictions for country predicted absolute yield change
  
  }) 

# convert back to percentage, take baseline country total yield

#country_baseline_yield <- exactextractr::exact_extract(crop_production_gaez_raster, worldmap_clean, 'sum')

#country_baseline_yield <- country_baseline_yield %>% 
#  cbind(Name = worldmap_clean$NAME,
#  ISO_A2 = worldmap_clean$ISO_A2)


#country_baseline_list <- list(
#  data.frame(name = country_baseline_yield$Name, Y = country_baseline_yield$sum.GAEZAct2015_Production_Maize_Total),
#  data.frame(name = country_baseline_yield$Name, Y = country_baseline_yield$sum.GAEZAct2015_Production_Rice_Total),
#  data.frame(name = country_baseline_yield$Name, Y = country_baseline_yield$sum.GAEZAct2015_Production_Soybean_Total),
#  data.frame(name = country_baseline_yield$Name, Y = country_baseline_yield$sum.GAEZAct2015_Production_Wheat_Total))

# join baseline and predictions by country

country_baseline_predictions <- lapply(1:4, function(i){
  
  country_predictions_yield[[i]] %>% 
  left_join(country_baseline_list_2[[i]], by = c("name"))
  
}) # this gives absolute yield changes (in t/ha)

country_baseline_predictions[[1]] %>% dplyr::select(country.x, name, mean_yield) # this gives baseline yield

# for every crop dataset, calculate as percentage of Y

country_predicted_yield_pct <- lapply(1:4, function(i) {
  
  yield_pct <- lapply(country_baseline_predictions[[i]][3:20], function(x) 
    {x/country_baseline_predictions[[i]]$mean_yield}
    )
  
  yield_pct %>% as.data.table() %>% 
    cbind(country = country_baseline_predictions[[i]]$country.x,
                      name = country_baseline_predictions[[i]]$name) %>% 
    relocate(country, name)
  
})

# the issue is also the ~ 60 country NAs - sum(is.na(country_predicted_yield_pct[[1]]$mean_y_0))
# this is on top of the ~ 50 missing small island countries lost from lowres 
# the missing 60 are most important and largely due to NA, either bc of baseline yield? or predictions?

lapply(1:4, function(i) {
  
  country_predicted_yield_pct[[i]] %>% 
  readr::write_csv(here("processed", paste0("country_predicted_yield_pct_re_", crops[[i]], ".csv")))
  
})

# these are different, but fairly similar to the production percentages

# plot these

# first rasterise

# world left_join country_predicted_yield_pct by country

abs_converted_predictions <- lapply(1:4, function(i) {
  
  country_pred <- world %>% 
  left_join(country_predicted_yield_pct[[i]], by = "country")
  
  mean_y_0 <- rasterize(country_pred, r, field = "mean_y_0")
  mean_y_1 <- rasterize(country_pred, r, field = "mean_y_1")
  mean_y_2 <- rasterize(country_pred, r, field = "mean_y_2")
  mean_y_3 <- rasterize(country_pred, r, field = "mean_y_3")
  mean_y_4 <- rasterize(country_pred, r, field = "mean_y_4")
  mean_y_5 <- rasterize(country_pred, r, field = "mean_y_5")

  predicted_yield_country <- stack(mean_y_0,
                                   mean_y_1,
                                   mean_y_2,
                                   mean_y_3,
                                   mean_y_4,
                                   mean_y_5)
  
  rasterVis::levelplot(predicted_yield_country, 
                       col.regions = rev(terrain.colors(10000)),
                       #  at = seq(-100,100),
                       names.attr = c("0 degrees warming",
                                      "1 degree warming",
                                      "2 degrees warming",
                                      "3 degrees warming",
                                      "4 degrees warming",
                                      "5 degrees warming"))
  
})



lapply(1:4, function(i){
  
  mypath <- file.path(here("results", "figures"),
                      paste("country_abs_predicted_yield_re", crops[[i]], ".png", sep = "_"))
  
  png(file = mypath)
  
  plot(abs_converted_predictions[[i]])
  
  dev.off()
  
})


# there are far fewer countries represented - first of all, limited by the 191? and NAs?
# results are different from non-converted country predictions, even though the ranges do not change.
# missing countries perhaps due to missing current baseline yield information at the gridded level? 
# this means absolute yield cannot be calculated - but doesn't explain australian soybean missing?
# look into this more

country_predicted_yield_pct[[3]] %>% as.data.frame() %>% filter(name=="Australia")

country_baseline_list_2[[3]] %>% as.data.frame() %>% filter(name=="Australia") # total production = 0???

plot(crop_production_gaez_raster)

# this is confirmed in the GAEZ data and Grogan paper. Go back to Monfreda to check. 


crop_production_files <- c("maize_Production.tif",
                           "rice_Production.tif",
                           "soybean_Production.tif",
                           "wheat_Production.tif")

#crop_country_lists <- data.frame(matrix(nrow=243))

crop_production_files2 <- here("data", "Monfreda data", crop_production_files)


crop_production_raster <- lapply(crop_production_files2, raster)

crop_production_raster <- stack(crop_production_raster)

plot(crop_production_raster) # there is at least one pixel in Australia for soybean

crop_production_csv <- readr::read_csv(here("processed", "crop_production_list.csv"))

crop_production_csv # there is non-zero soybean production in Australia

# look into the paper describing the dataset 

# report plots in draft

# there does appear to be more variation in the yield predictions compared to the production predictions

# we can also plot the absolute yield changes