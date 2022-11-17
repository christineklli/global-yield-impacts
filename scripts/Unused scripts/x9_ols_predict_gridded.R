# Global prediction -------------------------------------------------------
# global prediction with stratified bootstrapping -------------------------------------------

# need to make 750 bootstrapped gridded predictions

bs_cluster <- bootstraps(AGIMPACTS_TRUNCATED_DATA, times = 1500, strata = Reference) 

# look at samples to check that every study is represented, but different point estimates from each study

# unique(as_tibble(bs_cluster$splits[[1]])[11]) %>% print(n=Inf) # cycle through [[1:750]] to confirm; [[11]] is the Ref column

bootstrap_gridded_predict <- function(i, PREDICTION_DATA, CROP_PRODUCTION){
  
  split_tibble <- as_tibble(bs_cluster$splits[[i]])
  
  damages <- lm(Yield.Change ~ 0 + Temp.Change:crop_pooled + I(Temp.Change^2):crop_pooled +
                  Temp.Change:crop_pooled:Baseline_tmp_weighted + 
                  I(Temp.Change^2):crop_pooled:Baseline_tmp_weighted +
                  # crop_pooled:Baseline_tmp_weighted + ### NOTE THIS CHANGE ###
                  f_CO2:C3 + f_CO2:C4  + 
                  Precipitation.change +
                  Temp.Change:adapt_dummy +
                  adapt_dummy,
                data = split_tibble)
  
  adj <- coef(damages)[3]-coef(damages)[2] # take the bootstrapped adjustment 
  
  # make bootstrapped gridded predictions
  predicted_model_1 <- predict(damages, PREDICTION_DATA[[1]])
  predicted_model_1_adj <- predicted_model_1 - adj
  # set grid cells with yield loss > 99% to 99%
  predicted_model_1_cap <- predicted_model_1_adj
  predicted_model_1_cap[predicted_model_1_cap < -99] <- -99
  #predicted_model_1_cap[predicted_model_1_cap > 300] <- NA
  
  predicted_model_2 <- predict(damages, PREDICTION_DATA[[2]])
  predicted_model_2_adj <- predicted_model_2 - adj
  # set grid cells with yield loss > 99% to 99%
  predicted_model_2_cap <- predicted_model_2_adj
  predicted_model_2_cap[predicted_model_2_cap < -99] <- -99
  #predicted_model_2_cap[predicted_model_2_cap > 300] <- NA
  
  predicted_model_3 <- predict(damages, PREDICTION_DATA[[3]])
  predicted_model_3_adj <- predicted_model_3 - adj
  # set grid cells with yield loss > 99% to 99%
  predicted_model_3_cap <- predicted_model_3_adj
  predicted_model_3_cap[predicted_model_3_cap < -99] <- -99
  #predicted_model_3_cap[predicted_model_3_cap > 300] <- NA
  
  raster_1dc <- raster(ncol = 720, nrow = 360) 
  ncell(raster_1dc)
  values(raster_1dc) <- predicted_model_1_cap
  
  raster_2dc <- raster(ncol = 720, nrow = 360) 
  ncell(raster_2dc)
  values(raster_2dc) <- predicted_model_2_cap
  
  raster_3dc <- raster(ncol = 720, nrow = 360) 
  ncell(raster_3dc)
  values(raster_3dc) <- predicted_model_3_cap
  
  raster_alldc_stack <- stack(raster_1dc, raster_2dc, raster_3dc)
  
  spatraster_alldc_stack <- as(raster_alldc_stack, "SpatRaster")
  
  spatraster_weights <- as(CROP_PRODUCTION, "SpatRaster")
  
  terra::global(spatraster_alldc_stack, 'mean', weights = spatraster_weights, na.rm = TRUE)
  
}

# do for maize first, then run apply function on three other crops

maize_CI_predictions <- lapply(1:750, 
                               bootstrap_gridded_predict, 
                               PREDICTION_DATA = maize_prediction_data,
                               CROP_PRODUCTION = maize_production)

# make into df
maize_CI_predictions_df <- do.call("cbind", maize_CI_predictions)

maize_global_prediction_CI <- t(maize_CI_predictions_df) %>% 
  as.data.frame() %>% 
  summarise(degree_1_025 = quantile(layer.1, 0.025),
            degree_1_975 = quantile(layer.1, 0.975),
            degree_2_025 = quantile(layer.2, 0.025),
            degree_2_975 = quantile(layer.2, 0.975),
            degree_3_025 = quantile(layer.3, 0.025),
            degree_3_975 = quantile(layer.3, 0.975)) 

# wrap into function for other three crops
global_prediction_CI <- function(crop_prediction_data, crop_production){
  
  crop_CI_predictions <- lapply(1:750, 
                                bootstrap_gridded_predict, 
                                PREDICTION_DATA = crop_prediction_data,
                                CROP_PRODUCTION = crop_production)
  
  # make into df
  crop_CI_predictions_df <- do.call("cbind", crop_CI_predictions)
  
  t(crop_CI_predictions_df) %>% 
    as.data.frame() %>% 
    summarise(degree_1_025 = quantile(layer.1, 0.025),
              degree_1_975 = quantile(layer.1, 0.975),
              degree_2_025 = quantile(layer.2, 0.025),
              degree_2_975 = quantile(layer.2, 0.975),
              degree_3_025 = quantile(layer.3, 0.025),
              degree_3_975 = quantile(layer.3, 0.975)) 
  
}

# apply function to other three crops
rice_global_prediction_CI <- global_prediction_CI(crop_prediction_data = rice_prediction_data,
                                                  crop_production = rice_production)

soybean_global_prediction_CI <- global_prediction_CI(crop_prediction_data = soybean_prediction_data,
                                                     crop_production = soybean_production)

wheat_global_prediction_CI <- global_prediction_CI(crop_prediction_data = wheat_prediction_data,
                                                   crop_production = wheat_production)

global_prediction_CI <- rbind(maize_global_prediction_CI,
                              rice_global_prediction_CI,
                              soybean_global_prediction_CI,
                              wheat_global_prediction_CI)

global_prediction_CI <- cbind(global_prediction_CI, 
                              crop = c("Maize", "Rice", "Soybean", "Wheat"))

# pivot long so that can cbind with global_averages_df_long
global_avg_predictions <- global_prediction_CI %>% 
  pivot_longer(cols = starts_with("degree"), names_to = "CI",
               # names_prefix = "degree_1_",
               values_to = "value") %>% 
  mutate(CI_pct = case_when(endsWith(CI, "025") ~ "025",
                            endsWith(CI, "975") ~ "975"),
         variable = case_when(startsWith(CI, "degree_1") ~ "1 degree",
                              startsWith(CI, "degree_2") ~ "2 degrees",
                              startsWith(CI, "degree_3") ~ "3 degrees")) %>% 
  dplyr::select(!CI) %>% 
  full_join(global_averages_df_long) %>% 
  mutate(CI_pct = replace(CI_pct, is.na(CI_pct), "central")) %>% 
  print(n=Inf)


# re-run global average predictions, add CI, plot on same bar chart
ggplot(global_avg_predictions, aes(group = CI_pct, x = variable, y = value)) +
  geom_bar(position = "dodge", stat = "identity", fill = "goldenrod1") +
  facet_wrap(~crop) + theme_bw() +
  labs(x = "Warming (degrees C)",
       y = "Yield Change (%)")


# original 
ggplot(global_averages_df_long, aes(x = variable, y = value)) +
  geom_bar(stat = "identity", fill = "goldenrod1") +
  facet_wrap(~crop) + theme_bw() +
  labs(x = "Warming (degrees C)",
       y = "Yield Change (%)")

# with error bars
ggplot() + 
  geom_bar(data = global_averages_df_long, aes(x = variable, y = value), stat = "identity", fill = "goldenrod1") +
  geom_errorbar(data = global_avg_predictions[global_avg_predictions$CI_pct %in% c("025","975"),], 
                aes(x = variable, ymin = value, ymax = value), 
                width = 0.4) + # need to fix this
  facet_wrap(~crop) + theme_bw() +
  labs(x = "Warming (degrees C)",
       y = "Yield Change (%)")



# global prediction with block/cluster bootstrapping -----------------------------------

# note inconsistency here with naming convention; where cluster really is 'cluster' not stratification

# need to make 750 bootstrapped gridded predictions


bootstrap_gridded_predict_cluster <- function(i, PREDICTION_DATA, CROP_PRODUCTION){
  
  split_tibble <- as_tibble(bs$splits[[i]]) %>% unnest(cols = c(data))  
  
  damages <- lm(Yield.Change ~ 0 + Temp.Change:crop_pooled + I(Temp.Change^2):crop_pooled +
                  Temp.Change:crop_pooled:Baseline_tmp_weighted + 
                  I(Temp.Change^2):crop_pooled:Baseline_tmp_weighted +
                  # crop_pooled:Baseline_tmp_weighted + ### NOTE THIS CHANGE ###
                  f_CO2:C3 + f_CO2:C4  + 
                  Precipitation.change +
                  Temp.Change:adapt_dummy +
                  adapt_dummy,
                data = split_tibble)
  
  adj <- coef(damages)[3]-coef(damages)[2] # take the bootstrapped adjustment 
  
  # make bootstrapped gridded predictions
  predicted_model_1 <- predict(damages, PREDICTION_DATA[[1]])
  predicted_model_1_adj <- predicted_model_1 - adj
  # set grid cells with yield loss > 99% to 99%
  predicted_model_1_cap <- predicted_model_1_adj
  predicted_model_1_cap[predicted_model_1_cap < -99] <- -99
  #predicted_model_1_cap[predicted_model_1_cap > 300] <- NA
  
  predicted_model_2 <- predict(damages, PREDICTION_DATA[[2]])
  predicted_model_2_adj <- predicted_model_2 - adj
  # set grid cells with yield loss > 99% to 99%
  predicted_model_2_cap <- predicted_model_2_adj
  predicted_model_2_cap[predicted_model_2_cap < -99] <- -99
  #predicted_model_2_cap[predicted_model_2_cap > 300] <- NA
  
  predicted_model_3 <- predict(damages, PREDICTION_DATA[[3]])
  predicted_model_3_adj <- predicted_model_3 - adj
  # set grid cells with yield loss > 99% to 99%
  predicted_model_3_cap <- predicted_model_3_adj
  predicted_model_3_cap[predicted_model_3_cap < -99] <- -99
  #predicted_model_3_cap[predicted_model_3_cap > 300] <- NA
  
  raster_1dc <- raster(ncol = 720, nrow = 360) 
  ncell(raster_1dc)
  values(raster_1dc) <- predicted_model_1_cap
  
  raster_2dc <- raster(ncol = 720, nrow = 360) 
  ncell(raster_2dc)
  values(raster_2dc) <- predicted_model_2_cap
  
  raster_3dc <- raster(ncol = 720, nrow = 360) 
  ncell(raster_3dc)
  values(raster_3dc) <- predicted_model_3_cap
  
  raster_alldc_stack <- stack(raster_1dc, raster_2dc, raster_3dc)
  
  spatraster_alldc_stack <- as(raster_alldc_stack, "SpatRaster")
  
  spatraster_weights <- as(CROP_PRODUCTION, "SpatRaster")
  
  terra::global(spatraster_alldc_stack, 'mean', weights = spatraster_weights, na.rm = TRUE)
  
}


# wrap into function for other three crops
global_prediction_CI_cluster <- function(crop_prediction_data, crop_production){
  
  crop_CI_predictions <- lapply(1:1500,
                                bootstrap_gridded_predict_cluster, 
                                PREDICTION_DATA = crop_prediction_data,
                                CROP_PRODUCTION = crop_production)
  
  # make into df
  crop_CI_predictions_df <- do.call("cbind", crop_CI_predictions)
  
  t(crop_CI_predictions_df) %>% 
    as.data.frame() %>% 
    summarise(degree_1_025 = quantile(layer.1, 0.025),
              degree_1_975 = quantile(layer.1, 0.975),
              degree_1_500 = quantile(layer.1, 0.500), # new
              degree_2_025 = quantile(layer.2, 0.025),
              degree_2_500 = quantile(layer.2, 0.500), # new
              degree_2_975 = quantile(layer.2, 0.975),
              degree_3_025 = quantile(layer.3, 0.025),
              degree_3_500 = quantile(layer.3, 0.500),# new 
              degree_3_975 = quantile(layer.3, 0.975)) 
  
}

# apply function to other three crops
# to replace * with _bootstrap, but appear to require allocating vector of 43.5 Mb - need to sort this out before re-running
maize_global_prediction_CI_cluster <- global_prediction_CI_cluster(crop_prediction_data = maize_prediction_data,
                                                                   crop_production = maize_production)

rice_global_prediction_CI_cluster <- global_prediction_CI_cluster(crop_prediction_data = rice_prediction_data,
                                                                  crop_production = rice_production)

soybean_global_prediction_CI_cluster <- global_prediction_CI_cluster(crop_prediction_data = soybean_prediction_data,
                                                                     crop_production = soybean_production)

# soybean is not being computed properly

wheat_global_prediction_CI_cluster <- global_prediction_CI_cluster(crop_prediction_data = wheat_prediction_data,
                                                                   crop_production = wheat_production)


global_prediction_CI_cluster <- rbind(maize_global_prediction_CI_cluster,
                                      rice_global_prediction_CI_cluster,
                                      soybean_global_prediction_CI_cluster,
                                      wheat_global_prediction_CI_cluster)

global_prediction_CI_cluster <- cbind(global_prediction_CI_cluster, 
                                      crop = c("Maize", "Rice", "Soybean", "Wheat"))

# pivot long so that can cbind with global_averages_df_long
global_avg_predictions_cluster <- global_prediction_CI_cluster %>% 
  pivot_longer(cols = starts_with("degree"), names_to = "CI",
               # names_prefix = "degree_1_",
               values_to = "value") %>% 
  mutate(CI_pct = case_when(endsWith(CI, "025") ~ "025",
                            endsWith(CI, "975") ~ "975"),
         variable = case_when(startsWith(CI, "degree_1") ~ "1 degree",
                              startsWith(CI, "degree_2") ~ "2 degrees",
                              startsWith(CI, "degree_3") ~ "3 degrees")) %>% 
  dplyr::select(!CI) %>% 
  full_join(global_averages_df_long) %>% 
  mutate(CI_pct = replace(CI_pct, is.na(CI_pct), "central")) %>% 
  print(n=Inf)


# re-run global average predictions, add CI, plot on same bar chart
ggplot(global_avg_predictions_cluster, aes(group = CI_pct, x = variable, y = value)) +
  geom_bar(position = "dodge", stat = "identity", fill = "goldenrod1") +
  facet_wrap(~crop) + theme_bw() +
  labs(x = "Warming (degrees C)",
       y = "Yield Change (%)")


# original 
ggplot(global_averages_df_long, aes(x = variable, y = value)) +
  geom_bar(stat = "identity", fill = "goldenrod1") +
  facet_wrap(~crop) + theme_bw() +
  labs(x = "Warming (degrees C)",
       y = "Yield Change (%)")

# with error bars
ggplot() + 
  geom_bar(data = global_averages_df_long, aes(x = variable, y = value), stat = "identity", fill = "goldenrod1") +
  geom_errorbar(data = global_avg_predictions_cluster[global_avg_predictions_cluster$CI_pct %in% c("025","975"),], 
                aes(x = variable, ymin = value, ymax = value), 
                width = 0.2) + 
  facet_wrap(~crop) + theme_bw() +
  labs(x = "Warming (degrees C)",
       y = "Yield Change (%)")

# pivot wide data 
global_avg_predictions_cluster_wide <- global_avg_predictions_cluster %>% 
  pivot_wider(names_from = CI_pct, values_from = value)

# replot with error bars
ggplot() + 
  geom_bar(data = global_averages_df_long, aes(x = variable, y = value), stat = "identity", fill = "goldenrod1") +
  geom_errorbar(data = global_avg_predictions_cluster_wide, 
                aes(x = variable, ymin = `025`, ymax = `975`), 
                width = 0.2) + 
  facet_wrap(~crop) + theme_bw() +
  labs(x = "Warming (degrees C)",
       y = "Yield Change (%)")


# REVISE THIS PER JOHN'S SUGGESTIONS
bootstrap_predict_cluster_gridded <- function(i, PREDICTION_DATA, CROP_PRODUCTION){
  
  split_tibble <- as_tibble(bs$splits[[i]]) %>% unnest(cols = c(data))  
  
  damages <- lm(Yield.Change ~ 0 + Temp.Change:crop_pooled + I(Temp.Change^2):crop_pooled +
                  Temp.Change:crop_pooled:Baseline_tmp_weighted + 
                  I(Temp.Change^2):crop_pooled:Baseline_tmp_weighted +
                  # crop_pooled:Baseline_tmp_weighted + ### NOTE THIS CHANGE ###
                  f_CO2:C3 + f_CO2:C4  + 
                  Precipitation.change +
                  Temp.Change:adapt_dummy +
                  adapt_dummy,
                data = split_tibble)
  
  adj <- coef(damages)[3]-coef(damages)[2] # take the bootstrapped adjustment 
  
  # make bootstrapped gridded predictions
  predicted_model_1 <- predict(damages, PREDICTION_DATA[[1]])
  predicted_model_1_adj <- predicted_model_1 - adj
  # set grid cells with yield loss > 99% to 99%
  predicted_model_1_cap <- predicted_model_1_adj
  predicted_model_1_cap[predicted_model_1_cap < -99] <- -99
  #predicted_model_1_cap[predicted_model_1_cap > 300] <- NA
  
  predicted_model_2 <- predict(damages, PREDICTION_DATA[[2]])
  predicted_model_2_adj <- predicted_model_2 - adj
  # set grid cells with yield loss > 99% to 99%
  predicted_model_2_cap <- predicted_model_2_adj
  predicted_model_2_cap[predicted_model_2_cap < -99] <- -99
  #predicted_model_2_cap[predicted_model_2_cap > 300] <- NA
  
  predicted_model_3 <- predict(damages, PREDICTION_DATA[[3]])
  predicted_model_3_adj <- predicted_model_3 - adj
  # set grid cells with yield loss > 99% to 99%
  predicted_model_3_cap <- predicted_model_3_adj
  predicted_model_3_cap[predicted_model_3_cap < -99] <- -99
  #predicted_model_3_cap[predicted_model_3_cap > 300] <- NA
  
  predicted_model_4 <- predict(damages, PREDICTION_DATA[[4]])
  predicted_model_4_adj <- predicted_model_4 - adj
  # set grid cells with yield loss > 99% to 99%
  predicted_model_4_cap <- predicted_model_4_adj
  predicted_model_4_cap[predicted_model_4_cap < -99] <- -99
  #predicted_model_4_cap[predicted_model_4_cap > 300] <- NA
  
  df <- data.frame("1 degree" = predicted_model_1_cap, 
                   "2 degrees" = predicted_model_2_cap, 
                   "3 degrees" = predicted_model_3_cap, 
                   "4 degrees" = predicted_model_4_cap) %>% 
    mutate(ncell = c(1:259200))
  
  df
  
}


gridded_prediction_CI_cluster <- function(crop_prediction_data, crop_production){
  
  crop_CI_predictions <- lapply(1:1, # will not have the memory to do this 750 times
                                bootstrap_predict_cluster_gridded, # apply function above
                                PREDICTION_DATA = crop_prediction_data,
                                CROP_PRODUCTION = crop_production)
  
  crop_CI_predictions_df <- do.call("rbind", crop_CI_predictions)
  
  crop_CI_predictions_df %>% 
    as.data.frame() %>% 
    group_by(ncell) %>% 
    summarise("1 degrees median" = quantile(X1.degree, 0.500, na.rm = TRUE),
              "2 degrees median" = quantile(X2.degrees, 0.500, na.rm = TRUE),
              "3 degrees median" = quantile(X3.degrees, 0.500, na.rm = TRUE),
              "4 degrees median" = quantile(X4.degrees, 0.500, na.rm = TRUE)) 
  
}

maize_prediction_CI_cluster_gridded_1995_2020 <- gridded_prediction_CI_cluster(crop_prediction_data = maize_prediction_2015_2020_data,
                                                                               crop_production = maize_production)

