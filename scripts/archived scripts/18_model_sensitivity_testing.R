
# Running all steps from model fit, to plotting pooled gridded and country predictions--------

# run program based on 4 different model specifications, meta-programming

model_specs <- c(quote(Yield.Change ~ 
                        s(Temp.Change, k = 3) + 
                        Temp.Change:Baseline_tmp_weighted +
                        s(Precipitation.change) +
                        f_CO2:C3 +
                        f_CO2:C4 +
                        adapt_dummy +
                        Temp.Change:adapt_dummy +
                        s(Reference_fact, bs = 're') + 
                        s(Country2_fact, bs = 're')),
                 quote(Yield.Change ~ 
                        s(Temp.Change, k = 3) + 
                        te(Temp.Change,Baseline_tmp_weighted) +
                        s(Precipitation.change) +
                        f_CO2:C3 +
                        f_CO2:C4 +
                        adapt_dummy +
                        Temp.Change:adapt_dummy +
                        s(Reference_fact, bs = 're') + 
                        s(Country2_fact, bs = 're')),
                 quote(Yield.Change ~ 
                        s(Temp.Change, k = 3) + 
                        ti(Temp.Change,Baseline_tmp_weighted) +
                        s(Precipitation.change) +
                        f_CO2:C3 +
                        f_CO2:C4 +
                        adapt_dummy +
                        Temp.Change:adapt_dummy +
                        s(Reference_fact, bs = 're') + 
                        s(Country2_fact, bs = 're')),
                 quote(Yield.Change ~ 
                        s(Temp.Change, k = 3) + 
                        Baseline_tmp_weighted +
                        ti(Temp.Change,Baseline_tmp_weighted) +
                        s(Precipitation.change) +
                        f_CO2:C3 +
                        f_CO2:C4 +
                        adapt_dummy +
                        Temp.Change:adapt_dummy +
                        s(Reference_fact, bs = 're') + 
                        s(Country2_fact, bs = 're')),
                 quote(Yield.Change ~ 
                        s(Temp.Change, k = 3) + 
                        s(Baseline_tmp_weighted) +
                        ti(Temp.Change,Baseline_tmp_weighted) +
                        s(Precipitation.change) +
                        f_CO2:C3 +
                        f_CO2:C4 +
                        adapt_dummy +
                        Temp.Change:adapt_dummy +
                        s(Reference_fact, bs = 're') + 
                        s(Country2_fact, bs = 're')))

# run program

model_country_alt <- lapply(1:4, function(k){
  
  fit_maize_restricted <- function(i, j){
    
    model <-  mgcv::gam(model_specs[[k]], 
                        method = 'REML', 
                        family = 'gaussian',
                        data = meta_m1_data_restricted_crops[[j]][[i]]) 
    
  }
  
  
  fit_maize_restricted <- lapply(1, function(j){lapply(1:5, fit_maize_restricted, j)})
  
  
  fit_rice_restricted <- function(i, j){
    
    
    model <-  mgcv::gam(model_specs[[k]], 
                        method = 'REML', 
                        family = 'gaussian',
                        data = meta_m1_data_restricted_crops[[j]][[i]]) 
    
  }
  
  
  fit_rice_restricted <- lapply(2, function(j){lapply(1:5, fit_rice_restricted, j)})
  
  
  fit_soy_restricted <- function(i, j){
    
    
    model <-  mgcv::gam(model_specs[[k]], 
                        method = 'REML', 
                        family = 'gaussian',
                        data = meta_m1_data_restricted_crops[[j]][[i]]) 
    
  }
  
  
  fit_soy_restricted <- lapply(3, function(j){lapply(1:5, fit_soy_restricted, j)})
  
  
  fit_wheat_restricted <- function(i, j){
    
    
    model <-  mgcv::gam(model_specs[[k]], 
                        method = 'REML', 
                        family = 'gaussian',
                        data = meta_m1_data_restricted_crops[[j]][[i]]) 
    
  }
  
  fit_wheat_restricted <- lapply(4, function(j){lapply(1:5, fit_wheat_restricted, j)})
  
  fit_restricted_list <- list(fit_maize_restricted, 
                                       fit_rice_restricted,
                                       fit_soy_restricted,
                                       fit_wheat_restricted)
  

predict_across_crops_imputations <- function(i, j){
  
  pred_data_0 <- crops_prediction_data[[j]][[1]] %>% 
    mutate(ncell = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted)) 
  
  pred_data_1 <- crops_prediction_data[[j]][[2]] %>% 
    mutate(ncell = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted)) 
  
  pred_data_2 <- crops_prediction_data[[j]][[3]] %>% 
    mutate(ncell = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted)) 
  
  pred_data_3 <- crops_prediction_data[[j]][[4]] %>% 
    mutate(ncell = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted)) 
  
  pred_data_4 <- crops_prediction_data[[j]][[5]] %>% 
    mutate(ncell = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted)) 
  
  pred_data_5 <- crops_prediction_data[[j]][[6]] %>% 
    mutate(ncell = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted)) 
  
  y_0 <- gammit::predict_gamm(fit_restricted_list[[j]][[1]][[i]],
                              pred_data_0, 
                              exclude = c("s(Country2_fact)", "s(Reference_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit) # must have this arg
  
  y_1 <- gammit::predict_gamm(fit_restricted_list[[j]][[1]][[i]],
                              pred_data_1, 
                              exclude = c("s(Country2_fact)", "s(Reference_fact)"), # must exclude
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit)# must have this arg
  
  y_2 <- gammit::predict_gamm(fit_restricted_list[[j]][[1]][[i]],
                              pred_data_2, # 3 REFERS TO 2 DC
                              exclude = c("s(Country2_fact)", "s(Reference_fact)"), # must exclude
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit)# must have this arg
  
  y_3 <- gammit::predict_gamm(fit_restricted_list[[j]][[1]][[i]],
                              pred_data_3, # 4 REFERS TO 3 DC
                              exclude = c("s(Country2_fact)", "s(Reference_fact)"), # must exclude
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit) # must have this arg
  
  y_4 <- gammit::predict_gamm(fit_restricted_list[[j]][[1]][[i]],
                              pred_data_4, # 5 REFERS TO 4 DC
                              exclude = c("s(Country2_fact)", "s(Reference_fact)"), # must exclude
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit)# must have this arg
  
  y_5 <- gammit::predict_gamm(fit_restricted_list[[j]][[1]][[i]],
                              pred_data_5, # 5 REFERS TO 4 DC
                              exclude = c("s(Country2_fact)", "s(Reference_fact)"), # must exclude
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit)# must have this arg
  
  example <- matrix(NA, nrow=720*360) %>% 
    as.data.table() %>% 
    rename(ncell = 1) %>%
    mutate(ncell = row_number()) %>% 
    left_join(y_0, by = "ncell") %>% 
    rename(y_0 = prediction.fit, y_0_se = prediction.se.fit) %>% 
    mutate(ncell = row_number()) %>% 
    left_join(y_1, by = "ncell") %>% 
    rename(y_1 = prediction.fit, y_1_se = prediction.se.fit) %>% 
    mutate(ncell = row_number()) %>% 
    left_join(y_2, by = "ncell") %>% 
    rename(y_2 = prediction.fit, y_2_se = prediction.se.fit) %>% 
    mutate(ncell = row_number()) %>% 
    left_join(y_3, by = "ncell") %>% 
    rename(y_3 = prediction.fit, y_3_se = prediction.se.fit) %>% 
    mutate(ncell = row_number()) %>% 
    left_join(y_4, by = "ncell") %>% 
    rename(y_4 = prediction.fit, y_4_se = prediction.se.fit) %>% 
    mutate(ncell = row_number()) %>% 
    left_join(y_5, by = "ncell") %>% 
    rename(y_5 = prediction.fit, y_5_se = prediction.se.fit) %>% 
    mutate(ncell = row_number()) 
  
}

predictions_across_crops_imputations <- lapply(1:4, function(j){lapply(1:5, predict_across_crops_imputations, j)})


gridded_predictions <-  predictions_across_crops_imputations

rename_prediction_cols <- function(i,j){
  
  colnames(gridded_predictions[[j]][[i]]) <- paste(colnames(gridded_predictions[[j]][[i]]), "m", i, sep = "_")
  gridded_predictions[[j]][[i]]
}

renamed_prediction_cols <- lapply(1:4, function(j){lapply(1:5, rename_prediction_cols, j)})

# merge level two list dataframes for each crop

pool_prediction_cols <- function(i){
  
  cbind(
    renamed_prediction_cols[[i]][[1]],
    renamed_prediction_cols[[i]][[2]],
    renamed_prediction_cols[[i]][[3]],
    renamed_prediction_cols[[i]][[4]],
    renamed_prediction_cols[[i]][[5]])
  
}      

pooled_prediction_cols <- lapply(1:4, pool_prediction_cols)          

average_pooled_predictions <- function(i){
  
  pooled_prediction_cols[[i]] %>% 
    rowwise %>% 
    mutate(mean_y_0 = mean(y_0_m_1, y_0_m_2, y_0_m_3, y_0_m_4, y_0_m_5, na.rm=TRUE),
           mean_y_1 = mean(y_1_m_1, y_1_m_2, y_1_m_3, y_1_m_4, y_1_m_5, na.rm=TRUE),
           mean_y_2 = mean(y_2_m_1, y_2_m_2, y_2_m_3, y_2_m_4, y_2_m_5, na.rm=TRUE),
           mean_y_3 = mean(y_3_m_1, y_3_m_2, y_3_m_3, y_3_m_4, y_3_m_5, na.rm=TRUE),
           mean_y_4 = mean(y_4_m_1, y_4_m_2, y_4_m_3, y_4_m_4, y_4_m_5, na.rm=TRUE),
           mean_y_5 = mean(y_5_m_1, y_5_m_2, y_5_m_3, y_5_m_4, y_5_m_5, na.rm=TRUE),
           V_w_0 = (y_0_se_m_1^2 + y_0_se_m_2^2 + y_0_se_m_3^2 + y_0_se_m_4^2 + y_0_se_m_5^2)/5,
           V_w_1 = (y_1_se_m_1^2 + y_1_se_m_2^2 + y_1_se_m_3^2 + y_1_se_m_4^2 + y_1_se_m_5^2)/5,
           V_w_2 = (y_2_se_m_1^2 + y_2_se_m_2^2 + y_2_se_m_3^2 + y_2_se_m_4^2 + y_2_se_m_5^2)/5,
           V_w_3 = (y_3_se_m_1^2 + y_3_se_m_2^2 + y_3_se_m_3^2 + y_3_se_m_4^2 + y_3_se_m_5^2)/5,
           V_w_4 = (y_4_se_m_1^2 + y_4_se_m_2^2 + y_4_se_m_3^2 + y_4_se_m_4^2 + y_4_se_m_5^2)/5,
           V_w_5 = (y_5_se_m_1^2 + y_5_se_m_2^2 + y_5_se_m_3^2 + y_5_se_m_4^2 + y_5_se_m_5^2)/5,
           V_b_0 = ((y_0_m_1 - mean_y_0)^2 + (y_0_m_2 - mean_y_0)^2 + (y_0_m_3 - mean_y_0)^2 + (y_0_m_4 - mean_y_0)^2 + (y_0_m_5 - mean_y_0)^2)/(5-1),
           V_b_1 = ((y_1_m_1 - mean_y_1)^2 + (y_1_m_2 - mean_y_1)^2 + (y_1_m_3 - mean_y_1)^2 + (y_1_m_4 - mean_y_1)^2 + (y_1_m_5 - mean_y_1)^2)/(5-1),
           V_b_2 = ((y_2_m_1 - mean_y_2)^2 + (y_2_m_2 - mean_y_2)^2 + (y_2_m_3 - mean_y_2)^2 + (y_2_m_4 - mean_y_2)^2 + (y_2_m_5 - mean_y_2)^2)/(5-1),
           V_b_3 = ((y_3_m_1 - mean_y_3)^2 + (y_3_m_2 - mean_y_3)^2 + (y_3_m_3 - mean_y_3)^2 + (y_3_m_4 - mean_y_3)^2 + (y_3_m_5 - mean_y_3)^2)/(5-1),
           V_b_4 = ((y_4_m_1 - mean_y_4)^2 + (y_4_m_2 - mean_y_4)^2 + (y_4_m_3 - mean_y_4)^2 + (y_4_m_4 - mean_y_4)^2 + (y_4_m_5 - mean_y_4)^2)/(5-1),
           V_b_5 = ((y_5_m_1 - mean_y_5)^2 + (y_5_m_2 - mean_y_5)^2 + (y_5_m_3 - mean_y_5)^2 + (y_5_m_4 - mean_y_5)^2 + (y_5_m_5 - mean_y_5)^2)/(5-1),
           se_0 = sqrt(V_w_0 + V_b_0 + V_b_0/5),
           se_1 = sqrt(V_w_1 + V_b_1 + V_b_1/5),
           se_2 = sqrt(V_w_2 + V_b_2 + V_b_2/5),
           se_3 = sqrt(V_w_3 + V_b_3 + V_b_3/5),
           se_4 = sqrt(V_w_4 + V_b_4 + V_b_4/5),
           se_5 = sqrt(V_w_5 + V_b_5 + V_b_5/5),
           t_0 = (mean_y_0^2)/(V_w_0 + V_b_0 + V_b_0/5),
           t_1 = (mean_y_1^2)/(V_w_1 + V_b_1 + V_b_1/5),
           t_2 = (mean_y_2^2)/(V_w_2 + V_b_2 + V_b_2/5),
           t_3 = (mean_y_3^2)/(V_w_3 + V_b_3 + V_b_3/5),
           t_4 = (mean_y_4^2)/(V_w_4 + V_b_4 + V_b_4/5),
           t_5 = (mean_y_5^2)/(V_w_5 + V_b_5 + V_b_5/5),
           conf_lwr_0 = mean_y_0 - t_0*se_0,
           conf_lwr_1 = mean_y_1 - t_1*se_1,
           conf_lwr_2 = mean_y_2 - t_2*se_2,
           conf_lwr_3 = mean_y_3 - t_3*se_3,
           conf_lwr_4 = mean_y_4 - t_4*se_4,
           conf_lwr_5 = mean_y_5 - t_5*se_5,
           conf_upr_0 = mean_y_0 + t_0*se_0,
           conf_upr_1 = mean_y_1 + t_1*se_1,
           conf_upr_2 = mean_y_2 + t_2*se_2,
           conf_upr_3 = mean_y_3 + t_3*se_3,
           conf_upr_4 = mean_y_4 + t_4*se_4,
           conf_upr_5 = mean_y_5 + t_5*se_5
    ) %>% 
    dplyr::select(mean_y_0, mean_y_1, mean_y_2, mean_y_3, mean_y_4, mean_y_5,
                  conf_lwr_0, conf_lwr_1, conf_lwr_2, conf_lwr_3, conf_lwr_4, conf_lwr_5,
                  conf_upr_0, conf_upr_1, conf_upr_2, conf_upr_3, conf_upr_4, conf_upr_5)
  
}

pooled_predictions <- lapply(1:4, average_pooled_predictions)  



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
  
  
  predicted_yield_all_dc_country %>% 
    cbind(worldmap@data$NAME) %>%
    cbind(worldmap@data$ADMIN.1) %>% 
    cbind(worldmap@data$ISO_A2) %>% 
    rename("Country" = 7,
           "Admin" = 8,
           "ISO_A2" = 9) 
  
  
}


pooled_country_predictions <- lapply(1:4, pooled_country_dc) })

write_country_alt <- function(i,j)
  
  {model_country_alt[[i]][[j]] %>% readr::write_csv(here("results", "tables", "model sensitivity", 
                                            paste0("country_alt_model", "_", i, "_", crops[[j]], ".csv")))
  
}

lapply(1:4, function(j){lapply(1:4, write_country_alt, j)})


# the order is model_country_alt[[model_specs]][[crops]]

# comparing all predictions for country, by degree ------------------------

# not the coalesced versions, just the weighted versions


# read m-pooled country results for yield changes
predicted_yield_country_reweighted <- list.files(path = here("results", "tables"), 
                                                 pattern = "predicted_country_weighted") # alphabetical order of crops

predicted_yield_country_reweighted <- lapply(1:4, 
                                             function(i) readr::read_csv(
                                               here("results", "tables", predicted_yield_country_reweighted[[i]]))) # list


# left_join [[1]][[1]], [[2]][[1]], [[3]][[1]], [[4]][[1]] with predicted_yield_country_reweighted[[1]] by Admin, ISO_A2

# repeat for [[1]][[j]], [[2]][[j]], [[3]][[j]], [[4]][[j]]
predicted_weighted_model_comp <- lapply(1:4, function(i){
  
  predicted_weighted_model_comp <- predicted_yield_country_reweighted[[i]] %>%
    left_join(model_country_alt[[1]][[i]], by = c("Admin", "ISO_A2", "Country")) %>% 
    left_join(model_country_alt[[2]][[i]], by = c("Admin", "ISO_A2", "Country")) %>% 
    left_join(model_country_alt[[3]][[i]], by = c("Admin", "ISO_A2", "Country")) %>% 
    left_join(model_country_alt[[4]][[i]], by = c("Admin", "ISO_A2", "Country")) 
  
  setnames(predicted_weighted_model_comp,
           old = c("0 degrees.x",
                   "1 degree.x",
                   "2 degrees.x",
                   "3 degrees.x",
                   "4 degrees.x", 
                   "5 degrees.x",
                   "0 degrees.y",
                   "1 degree.y",
                   "2 degrees.y",
                   "3 degrees.y",
                   "4 degrees.y", 
                   "5 degrees.y",
                   "0 degrees.x.x",
                   "1 degree.x.x",
                   "2 degrees.x.x",
                   "3 degrees.x.x",
                   "4 degrees.x.x", 
                   "5 degrees.x.x",
                   "0 degrees.y.y",
                   "1 degree.y.y",
                   "2 degrees.y.y",
                   "3 degrees.y.y",
                   "4 degrees.y.y", 
                   "5 degrees.y.y",
                   "0 degrees",
                   "1 degree",
                   "2 degrees",
                   "3 degrees",
                   "4 degrees", 
                   "5 degrees"),
           new = c("0dc_m0",
                   "1dc_m0",
                   "2dc_m0",
                   "3dc_m0",
                   "4dc_m0",
                   "5dc_m0",
                   "0dc_m1",
                   "1dc_m1",
                   "2dc_m1",
                   "3dc_m1",
                   "4dc_m1",
                   "5dc_m1",
                   "0dc_m2",
                   "1dc_m2",
                   "2dc_m2",
                   "3dc_m2",
                   "4dc_m2",
                   "5dc_m2",
                   "0dc_m3",
                   "1dc_m3",
                   "2dc_m3",
                   "3dc_m3",
                   "4dc_m3",
                   "5dc_m3",
                   "0dc_m4",
                   "1dc_m4",
                   "2dc_m4",
                   "3dc_m4",
                   "4dc_m4",
                   "5dc_m4"))  
  
  predicted_weighted_model_comp %>% 
    relocate("Country", "Admin", "ISO_A2",
             starts_with("0dc"), starts_with("1dc"), starts_with("2dc"), starts_with("3dc"), starts_with("4dc"), starts_with("5dc"))
  
})

lapply(1:4, function(i) {
  
  predicted_weighted_model_comp %>% write.csv(here("results", "tables", paste0("predicted_weighted_model_comp_", crops[[i]], ".csv")))
})
  

# AGGREGATE TO THE GTAP REGION, WEIGHTED ONLY -----------------------------


country_weighted_model_comp <- lapply(1:4, function(i){
  
  left_join(predicted_weighted_model_comp[[i]], 
            crop_production_list[[i]],
            by = c("Country" = "name")) %>% 
  left_join(mapping, by = c("Country"))
})

# Calculate production weighted regional average

gtap_region_weighted_results_model_comp <- lapply(1:4, function(i) {
  
  country_weighted_model_comp[[i]] %>%
    group_by(id141, n141, c141) %>% 
    summarise(across(starts_with("0dc"), ~weighted.mean(., w = Y, na.rm=TRUE)),
              across(starts_with("1dc"), ~weighted.mean(., w = Y, na.rm=TRUE)),
              across(starts_with("2dc"), ~weighted.mean(., w = Y, na.rm=TRUE)),
              across(starts_with("3dc"), ~weighted.mean(., w = Y, na.rm=TRUE)),
              across(starts_with("4dc"), ~weighted.mean(., w = Y, na.rm=TRUE)),
              across(starts_with("5dc"), ~weighted.mean(., w = Y, na.rm=TRUE)))
  
})


lapply(1:4, function(i) {
  
  readr::write_csv(gtap_region_weighted_results_model_comp[[i]], 
                   here("results", "tables", paste0("gtap_region_weighted_results_model_comp_", crops[[i]], ".csv")))
  
}) 

# there is limited variation across models for maize, slightly higher for rice and wheat (up to 10% points), much higher for soybean (up to 40-50%)

