
# READ IN DATA ------------------------------------------------------------

crops <- c("Maize", "Rice", "Soybean", "Wheat")

DATA_YIELD <- lapply(1:4, function(j){
  lapply(1:5, function(i,j){
    
    readr::read_csv(here("processed", paste0("data_yield_", crops[[j]], "_", i, ".csv")))}, j)
})

# refactorise


names <- c(10:15) # this may need to change to 9:15 owing to new variable Baseline_yield
# this ensures that C3 and C4, binary variables, are deliberately not factorised because cvms does not allow factor vars with less than 2 levels. No change to model estimates bc binary.

DATA_YIELD <- lapply(1:4, function(j){
  lapply(1:5, function(i,j){
    
    DATA_YIELD[[j]][[i]][,names] <- lapply(DATA_YIELD[[j]][[i]][,names], factor)
    
    DATA_YIELD[[j]][[i]]
    
  }, j)
})

# 2. predict original model direct to country -----------------------------------------------

countries_subset <- country_prediction_data_yield[[1]][[1]] %>% 
  dplyr::select(country, name)


predict_country_yield_main <- function(i, j){
  
  pred_data_0 <- country_prediction_data_yield[[j]][[1]] %>% 
    ungroup() %>% 
    mutate(row = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) # should only occur by crop
  
  pred_data_1 <- country_prediction_data_yield[[j]][[2]] %>% 
    ungroup() %>% 
    mutate(row = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  pred_data_2 <- country_prediction_data_yield[[j]][[3]] %>% 
    ungroup() %>% 
    mutate(row = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  pred_data_3 <- country_prediction_data_yield[[j]][[4]] %>% 
    ungroup() %>% 
    mutate(row = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  pred_data_4 <- country_prediction_data_yield[[j]][[5]] %>% 
    ungroup() %>% 
    mutate(row = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  pred_data_5 <- country_prediction_data_yield[[j]][[6]] %>% 
    ungroup() %>% 
    mutate(row = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  y_0 <- gammit::predict_gamm(fit_weighted_restricted_list[[j]][[1]][[i]],
                              pred_data_0, 
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(row, prediction.fit, prediction.se.fit) # must have this arg
  
  y_1 <- gammit::predict_gamm(fit_weighted_restricted_list[[j]][[1]][[i]],
                              pred_data_1, 
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(row, prediction.fit, prediction.se.fit)# must have this arg
  
  y_2 <- gammit::predict_gamm(fit_weighted_restricted_list[[j]][[1]][[i]],
                              pred_data_2, # 3 REFERS TO 2 DC
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(row, prediction.fit, prediction.se.fit)# must have this arg
  
  y_3 <- gammit::predict_gamm(fit_weighted_restricted_list[[j]][[1]][[i]],
                              pred_data_3, # 4 REFERS TO 3 DC
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(row, prediction.fit, prediction.se.fit) # must have this arg
  
  y_4 <- gammit::predict_gamm(fit_weighted_restricted_list[[j]][[1]][[i]],
                              pred_data_4, # 5 REFERS TO 4 DC
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(row, prediction.fit, prediction.se.fit)# must have this arg
  
  y_5 <- gammit::predict_gamm(fit_weighted_restricted_list[[j]][[1]][[i]],
                              pred_data_5, # 5 REFERS TO 4 DC
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(row, prediction.fit, prediction.se.fit)# must have this arg
  
  
  example <- matrix(NA, nrow=191) %>% 
    as.data.table() %>% 
    rename(row = 1) %>%
    mutate(row = row_number()) %>% 
    left_join(y_0, by = "row") %>% 
    rename(y_0 = prediction.fit, y_0_se = prediction.se.fit) %>% 
    mutate(row = row_number()) %>% 
    left_join(y_1, by = "row") %>% 
    rename(y_1 = prediction.fit, y_1_se = prediction.se.fit) %>% 
    mutate(row = row_number()) %>% 
    left_join(y_2, by = "row") %>% 
    rename(y_2 = prediction.fit, y_2_se = prediction.se.fit) %>% 
    mutate(row = row_number()) %>% 
    left_join(y_3, by = "row") %>% 
    rename(y_3 = prediction.fit, y_3_se = prediction.se.fit) %>% 
    mutate(row = row_number()) %>% 
    left_join(y_4, by = "row") %>% 
    rename(y_4 = prediction.fit, y_4_se = prediction.se.fit) %>% 
    mutate(row = row_number()) %>% 
    left_join(y_5, by = "row") %>% 
    rename(y_5 = prediction.fit, y_5_se = prediction.se.fit) %>% 
    mutate(row = row_number()) %>% 
    cbind(countries_subset) %>% # cannot left_join to world_d bc country = row_number()
    left_join(dplyr::select(worldmap_clean@data, NAME, ISO_A2), by = c("name" = "NAME"))
  
}

predictions_country_yield_main <- lapply(1:4, function(j){lapply(1:5, predict_country_yield_main, j)})


rename_country_cols_yield_main <- function(i,j){
  
  colnames(predictions_country_yield_main[[j]][[i]]) <- paste(colnames(predictions_country_yield_main[[j]][[i]]), "m", i, sep = "_")
  predictions_country_yield_main[[j]][[i]]
}

renamed_country_cols_yield_main <- lapply(1:4, function(j){lapply(1:5, rename_country_cols_yield_main, j)})

# merge level two list dataframes for each crop

pool_country_cols_yield_main <- function(i){
  
  cbind(
    renamed_country_cols_yield_main[[i]][[1]],
    renamed_country_cols_yield_main[[i]][[2]],
    renamed_country_cols_yield_main[[i]][[3]],
    renamed_country_cols_yield_main[[i]][[4]],
    renamed_country_cols_yield_main[[i]][[5]]) %>% 
    as_tibble()
  
}      

pooled_country_cols_yield_main <- lapply(1:4, pool_country_cols_yield_main)       

# NAs for many countries including Australia begins here at this step

average_pooled_country_predictions_yield_main <- function(i){
  
  pooled_country_cols_yield_main[[i]] %>% 
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

pooled_predictions_country_yield_main <- lapply(1:4, average_pooled_country_predictions_yield_main)     


lapply(1:4, function(i){
  
  pooled_predictions_country_yield_main[[i]] %>% 
    readr::write_csv(here("processed", paste0("pooled_predictions_re_country_direct_", crops[[i]], ".csv")))
})

# note that the predictions are still similar directions - response functions - maize and rice see increased yields?

# then convert to percentage changes if needed - rename code

# Then convert to % terms from 2015-2020 country baseline mean yield
country_direct_predicted_yields_main <- lapply(1:4, function(i){
  
  pooled_predictions_country_yield_main[[i]] %>% 
    cbind(countries_subset) %>%  # in the order of country_prediction_data_yield[[j]][[i]]
    left_join(country_baseline_list_2[[i]], by = c("name")) %>% 
    dplyr::select(!c("country.x")) %>% 
    rename(country = country.y) %>% 
    relocate(name,country) %>% 
    as_tibble()
  
}) 

lapply(1:4, function(i){
  
  country_direct_predicted_yields_main[[i]] %>% 
    readr::write_csv(here("processed", paste0("country_direct_predicted_yield_re_", crops[[i]], ".csv")))
}) # these declines are far greater than in the version where we only convert to absolute yields 


# plot country predictions ------------------------------------------------

raster_country_direct_predicted_yields_main <- lapply(1:4, function(i) {
  
  country_pred <- world %>% 
    left_join(country_direct_predicted_yields_main[[i]], by = "country")
  
  mean_y_0 <- rasterize(country_pred, r, field = "mean_y_0") # how is this being predicted at the gridded level??
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
                      paste("country_direct_predicted_yields_re", crops[[i]], ".png", sep = "_"))
  
  png(file = mypath)
  
  plot(raster_country_direct_predicted_yields_main[[i]])
  
  dev.off()
  
})




# 4. Model absolute changes as the response variable  -------------------------------------------------

# Model absolute changes (have to use baseline 2000 production data, assumed to be the yield)
# Multiply CGIAR percentage yield change by baseline total yield for each country to obtain absolute change in yield

# use Monfreda yields - NOTE SHOULD CHANGE THIS TO GHDY GRIDDED TIME SERIES DATA


#country_baseline_list_3 <- lapply(1:4, function(i) {
  
#  crop_yield_monf_agg_dt[[i]] %>% 
#    left_join(world_d, by="cell") %>% 
#    as.data.frame() %>% 
#    group_by(country, name) %>% # only 191 countries
#    summarise(mean_yield = mean(yield, na.rm=TRUE)) # sum grid cell predictions for country predicted absolute yield change
  
#}) 

# aggregate yield data to the country level, like what has been done for crop production  


# left_join by worldmap_clean@data `for ISO_A2 classification

#country_baseline_list_4 <- lapply(1:4, function(i) {
#  country_baseline_list_3[[i]] %>% 
#    left_join(dplyr::select(worldmap_clean@data, NAME, ISO_A2), by = c("name" = "NAME"))})

# left_join by country


# Consider including baseline total yield as a predictor - redo model fit


# fit model ---------------------------------------------------------------


fit_maize_abs <- function(i, j){
  
  model <-  mgcv::gam(Abs.Yield.Change ~ 
                        s(Temp.Change, k = 3) + 
                        # s(Baseline_tmp_weighted) + 
                        Temp.Change:Baseline_tmp_weighted +
                        s(Precipitation.change) +
                        Baseline_yield + # baseline total yield, included as a fixed effect
                        f_CO2:C3 +
                        f_CO2:C4 +
                        adapt_dummy +
                        Temp.Change:adapt_dummy +
                        s(Reference_fact, bs = 're') + 
                        s(Country2_fact, bs = 're'), 
                      method = 'REML', 
                      family = 'gaussian',
                      # weights = 1/(se^2), 
                      data = DATA_YIELD[[j]][[i]]) 
  
}


model_maize_abs <- lapply(1, function(j){lapply(1:5, fit_maize_abs, j)})

# note the index goes [[imputed dataset]][[crop]]

# run rice model

fit_rice_abs <- function(i, j){
  
  
  model <-  mgcv::gam(Abs.Yield.Change ~ 
                        s(Temp.Change, k = 3) + 
                        # s(Baseline_tmp_weighted) + 
                        Temp.Change:Baseline_tmp_weighted +
                        s(Precipitation.change) +
                        Baseline_yield +
                        f_CO2:C3 +
                        f_CO2:C4 +
                        adapt_dummy +
                        Temp.Change:adapt_dummy +
                        s(Reference_fact, bs = 're') + 
                        s(Country2_fact, bs = 're'), 
                      method = 'REML', 
                      family = 'gaussian',
                      #  weights = 1/(se^2), 
                      data = DATA_YIELD[[j]][[i]]) 
  
}


model_rice_abs <- lapply(2, function(j){lapply(1:5, fit_rice_abs, j)})

# run soy model

fit_soy_abs <- function(i, j){
  
  
  model <-  mgcv::gam(Abs.Yield.Change ~ 
                        s(Temp.Change, k = 3 ) +
                        Temp.Change:Baseline_tmp_weighted +
                        s(Precipitation.change) +
                        Baseline_yield +
                        f_CO2:C3 +
                        f_CO2:C4 +
                        adapt_dummy +
                        Temp.Change:adapt_dummy +
                        s(Reference_fact, bs = 're') +
                        s(Country2_fact, bs = 're'), 
                      method = 'REML', 
                      family = 'gaussian',
                      # weights = 1/(se^2), 
                      data = DATA_YIELD[[j]][[i]]) 
  
}


model_soy_abs <- lapply(3, function(j){lapply(1:5, fit_soy_abs, j)})

# WHEAT

fit_wheat_abs <- function(i, j){
  
  
  model <-  mgcv::gam(Abs.Yield.Change ~ 
                        s(Temp.Change, k = 3) +
                        Temp.Change:Baseline_tmp_weighted +
                        s(Precipitation.change) +
                        Baseline_yield +
                        f_CO2:C3 +
                        f_CO2:C4 +
                        adapt_dummy +
                        Temp.Change:adapt_dummy +
                        s(Reference_fact, bs = 're') +
                        s(Country2_fact, bs = 're'), 
                      method = 'REML', 
                      family = 'gaussian',
                      #  weights = weights, 
                      data = DATA_YIELD[[j]][[i]]) 
  
}

model_wheat_abs <- lapply(4, function(j){lapply(1:5, fit_wheat_abs, j)})


fit_model_abs_list <- list(model_maize_abs,
                           model_rice_abs,
                           model_soy_abs,
                           model_wheat_abs)


# create prediction data --------------------------------------------------


# read in prediction data (start of script 14)

# now cbind with gridded yield data for yield predictor

crops_prediction_data_yield <- lapply(1:4, function(j) {
  lapply(1:12, function(i, j){
    crops_prediction_data[[j]][[i]] %>% 
      cbind(crop_yield_gaez_agg_dt[[j]]) %>% # gridded yield data - but using baseline 2015 data this time
      #mutate(Pct.Precipitation.Change = Precipitation.change/100) %>%
      rename(Baseline_yield = yield) }, j)
}
)

sum(is.na(crops_prediction_data_yield[[1]][[1]]$mean_yield))


# predict model at gridded level then aggregate -----------------------------------------------------------

# Then predict based on 2015-2020 prediction data, for absolute yield changes

predict_across_crops_yield <- function(i, j){
  
  pred_data_0 <- crops_prediction_data_yield[[j]][[1]] %>% 
    mutate(ncell = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  pred_data_1 <- crops_prediction_data_yield[[j]][[2]] %>% 
    mutate(ncell = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  pred_data_2 <- crops_prediction_data_yield[[j]][[3]] %>% 
    mutate(ncell = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  pred_data_3 <- crops_prediction_data_yield[[j]][[4]] %>% 
    mutate(ncell = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  pred_data_4 <- crops_prediction_data_yield[[j]][[5]] %>% 
    mutate(ncell = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  pred_data_5 <- crops_prediction_data_yield[[j]][[6]] %>% 
    mutate(ncell = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  y_0 <- gammit::predict_gamm(fit_model_abs_list[[j]][[1]][[i]],
                              pred_data_0, 
                              re_form = c("s(Country_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit) # must have this arg
  
  y_1 <- gammit::predict_gamm(fit_model_abs_list[[j]][[1]][[i]],
                              pred_data_1, 
                              re_form = c("s(Country_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit)# must have this arg
  
  y_2 <- gammit::predict_gamm(fit_model_abs_list[[j]][[1]][[i]],
                              pred_data_2, # 3 REFERS TO 2 DC
                              re_form = c("s(Country_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit)# must have this arg
  
  y_3 <- gammit::predict_gamm(fit_model_abs_list[[j]][[1]][[i]],
                              pred_data_3, # 4 REFERS TO 3 DC
                              re_form = c("s(Country_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit) # must have this arg
  
  y_4 <- gammit::predict_gamm(fit_model_abs_list[[j]][[1]][[i]],
                              pred_data_4, # 5 REFERS TO 4 DC
                              re_form = c("s(Country_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit)# must have this arg
  
  y_5 <- gammit::predict_gamm(fit_model_abs_list[[j]][[1]][[i]],
                              pred_data_5, # 5 REFERS TO 4 DC
                              re_form = c("s(Country_fact)"), 
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

predictions_across_crops_yield <- lapply(1:4, function(j){lapply(1:5, predict_across_crops_yield, j)})


rename_prediction_cols_yield <- function(i,j){
  
  colnames(predictions_across_crops_yield[[j]][[i]]) <- paste(colnames(predictions_across_crops_yield[[j]][[i]]), "m", i, sep = "_")
  predictions_across_crops_yield[[j]][[i]]
}

renamed_prediction_cols_yield <- lapply(1:4, function(j){lapply(1:5, rename_prediction_cols_yield, j)})

# merge level two list dataframes for each crop

pool_prediction_cols_yield <- function(i){
  
  cbind(
    renamed_prediction_cols_yield[[i]][[1]],
    renamed_prediction_cols_yield[[i]][[2]],
    renamed_prediction_cols_yield[[i]][[3]],
    renamed_prediction_cols_yield[[i]][[4]],
    renamed_prediction_cols_yield[[i]][[5]])
  
}      

pooled_prediction_cols_yield <- lapply(1:4, pool_prediction_cols_yield)          

average_pooled_predictions_yield <- function(i){
  
  pooled_prediction_cols_yield[[i]] %>% 
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

pooled_predictions_yield <- lapply(1:4, average_pooled_predictions_yield)     


lapply(1:4, function(i){
  
  pooled_predictions_yield[[i]] %>% 
    readr::write_csv(here("processed", paste0("pooled_predictions_yield_", crops[[i]], ".csv")))
})

# aggregate to country ----------------------------------------------------

# Then aggregate to country

country_predicted_abs_yields <- lapply(1:4, function(i) {
  
  pooled_predictions_yield[[i]] %>%
    cbind(world_d) %>% # know that this procedure may not be totally accurate - have missed countries as NA before
    as.data.frame() %>% 
    group_by(country, name) %>%
    summarise(across(1:18, ~mean(.x,na.rm=TRUE)))
  
}) # this gives absolute change in yields (in t/ha)

# repeat aggregation using exactextractr instead

pooled_gridded_yield <- function(i){
  
  raster_0dc <- raster(ncol = 720, nrow = 360) 
  values(raster_0dc) <-  pooled_predictions_yield[[i]][["mean_y_0"]]
  
  raster_1dc <- raster(ncol = 720, nrow = 360) 
  values(raster_1dc) <- pooled_predictions_yield[[i]][["mean_y_1"]]
  
  raster_2dc <- raster(ncol = 720, nrow = 360) 
  values(raster_2dc) <- pooled_predictions_yield[[i]][["mean_y_2"]]
  
  raster_3dc <- raster(ncol = 720, nrow = 360) 
  values(raster_3dc) <- pooled_predictions_yield[[i]][["mean_y_3"]]
  
  raster_4dc <- raster(ncol = 720, nrow = 360) 
  values(raster_4dc) <- pooled_predictions_yield[[i]][["mean_y_4"]]
  
  raster_5dc <- raster(ncol = 720, nrow = 360) 
  values(raster_5dc) <- pooled_predictions_yield[[i]][["mean_y_5"]]
  
  raster_alldc_stack <- stack(raster_0dc, raster_1dc, raster_2dc, raster_3dc, raster_4dc, raster_5dc)
  
  rasterVis::levelplot(raster_alldc_stack, 
                       col.regions = rev(terrain.colors(10000)),
                       # at = seq(-100,100),
                       names.attr = c("0 degrees warming",
                                      "1 degree warming",
                                      "2 degrees warming",
                                      "3 degrees warming",
                                      "4 degrees warming",
                                      "5 degrees warming"))
  
} 


lapply(1:4, pooled_gridded_yield) 

# convert to pct changes --------------------------------------------------

# Then convert to % terms from 2015-2020 country baseline mean yield
country_predicted_abs_yields <- lapply(1:4, function(i){
  
  country_predicted_abs_yields[[i]] %>% 
    left_join(country_baseline_list_2[[i]], by = c("name"))
  
}) # this gives absolute yield changes (in t/ha)

country_predicted_abs_yields[[1]] %>% dplyr::select(country.x, name, mean_yield) # this gives baseline mean country yield (in t/ha)

# for every crop dataset, calculate as percentage of Y

country_predicted_pct_yields <- lapply(1:4, function(i) {
  
  yield_pct <- lapply(country_predicted_abs_yields[[i]][3:20], function(x) 
  {x/country_predicted_abs_yields[[i]]$mean_yield}
  )
  
  yield_pct %>% as.data.table() %>% 
    cbind(country = country_predicted_abs_yields[[i]]$country.x,
          name = country_predicted_abs_yields[[i]]$name) %>% 
    relocate(country, name)
  
})


lapply(1:4, function(i){
  
  country_predicted_pct_yields[[i]] %>% 
    readr::write_csv(here("processed", paste0("country_predicted_modelled_pct_yields_", crops[[i]], ".csv")))
}) # these declines are far greater than in the version where we only convert to absolute yields 

# especially Horn of Africa in wheat - implausible
# and unexpected direction of change for maize and rice
# try respecifying model

# plot country predictions --------------------------------------------------------


modelled_yield_predictions <- lapply(1:4, function(i) {
  
  country_pred <- world %>% 
    left_join(country_predicted_pct_yields[[i]], by = "country")
  
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



plot_country_modelled_yield_predictions <- function(i){
  
  mypath <- file.path(here("results", "figures"),
                      paste("country_modelled_yield_predictions", crops[[i]], ".png", sep = "_"))
  
  png(file = mypath)
  
  plot(modelled_yield_predictions[[i]])
  
  dev.off()
  
}

lapply(1:4, plot_country_modelled_yield_predictions)

# park this model respecification and predict directly to country, then pool, then convert back to % yield change

# Predict country absolute yields directly -------------------------------

# predicting directly to country doesn't change the fact that response functions are still strange

# predict directly to country - create prediction data at the country level


# create country level prediction data -------------------------------------

calculate_mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

country_prediction_data_yield <- lapply(1:4, function(j) {
  lapply(1:12, function(i, j){
    crops_prediction_data_yield[[j]][[i]] %>% 
      left_join(world_d, by = "cell") %>% # country information by cell
      left_join(dplyr::select(worldmap_clean@data, NAME, ISO_A2), by = c("name" = "NAME")) %>% 
      rename(Country2_fact = ISO_A2) %>% 
      group_by(country, name, Country2_fact) %>% 
      summarise(across(where(is.double), ~mean(.x,na.rm=TRUE)),
                across(where(is.factor), ~calculate_mode(.x)))}, j) }
)


# predict model at country level-----------------------------------------------------------

# grab truncated list of 191 countries in exact order from prediction data

countries_subset <- country_prediction_data_yield[[1]][[1]] %>% 
  dplyr::select(country, name)


predict_country_yield <- function(i, j){
  
  pred_data_0 <- country_prediction_data_yield[[j]][[1]] %>% 
    ungroup() %>% 
    mutate(row = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) # should only occur by crop
  
  pred_data_1 <- country_prediction_data_yield[[j]][[2]] %>% 
    ungroup() %>% 
    mutate(row = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  pred_data_2 <- country_prediction_data_yield[[j]][[3]] %>% 
    ungroup() %>% 
    mutate(row = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  pred_data_3 <- country_prediction_data_yield[[j]][[4]] %>% 
    ungroup() %>% 
    mutate(row = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  pred_data_4 <- country_prediction_data_yield[[j]][[5]] %>% 
    ungroup() %>% 
    mutate(row = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  pred_data_5 <- country_prediction_data_yield[[j]][[6]] %>% 
    ungroup() %>% 
    mutate(row = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  y_0 <- gammit::predict_gamm(fit_model_abs_list[[j]][[1]][[i]],
                              pred_data_0, 
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(row, prediction.fit, prediction.se.fit) # must have this arg
  
  y_1 <- gammit::predict_gamm(fit_model_abs_list[[j]][[1]][[i]],
                              pred_data_1, 
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(row, prediction.fit, prediction.se.fit)# must have this arg
  
  y_2 <- gammit::predict_gamm(fit_model_abs_list[[j]][[1]][[i]],
                              pred_data_2, # 3 REFERS TO 2 DC
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(row, prediction.fit, prediction.se.fit)# must have this arg
  
  y_3 <- gammit::predict_gamm(fit_model_abs_list[[j]][[1]][[i]],
                              pred_data_3, # 4 REFERS TO 3 DC
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(row, prediction.fit, prediction.se.fit) # must have this arg
  
  y_4 <- gammit::predict_gamm(fit_model_abs_list[[j]][[1]][[i]],
                              pred_data_4, # 5 REFERS TO 4 DC
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(row, prediction.fit, prediction.se.fit)# must have this arg
  
  y_5 <- gammit::predict_gamm(fit_model_abs_list[[j]][[1]][[i]],
                              pred_data_5, # 5 REFERS TO 4 DC
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(row, prediction.fit, prediction.se.fit)# must have this arg
  
  
  example <- matrix(NA, nrow=191) %>% 
    as.data.table() %>% 
    rename(row = 1) %>%
    mutate(row = row_number()) %>% 
    left_join(y_0, by = "row") %>% 
    rename(y_0 = prediction.fit, y_0_se = prediction.se.fit) %>% 
    mutate(row = row_number()) %>% 
    left_join(y_1, by = "row") %>% 
    rename(y_1 = prediction.fit, y_1_se = prediction.se.fit) %>% 
    mutate(row = row_number()) %>% 
    left_join(y_2, by = "row") %>% 
    rename(y_2 = prediction.fit, y_2_se = prediction.se.fit) %>% 
    mutate(row = row_number()) %>% 
    left_join(y_3, by = "row") %>% 
    rename(y_3 = prediction.fit, y_3_se = prediction.se.fit) %>% 
    mutate(row = row_number()) %>% 
    left_join(y_4, by = "row") %>% 
    rename(y_4 = prediction.fit, y_4_se = prediction.se.fit) %>% 
    mutate(row = row_number()) %>% 
    left_join(y_5, by = "row") %>% 
    rename(y_5 = prediction.fit, y_5_se = prediction.se.fit) %>% 
    mutate(row = row_number()) %>% 
    cbind(countries_subset) %>% # cannot left_join to world_d bc country = row_number()
    left_join(dplyr::select(worldmap_clean@data, NAME, ISO_A2), by = c("name" = "NAME"))
  
}
predictions_country_yield <- lapply(1:4, function(j){lapply(1:5, predict_country_yield, j)})


rename_country_cols_yield <- function(i,j){
  
  colnames(predictions_country_yield[[j]][[i]]) <- paste(colnames(predictions_country_yield[[j]][[i]]), "m", i, sep = "_")
  predictions_country_yield[[j]][[i]]
}

renamed_country_cols_yield <- lapply(1:4, function(j){lapply(1:5, rename_country_cols_yield, j)})

# merge level two list dataframes for each crop

pool_country_cols_yield <- function(i){
  
  cbind(
    renamed_country_cols_yield[[i]][[1]],
    renamed_country_cols_yield[[i]][[2]],
    renamed_country_cols_yield[[i]][[3]],
    renamed_country_cols_yield[[i]][[4]],
    renamed_country_cols_yield[[i]][[5]]) %>% 
    as_tibble()
  
}      

pooled_country_cols_yield <- lapply(1:4, pool_country_cols_yield)       

# NAs for many countries including Australia begins here at this step

average_pooled_country_predictions_yield <- function(i){
  
  pooled_country_cols_yield[[i]] %>% 
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

pooled_predictions_country_yield <- lapply(1:4, average_pooled_country_predictions_yield)     


lapply(1:4, function(i){
  
  pooled_predictions_country_yield[[i]] %>% 
    readr::write_csv(here("processed", paste0("pooled_predictions_country_yield_re_", crops[[i]], ".csv")))
})


# Then convert to % terms from 2015-2020 country baseline mean yield
country_direct_predicted_yields <- lapply(1:4, function(i){
  
  pooled_predictions_country_yield[[i]] %>% 
    cbind(countries_subset) %>%  # in the order of country_prediction_data_yield[[j]][[i]]
    left_join(country_baseline_list_2[[i]], by = c("name")) %>% 
    dplyr::select(!c("country.x")) %>% 
    rename(country = country.y) %>% 
    relocate(name,country) %>% 
    as_tibble()
  
}) # this gives absolute yield changes (in t/ha)

country_direct_predicted_yields[[1]] %>% dplyr::select(country, name, mean_yield) # this gives baseline mean country yield (in t/ha)

# for every crop dataset, calculate as percentage of Y

country_direct_predicted_pct_yields <- lapply(1:4, function(i) {
  
  yield_pct <- lapply(country_direct_predicted_yields[[i]][3:21], function(x) 
  {x/country_direct_predicted_yields[[i]]$mean_yield * 100}
  )
  
  yield_pct %>% as.data.table() %>% 
    cbind(country = country_direct_predicted_yields[[i]]$country,
          name = country_direct_predicted_yields[[i]]$name) %>% 
    relocate(country, name) %>% 
    as_tibble()
  
})


lapply(1:4, function(i){
  
  country_direct_predicted_pct_yields[[i]] %>% 
    readr::write_csv(here("processed", paste0("country_predicted_modelled_pct_yields_re_", crops[[i]], ".csv")))
}) # these declines are far greater than in the version where we only convert to absolute yields 

# especially Horn of Africa in wheat - implausible
# and unexpected direction of change for maize and rice
# try respecifying model


# plot country predictions ------------------------------------------------

modelled_yield_predictions_direct <- lapply(1:4, function(i) {
  
  country_pred <- world %>% 
    left_join(country_direct_predicted_pct_yields[[i]], by = "country")
  
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
                      paste("country_modelled_yield_predictions_direct_re", crops[[i]], ".png", sep = "_"))
  
  png(file = mypath)
  
  plot(modelled_yield_predictions_direct[[i]])
  
  dev.off()
  
})


# 5. Model absolute yield levels ------------------------------------------


fit_maize_lvls <- function(i, j){
  
  model <-  mgcv::gam(Yield.Level ~ 
                        s(Temp.Change) + 
                        # s(Baseline_tmp_weighted) + 
                        Temp.Change:Baseline_tmp_weighted +
                        s(Precipitation.change) +
                        # Baseline_yield + # baseline total yield, included as a fixed effect
                        f_CO2:C3 +
                        f_CO2:C4 +
                        adapt_dummy +
                        Temp.Change:adapt_dummy +
                        s(Reference_fact, bs = 're') + 
                        s(Country2_fact, bs = 're'), 
                      method = 'REML', 
                      family = 'gaussian',
                      # weights = 1/(se^2), 
                      data = DATA_YIELD[[j]][[i]]) 
  
}


model_maize_lvls <- lapply(1, function(j){lapply(1:5, fit_maize_lvls, j)})

# note the index goes [[imputed dataset]][[crop]]

# run rice model

fit_rice_lvls <- function(i, j){
  
  
  model <-  mgcv::gam(Yield.Level ~ 
                        s(Temp.Change) + 
                        # s(Baseline_tmp_weighted) + 
                        Temp.Change:Baseline_tmp_weighted +
                        s(Precipitation.change) +
                        # Baseline_yield +
                        f_CO2:C3 +
                        f_CO2:C4 +
                        adapt_dummy +
                        Temp.Change:adapt_dummy +
                        s(Reference_fact, bs = 're') + 
                        s(Country2_fact, bs = 're'), 
                      method = 'REML', 
                      family = 'gaussian',
                      #  weights = 1/(se^2), 
                      data = DATA_YIELD[[j]][[i]]) 
  
}


model_rice_lvls <- lapply(2, function(j){lapply(1:5, fit_rice_lvls, j)})

# run soy model

fit_soy_lvls <- function(i, j){
  
  
  model <-  mgcv::gam(Yield.Level ~ 
                        s(Temp.Change) +
                        Temp.Change:Baseline_tmp_weighted +
                        s(Precipitation.change) +
                        # Baseline_yield +
                        f_CO2:C3 +
                        f_CO2:C4 +
                        adapt_dummy +
                        Temp.Change:adapt_dummy +
                        s(Reference_fact, bs = 're') +
                        s(Country2_fact, bs = 're'), 
                      method = 'REML', 
                      family = 'gaussian',
                      # weights = 1/(se^2), 
                      data = DATA_YIELD[[j]][[i]]) 
  
}


model_soy_lvls <- lapply(3, function(j){lapply(1:5, fit_soy_lvls, j)})

# WHEAT

fit_wheat_lvls <- function(i, j){
  
  
  model <-  mgcv::gam(Yield.Level ~ 
                        s(Temp.Change) +
                        Temp.Change:Baseline_tmp_weighted +
                        s(Precipitation.change) +
                        # Baseline_yield +
                        f_CO2:C3 +
                        f_CO2:C4 +
                        adapt_dummy +
                        Temp.Change:adapt_dummy +
                        s(Reference_fact, bs = 're') +
                        s(Country2_fact, bs = 're'), 
                      method = 'REML', 
                      family = 'gaussian',
                      #  weights = weights, 
                      data = DATA_YIELD[[j]][[i]]) 
  
}

model_wheat_lvls <- lapply(4, function(j){lapply(1:5, fit_wheat_lvls, j)})


fit_model_lvls_list <- list(model_maize_lvls,
                           model_rice_lvls,
                           model_soy_lvls,
                           model_wheat_lvls)




predict_country_yieldlvls <- function(i, j){
  
  pred_data_0 <- country_prediction_data_yield[[j]][[1]] %>% 
    ungroup() %>% 
    mutate(row = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) # should only occur by crop
  
  pred_data_1 <- country_prediction_data_yield[[j]][[2]] %>% 
    ungroup() %>% 
    mutate(row = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  pred_data_2 <- country_prediction_data_yield[[j]][[3]] %>% 
    ungroup() %>% 
    mutate(row = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  pred_data_3 <- country_prediction_data_yield[[j]][[4]] %>% 
    ungroup() %>% 
    mutate(row = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  pred_data_4 <- country_prediction_data_yield[[j]][[5]] %>% 
    ungroup() %>% 
    mutate(row = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  pred_data_5 <- country_prediction_data_yield[[j]][[6]] %>% 
    ungroup() %>% 
    mutate(row = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted) & !is.na(Baseline_yield)) 
  
  y_0 <- gammit::predict_gamm(fit_model_lvls_list[[j]][[1]][[i]],
                              pred_data_0, 
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(row, prediction.fit, prediction.se.fit) # must have this arg
  
  y_1 <- gammit::predict_gamm(fit_model_lvls_list[[j]][[1]][[i]],
                              pred_data_1, 
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(row, prediction.fit, prediction.se.fit)# must have this arg
  
  y_2 <- gammit::predict_gamm(fit_model_lvls_list[[j]][[1]][[i]],
                              pred_data_2, # 3 REFERS TO 2 DC
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(row, prediction.fit, prediction.se.fit)# must have this arg
  
  y_3 <- gammit::predict_gamm(fit_model_lvls_list[[j]][[1]][[i]],
                              pred_data_3, # 4 REFERS TO 3 DC
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(row, prediction.fit, prediction.se.fit) # must have this arg
  
  y_4 <- gammit::predict_gamm(fit_model_lvls_list[[j]][[1]][[i]],
                              pred_data_4, # 5 REFERS TO 4 DC
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(row, prediction.fit, prediction.se.fit)# must have this arg
  
  y_5 <- gammit::predict_gamm(fit_model_lvls_list[[j]][[1]][[i]],
                              pred_data_5, # 5 REFERS TO 4 DC
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(row, prediction.fit, prediction.se.fit)# must have this arg
  
  
  example <- matrix(NA, nrow=191) %>% 
    as.data.table() %>% 
    rename(row = 1) %>%
    mutate(row = row_number()) %>% 
    left_join(y_0, by = "row") %>% 
    rename(y_0 = prediction.fit, y_0_se = prediction.se.fit) %>% 
    mutate(row = row_number()) %>% 
    left_join(y_1, by = "row") %>% 
    rename(y_1 = prediction.fit, y_1_se = prediction.se.fit) %>% 
    mutate(row = row_number()) %>% 
    left_join(y_2, by = "row") %>% 
    rename(y_2 = prediction.fit, y_2_se = prediction.se.fit) %>% 
    mutate(row = row_number()) %>% 
    left_join(y_3, by = "row") %>% 
    rename(y_3 = prediction.fit, y_3_se = prediction.se.fit) %>% 
    mutate(row = row_number()) %>% 
    left_join(y_4, by = "row") %>% 
    rename(y_4 = prediction.fit, y_4_se = prediction.se.fit) %>% 
    mutate(row = row_number()) %>% 
    left_join(y_5, by = "row") %>% 
    rename(y_5 = prediction.fit, y_5_se = prediction.se.fit) %>% 
    mutate(row = row_number()) %>% 
    cbind(countries_subset) %>% # cannot left_join to world_d bc country = row_number()
    left_join(dplyr::select(worldmap_clean@data, NAME, ISO_A2), by = c("name" = "NAME"))
  
}


predictions_country_yieldlvls <- lapply(1:4, function(j){lapply(1:5, predict_country_yieldlvls, j)})


rename_country_cols_yieldlvls <- function(i,j){
  
  colnames(predictions_country_yieldlvls[[j]][[i]]) <- paste(colnames(predictions_country_yieldlvls[[j]][[i]]), "m", i, sep = "_")
  predictions_country_yieldlvls[[j]][[i]]
}

renamed_country_cols_yieldlvls <- lapply(1:4, function(j){lapply(1:5, rename_country_cols_yieldlvls, j)})

# merge level two list dataframes for each crop

pool_country_cols_yieldlvls <- function(i){
  
  cbind(
    renamed_country_cols_yieldlvls[[i]][[1]],
    renamed_country_cols_yieldlvls[[i]][[2]],
    renamed_country_cols_yieldlvls[[i]][[3]],
    renamed_country_cols_yieldlvls[[i]][[4]],
    renamed_country_cols_yieldlvls[[i]][[5]]) %>% 
    as_tibble()
  
}      

pooled_country_cols_yieldlvls <- lapply(1:4, pool_country_cols_yieldlvls)       

# NAs for many countries including Australia begins here at this step

average_pooled_country_predictions_yieldlvls <- function(i){
  
  pooled_country_cols_yieldlvls[[i]] %>% 
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

pooled_predictions_country_yieldlvls <- lapply(1:4, average_pooled_country_predictions_yieldlvls)     


lapply(1:4, function(i){
  
  pooled_predictions_country_yieldlvls[[i]] %>% 
    readr::write_csv(here("processed", paste0("pooled_predictions_country_yieldlvls_re_", crops[[i]], ".csv")))
})

# note that the predictions are still similar directions - response functions - maize and rice see increased yields?

# then convert to percentage changes if needed - rename code

# Then convert to % terms from 2015-2020 country baseline mean yield
country_direct_predicted_yieldlvls <- lapply(1:4, function(i){
  
  pooled_predictions_country_yieldlvls[[i]] %>% 
    cbind(countries_subset) %>%  # in the order of country_prediction_data_yield[[j]][[i]]
    left_join(country_baseline_list_2[[i]], by = c("name")) %>% 
    dplyr::select(!c("country.x")) %>% 
    rename(country = country.y) %>% 
    relocate(name,country) %>% 
    as_tibble()
  
}) # this gives absolute yield changes (in t/ha)

# for every crop dataset, calculate as percentage of Y

country_direct_predicted_pct_yieldlvls <- lapply(1:4, function(i) {
  
  yield_pct <- lapply(country_direct_predicted_yieldlvls[[i]][3:20], function(x) 
  {(x/country_direct_predicted_yieldlvls[[i]]$mean_yield-1)*100}
  )
  
  yield_pct %>% as.data.table() %>% 
    cbind(country = country_direct_predicted_yieldlvls[[i]]$country,
          name = country_direct_predicted_yieldlvls[[i]]$name) %>% 
    relocate(country, name) %>% 
    as_tibble()
  
})


lapply(1:4, function(i){
  
  country_direct_predicted_pct_yieldlvls[[i]] %>% 
    readr::write_csv(here("processed", paste0("country_predicted_modelled_pct_yieldlvls_re_", crops[[i]], ".csv")))
}) # these declines are far greater than in the version where we only convert to absolute yields 



# plot country predictions ------------------------------------------------


modelled_yieldlvls_predictions_direct <- lapply(1:4, function(i) {
  
  country_pred <- world %>% 
    left_join(country_direct_predicted_pct_yieldlvls[[i]], by = "country")
  
  mean_y_0 <- rasterize(country_pred, r, field = "mean_y_0") # how is this being predicted at the gridded level??
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
                      paste("country_modelled_yieldlvls_predictions_direct_re", crops[[i]], ".png", sep = "_"))
  
  png(file = mypath)
  
  plot(modelled_yieldlvls_predictions_direct[[i]])
  
  dev.off()
  
})


