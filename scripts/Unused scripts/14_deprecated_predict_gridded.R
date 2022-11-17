# Predict global gridded predictions, pooled predictions and pooled confidence intervals


# READ IN PREDICTION DATA -------------------------------------------------

crops <- c("Maize", "Rice", "Soybean", "Wheat")

read_prediction_data_csv <- function(i,j){
  
  readr::read_csv(here("processed", paste0("crops_prediction_data_", crops[[j]], "_", i, ".csv")), 
                  col_types = readr::cols(.default = "c"))} # o/w parsing failures means all baseline temperatures get read as NAs


crops_prediction_data <- lapply(1:4, function(j){
  lapply(1:12, read_prediction_data_csv, j)
})

# all variables need to be re-factorised due to how they are saved and read-in as characters from csv
# except for C3 and C4, due to GAMM issues. Model estimates are not affected because binary variable.

refactorise_prediction_data <- function(i,j){
  
  crops_prediction_data[[j]][[i]][,c(4,8)] <- lapply(crops_prediction_data[[j]][[i]][,c(4,8)], factor)
  
  crops_prediction_data[[j]][[i]][,c(1:3, 5, 6:7)] <- lapply(crops_prediction_data[[j]][[i]][,c(1:3, 5, 6:7)], as.numeric)
  
  
  crops_prediction_data[[j]][[i]]
  
}

crops_prediction_data <- lapply(1:4, function(j){
  lapply(1:12, refactorise_prediction_data, j)
})


# PREDICT CENTRAL GRIDDED PREDICTIONS -------------------------------------

# now run script 11 (first need to run script 09 to line 47)

# without country RE (original) -----------------------------------------

# the change between reweighted and no_re results probably related to exclude = c(s,s) no longer working
# it works again: re_form = NA is equivalent to exclude = s(Country2_fact), s(Reference_fact)


predict_across_crops_imputations_no_re <- function(i, j){
  
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
  
  y_0 <- gammit::predict_gamm(fit_weighted_restricted_list[[j]][[1]][[i]],
                              pred_data_0, 
                              exclude = c("s(Country2_fact)", "s(Reference_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit) # must have this arg
  
  y_1 <- gammit::predict_gamm(fit_weighted_restricted_list[[j]][[1]][[i]],
                              pred_data_1, 
                              exclude = c("s(Country2_fact)", "s(Reference_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit)# must have this arg
  
  y_2 <- gammit::predict_gamm(fit_weighted_restricted_list[[j]][[1]][[i]],
                              pred_data_2, # 3 REFERS TO 2 DC
                              exclude = c("s(Country2_fact)", "s(Reference_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit)# must have this arg
  
  y_3 <- gammit::predict_gamm(fit_weighted_restricted_list[[j]][[1]][[i]],
                              pred_data_3, # 4 REFERS TO 3 DC
                              exclude = c("s(Country2_fact)", "s(Reference_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit) # must have this arg
  
  y_4 <- gammit::predict_gamm(fit_weighted_restricted_list[[j]][[1]][[i]],
                              pred_data_4, # 5 REFERS TO 4 DC
                              exclude = c("s(Country2_fact)", "s(Reference_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit)# must have this arg
  
  y_5 <- gammit::predict_gamm(fit_weighted_restricted_list[[j]][[1]][[i]],
                              pred_data_5, # 5 REFERS TO 4 DC
                              exclude = c("s(Country2_fact)", "s(Reference_fact)"), 
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

predictions_across_crops_imputations_no_re <- lapply(1:4, function(j){lapply(1:5, predict_across_crops_imputations_no_re, j)})


# with country RE - amend prediction data ---------------------------------

# for re_form = c("s(Country2_fact)") to work, must have Country2_fact as column in gridded prediction data

# cbind crops_prediction_data with world_d and then left_join by worldmap_clean ISO names

crops_prediction_data_re <- lapply(1:4, function(j){
  lapply(1:12, function(i,j){
    crops_prediction_data[[j]][[i]] %>% 
         cbind(world_d) %>% 
         left_join(dplyr::select(worldmap_clean@data, NAME, ISO_A2), by = c("name"="NAME")) %>% 
      rename(Country2_fact = ISO_A2)}
         , j)
})


# with country RE - predict -------------------------------------------------------


predict_across_crops_imputations <- function(i, j){
  
  pred_data_0 <- crops_prediction_data_re[[j]][[1]] %>% 
    mutate(ncell = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted)) 
  
  pred_data_1 <- crops_prediction_data_re[[j]][[2]] %>% 
    mutate(ncell = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted)) 
  
  pred_data_2 <- crops_prediction_data_re[[j]][[3]] %>% 
    mutate(ncell = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted)) 
  
  pred_data_3 <- crops_prediction_data_re[[j]][[4]] %>% 
    mutate(ncell = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted)) 
  
  pred_data_4 <- crops_prediction_data_re[[j]][[5]] %>% 
    mutate(ncell = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted)) 
  
  pred_data_5 <- crops_prediction_data_re[[j]][[6]] %>% 
    mutate(ncell = row_number()) %>% 
    filter(!is.na(Baseline_tmp_weighted)) 
  
  y_0 <- gammit::predict_gamm(fit_weighted_restricted_list[[j]][[1]][[i]],
                              pred_data_0, 
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit) # must have this arg
  
  y_1 <- gammit::predict_gamm(fit_weighted_restricted_list[[j]][[1]][[i]],
                              pred_data_1, 
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit)# must have this arg
  
  y_2 <- gammit::predict_gamm(fit_weighted_restricted_list[[j]][[1]][[i]],
                              pred_data_2, # 3 REFERS TO 2 DC
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit)# must have this arg
  
  y_3 <- gammit::predict_gamm(fit_weighted_restricted_list[[j]][[1]][[i]],
                              pred_data_3, # 4 REFERS TO 3 DC
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit) # must have this arg
  
  y_4 <- gammit::predict_gamm(fit_weighted_restricted_list[[j]][[1]][[i]],
                              pred_data_4, # 5 REFERS TO 4 DC
                              re_form = c("s(Country2_fact)"), 
                              keep_prediction_data = TRUE,
                              newdata.guaranteed = TRUE,
                              se.fit = TRUE 
  ) %>% 
    as.data.table()  %>% 
    dplyr::select(ncell, prediction.fit, prediction.se.fit)# must have this arg
  
  y_5 <- gammit::predict_gamm(fit_weighted_restricted_list[[j]][[1]][[i]],
                              pred_data_5, # 5 REFERS TO 4 DC
                              re_form = c("s(Country2_fact)"), 
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
    mutate(ncell = row_number()) %>% 
    mutate(y_0_adj = ifelse(y_0 < -50 | y_0 > 50, NA, y_0), # adjust this - to make it easier to see variation across datasets
           y_1_adj = ifelse(y_1 < -50 | y_1 > 50, NA, y_1),
           y_2_adj = ifelse(y_2 < -50 | y_2 > 50, NA, y_2),
           y_3_adj = ifelse(y_3 < -50 | y_3 > 50, NA, y_3),
           y_4_adj = ifelse(y_4 < -50 | y_4 > 50, NA, y_4),
           y_5_adj = ifelse(y_5 < -50 | y_5 > 50, NA, y_5))
  
}

predictions_across_crops_imputations <- lapply(1:4, function(j){lapply(1:5, predict_across_crops_imputations, j)})

# # to go to pooled predictions, skip remainder of this section -----------


# plot

#  scenario_dc_adj <- c("y_0_adj", "y_1_adj", "y_2_adj", "y_3_adj", "y_4_adj", "y_5_adj")
scenario_dc <- c("y_0", "y_1", "y_2", "y_3", "y_4", "y_5")

# plot all imputed predictions for each scenario on the one plot, with one plot for each scenario

predict_gridded <- function(i, j){
  
  raster_m1 <- raster(ncol = 720, nrow = 360) 
  values(raster_m1) <-  predictions_across_crops_imputations[[j]][[1]][[eval(expr(scenario_dc[[i]]))]]
  
  raster_m2 <- raster(ncol = 720, nrow = 360) 
  values(raster_m2) <- predictions_across_crops_imputations[[j]][[2]][[eval(expr(scenario_dc[[i]]))]]
  
  raster_m3 <- raster(ncol = 720, nrow = 360) 
  values(raster_m3) <- predictions_across_crops_imputations[[j]][[3]][[eval(expr(scenario_dc[[i]]))]]
  
  raster_m4 <- raster(ncol = 720, nrow = 360) 
  values(raster_m4) <- predictions_across_crops_imputations[[j]][[4]][[eval(expr(scenario_dc[[i]]))]]
  
  raster_m5 <- raster(ncol = 720, nrow = 360) 
  values(raster_m5) <- predictions_across_crops_imputations[[j]][[5]][[eval(expr(scenario_dc[[i]]))]]
  
  raster_alldc_stack <- stack(raster_m1, raster_m2, raster_m3, raster_m4, raster_m5)
  
  rasterVis::levelplot(raster_alldc_stack, 
                       col.regions = rev(terrain.colors(10000)),
                       names.attr = c("Dataset 1",
                                      "Dataset 2",
                                      "Dataset 3",
                                      "Dataset 4",
                                      "Dataset 5"))
  
}


#  gridded_predictions_list <- lapply(1, function(j){lapply(1:5, predict_gridded, j)})

#  gridded_predictions_list[[1]][[1]] # maize, all imputation specific predictions, y_0_adj
#  gridded_predictions_list[[1]][[2]] # maize, all imputation specific predictions, y_1_adj
#  gridded_predictions_list[[1]][[3]]
#  gridded_predictions_list[[1]][[4]]
#  gridded_predictions_list[[1]][[5]]

# plot all degree scenarios on the one plot, with one plot for each imputed datset


predict_gridded_dc <- function(i, j){
  
  raster_0dc <- raster(ncol = 720, nrow = 360) 
  values(raster_0dc) <-  predictions_across_crops_imputations[[j]][[i]][[eval(expr(scenario_dc[[1]]))]]
  
  raster_1dc <- raster(ncol = 720, nrow = 360) 
  values(raster_1dc) <- predictions_across_crops_imputations[[j]][[i]][[eval(expr(scenario_dc[[2]]))]]
  
  raster_2dc <- raster(ncol = 720, nrow = 360) 
  values(raster_2dc) <- predictions_across_crops_imputations[[j]][[i]][[eval(expr(scenario_dc[[3]]))]]
  
  raster_3dc <- raster(ncol = 720, nrow = 360) 
  values(raster_3dc) <- predictions_across_crops_imputations[[j]][[i]][[eval(expr(scenario_dc[[4]]))]]
  
  raster_4dc <- raster(ncol = 720, nrow = 360) 
  values(raster_4dc) <- predictions_across_crops_imputations[[j]][[i]][[eval(expr(scenario_dc[[5]]))]]
  
  raster_5dc <- raster(ncol = 720, nrow = 360) 
  values(raster_5dc) <- predictions_across_crops_imputations[[j]][[i]][[eval(expr(scenario_dc[[6]]))]]
  
  raster_alldc_stack <- stack(raster_0dc, raster_1dc, raster_2dc, raster_3dc, raster_4dc, raster_5dc)
  
  rasterVis::levelplot(raster_alldc_stack, 
                       col.regions = rev(terrain.colors(10000)),
                       at = seq(-100,100),
                       names.attr = c("0 degrees warming",
                                      "1 degree warming",
                                      "2 degrees warming",
                                      "3 degrees warming",
                                      "4 degrees warming",
                                      "5 degrees warming"))
  
}


gridded_predictions_dc_list <- lapply(1:4, function(j){lapply(1:5, predict_gridded_dc, j)})


# plot 


gridded_predictions_dc_list[[1]][[1]] # maize, m1, all degrees 
gridded_predictions_dc_list[[1]][[2]] # maize, m2, all degrees
gridded_predictions_dc_list[[1]][[3]]
gridded_predictions_dc_list[[1]][[4]]
gridded_predictions_dc_list[[1]][[5]]

gridded_predictions_dc_list[[2]][[1]] # rice, m1, all degrees 
gridded_predictions_dc_list[[2]][[2]] # rice, m2, all degrees
gridded_predictions_dc_list[[2]][[3]]
gridded_predictions_dc_list[[2]][[4]]
gridded_predictions_dc_list[[2]][[5]]

gridded_predictions_dc_list[[3]][[1]] # soy, m1, all degrees 
gridded_predictions_dc_list[[3]][[2]] # soy, m2, all degrees
gridded_predictions_dc_list[[3]][[3]]
gridded_predictions_dc_list[[3]][[4]]
gridded_predictions_dc_list[[3]][[5]]

gridded_predictions_dc_list[[4]][[1]] # wheat, m1, all degrees 
gridded_predictions_dc_list[[4]][[2]] # wheat, m2, all degrees
gridded_predictions_dc_list[[4]][[3]]
gridded_predictions_dc_list[[4]][[4]]
gridded_predictions_dc_list[[4]][[5]]


# PLOT GRIDDED PREDICTIONS ------------------------------------------------



plot_gridded_predictions <- function(i,j){
  
  mypath <- file.path(here("results", "figures"),
                      paste("gridded_predictions", "reweighted", crops[[j]], i, ".png", sep = "_"))
  
  png(file = mypath)
  
  plot(gridded_predictions_dc_list[[j]][[i]])
  
  dev.off()
  
}

gridded_predictions_plots <- lapply(1:4, function(j){lapply(1:5, plot_gridded_predictions, j)})


# POOL PREDICTIONS ACROSS IMPUTED DATASETS --------------------------------------------------------

# run this dynamically for no_re and re versions, change output name to reflect no_re or re

gridded_predictions <-  predictions_across_crops_imputations_no_re

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


# write pooled_predictions to csv -----------------------------------------


lapply(1:4, function(i){
  
  pooled_predictions[[i]] %>% 
    readr::write_csv(here("processed", paste0("pooled_predictions_re_", crops[[i]], ".csv")))
}) 



# plot pooled predictions -------------------------------------------------

pooled_gridded_dc <- function(i){
  
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


pooled_gridded_predictions <- lapply(1:4, pooled_gridded_dc) 

plot_gridded_pooled_predictions <- function(i){
  
  mypath <- file.path(here("results", "figures"),
                      paste("gridded_pooled_predictions_re", crops[[i]], ".png", sep = "_"))
  
  png(file = mypath)
  
  plot(pooled_gridded_predictions[[i]])
  
  dev.off()
  
}

gridded_pooled_predictions_plots <- lapply(1:4, plot_gridded_pooled_predictions)


# PLOT POOLED GRIDDED PREDICTIONS [-50, 50] -------------------------------


pooled_gridded_dc_restricted <- function(i){
  
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
  
  rasterVis::levelplot(raster_alldc_stack, 
                       col.regions = rev(terrain.colors(10000)),
                       at = seq(-50,50),
                       names.attr = c("0 degrees warming",
                                      "1 degree warming",
                                      "2 degrees warming",
                                      "3 degrees warming",
                                      "4 degrees warming",
                                      "5 degrees warming"))
  
} 


pooled_gridded_predictions_restricted <- lapply(1:4, pooled_gridded_dc_restricted) 


plot_gridded_pooled_predictions_restricted <- function(i){
  
  mypath <- file.path(here("results", "figures"),
                      paste("gridded_pooled_predictions_restricted", crops[[i]], ".png", sep = "_"))
  
  png(file = mypath)
  
  plot(pooled_gridded_predictions_restricted[[i]])
  
  dev.off()
  
}

# still need to run this
gridded_pooled_predictions_plots_restricted <- lapply(1:4, plot_gridded_pooled_predictions_restricted)


# POOL STANDARD ERRORS FOR 95% CONFIDENCE INTERVAL PREDICTIONS -------------------------------------------------

# 2.5% percentile

pooled_gridded_dc_conf_lwr <- function(i){
  
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


pooled_gridded_predictions_conf_lwr <- lapply(1:4, pooled_gridded_dc_conf_lwr)

plot_gridded_pooled_predictions_conf_lwr <- function(i){
  
  mypath <- file.path(here("results", "figures"),
                      paste("gridded_pooled_predictions_conf_lwr_re", crops[[i]], ".png", sep = "_"))
  
  png(file = mypath)
  
  plot(pooled_gridded_predictions_conf_lwr[[i]])
  
  dev.off()
  
}

gridded_pooled_predictions_conf_lwr_plots <- lapply(1:4, plot_gridded_pooled_predictions_conf_lwr)

# 97.5% percentile

pooled_gridded_dc_conf_upr <- function(i){
  
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


pooled_gridded_predictions_conf_upr <- lapply(1:4, pooled_gridded_dc_conf_upr)

plot_gridded_pooled_predictions_conf_upr <- function(i){
  
  mypath <- file.path(here("results", "figures"),
                      paste("gridded_pooled_predictions_conf_upr_re", crops[[i]], ".png", sep = "_"))
  
  png(file = mypath)
  
  plot(pooled_gridded_predictions_conf_upr[[i]])
  
  dev.off()
  
}

gridded_pooled_predictions_conf_upr_plots <- lapply(1:4, plot_gridded_pooled_predictions_conf_upr)



pooled_test <- pooled_predictions[[1]] %>% as.data.table()
pooled_test[20000,]




