
  
# READ IN DATA IF NOT FOLLOWING DIRECTLY FROM 09_META_ANALYSIS ------------------------------------------------------------

crops <- c("Maize", "Rice", "Soybean", "Wheat")

crop_imputed_data_restricted <- lapply(1:4, function(j){
  lapply(1:5, function(i,j){
    readRDS(here("processed", paste0("crop_data_", crops[[j]], "_", i, ".Rdata")))
  }, j)
})


# CV EACH MODEL VARIABLE --------------------------------------------------

gam_model_fn <- function(train_data, formula, hyperparameters){
  mgcv::gam(
    formula = formula,
    data = train_data,
    method = 'REML',
    family = 'gaussian'
  )
}

gam_predict_fn <- function(test_data, model, formula, hyperparameters, train_data){
  predict(object = model,
          newdata = test_data,
          allow.new.levels = TRUE)
}



# create k folds

# create function for repeating across crop-imputed datasets 
apply_cv <- function(i,j){
  
  set.seed(123)
  
  # create k folds
  data_kfold <- groupdata2::fold(
    crop_imputed_data_restricted[[j]][[i]], # j for crop (4), i for imputed datasets (5)
    k = 10
  )
  
  cv <- cross_validate_fn(
    data = data_kfold,
    formulas = c(
    # model 1 - gamm relative
  "Yield.Change ~ 0 +
    s(Temp.Change, k = 3) + 
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    s(Reference_fact, bs = 're') + 
    s(Country2_fact, bs = 're')",
  # model 2 - gamm absolute
  "Abs.Yield.Change ~ 0 +
    s(Temp.Change, k = 3) + 
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    s(Reference_fact, bs = 're') + 
    s(Country2_fact, bs = 're')",
  # model 3 - glmm relative
  "Yield.Change ~ 0 + 
    poly(Temp.Change,2) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    s(Reference_fact, bs = 're') + 
    s(Country2_fact, bs = 're')",
  # model 4 - glmm abs
  "Abs.Yield.Change ~ 0 +
    poly(Temp.Change,2) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    Baseline_yield +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    s(Reference_fact, bs = 're') + 
    s(Country2_fact, bs = 're')"
    ),
    type = 'gaussian',
    model_fn = gam_model_fn,
    predict_fn = gam_predict_fn,
    parallel = FALSE)
  
  cv %>% 
    dplyr::mutate(`Model ID` = 1:nrow(cv)) %>% 
    dplyr::arrange(RMSE) %>% 
    select_definitions(additional_includes = c("RMSE", "Model ID", "MAE")) 
  
}

model_cv <- lapply(1:4, function(j){lapply(1:5, apply_cv, j)})

saveRDS(model_cv, here("results", "tables", "model_cv.RData"))

# bind as data table

model_cv_dt <- as.data.table(model_cv)

rbind_model_cv <- function(i){
  
  rbindlist(model_cv_dt[[i]], idcol='m')
  
}

crop_model_cv_list <- lapply(1:4, rbind_model_cv)

crop_model_cv_dt <- rbindlist(crop_model_cv_list, idcol = "crop_no")

crop_model_cv_dt %>% readr::write_csv(here("results", "tables", "model_cv.csv"))

# the results are pretty close for each response-pair of models
# RMSE is probably not the best way to decide on the best model to use