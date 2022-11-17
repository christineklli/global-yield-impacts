
# create k folds

# create function for repeating across crop-imputed datasets 
apply_cv <- function(data){
  
  
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
  
  lapply(1:4, function(j){
    lapply(1:5, function(i,j){
    
      
      
      set.seed(123)
      
      # create k folds
      data_kfold <- groupdata2::fold(
        data[[j]][[i]], # j for crop (4), i for imputed datasets (5)
        k = 10
      )
      
      cv <- cross_validate_fn(
        data = data_kfold,
        formulas = c(
          # model 1 - gamm 
          "Yield.Change ~ 0 +
      s(Temp.Change) + 
      s(Precipitation.change) +
      Temp.Change:Baseline_tmp +
      Precipitation.change:Baseline_pre +
      Temp.Change:Precipitation.change +
      f_CO2:C3 +
      f_CO2:C4 +
      adapt_dummy +
      Temp.Change:adapt_dummy +
      s(Reference_fact, bs='re') + # random intercept
      s(Temp.Change, Reference_fact, bs = 're') + # random slope - order of vars shouldnt matter
      s(Precipitation.change, Reference_fact, bs = 're') +
      s(f_CO2, Reference_fact, bs = 're') +
      s(Country2_fact, bs = 're')",
          
          # model 2 - glmm 
          "Yield.Change ~ 0 + 
      poly(Temp.Change,2) +
      poly(Precipitation.change, 2) +
      Temp.Change:Baseline_tmp +
      Precipitation.change:Baseline_pre +
      Temp.Change:Precipitation.change +
      f_CO2:C3 +
      f_CO2:C4 +
      adapt_dummy +
      Temp.Change:adapt_dummy +
      s(Reference_fact, bs = 're') + 
      s(Temp.Change, Reference_fact, bs = 're') + 
      s(Precipitation.change, Reference_fact, bs = 're') +
      s(f_CO2, Reference_fact, bs = 're') +
      s(Country2_fact, bs = 're')",
          
          # model 3 - lm 
          "Yield.Change ~ 0 +
    Temp.Change +
    I(Temp.Change)^2 +
    Temp.Change:Baseline_tmp +
    Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy"
          
        ),
        type = 'gaussian',
        model_fn = gam_model_fn,
        predict_fn = gam_predict_fn,
        parallel = FALSE)
      
      cv %>% 
        dplyr::mutate(`Model ID` = 1:nrow(cv)) %>% 
        dplyr::arrange(RMSE) %>% 
        select_definitions(additional_includes = c("RMSE", "Model ID", "MAE")) %>% 
        as.data.table()
      
    }, j)})
}
  

# bind as data table


rbind_model_cv <- function(list){
  
  lapply(1:4, function(i){
  
  rbindlist(list[[i]], idcol='m')
  
  })
  
  
}

rbind_model_cv_crop <- function(list){
  rbindlist(list, idcol = "crop_no")
}