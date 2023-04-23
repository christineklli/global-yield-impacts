
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

# apply cv to all 5 candidate models

apply_cv_all_models <- function(data){
  
  
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
                 s(Reference_fact, bs='re') + 
                 s(Temp.Change, Reference_fact, bs = 're') + 
                 s(Precipitation.change, Reference_fact, bs = 're') +
                 s(f_CO2, Reference_fact, bs = 're') +
                 s(Country2_fact, bs = 're')",
          
          # GAMM RI
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
                 s(Reference_fact, bs='re') + 
                 s(Country2_fact, bs = 're')",
          
          # GLMM RS
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
          
          # GLMM RI
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
                 s(Country2_fact, bs = 're')",
          
          # LM
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

plot_cv <- function(data, path){
  
  # summarise mean RMSE across imputations for each model
  dat <- data %>% 
    group_by(crop_no, `Model ID`, Dependent, Fixed) %>% 
    summarise(mean_RMSE=mean(RMSE),
              mean_MAE=mean(MAE)) %>% 
    mutate(`Model ID`=as.factor(`Model ID`)) %>% 
    mutate(crop_no=as.factor(crop_no))
  
  plot <- ggplot(data=dat) +
    geom_point(
      aes(x=crop_no, y=mean_RMSE,
          shape=`Model ID`, col=`Model ID`), size = 4, alpha=0.5) +
    scale_shape_discrete(
      name="Model ID",
      labels=c("GAM RS", "GAM RI", "GLM RS", "GLM RI", "LM"),
      breaks=c("1","2","3","4","5")
    ) +
    scale_colour_discrete(
      name="Model ID",
      labels=c("GAM RS", "GAM RI", "GLM RS", "GLM RI", "LM"),
      breaks=c("1","2","3","4","5")
    ) +
    labs(y="Mean Model RMSE from k-fold CV") +
    scale_x_discrete(name="Crop",
                     labels=c("Maize","Rice","Soy","Wheat"),
                     breaks=c("1","2","3","4")) 
  
  ggplot2::ggsave(filename=path,
                  plot=plot,
                  width=7, height=6)
  
  plot
  
}


plot_all_m_cv <- function(data, path){
  
  dat <- data %>% 
    mutate(`Model ID`=as.factor(`Model ID`)) %>% 
    mutate(crop_no=as.factor(crop_no))
  
  mean_rmse <- data %>% 
    group_by(crop_no, `Model ID`, Dependent, Fixed) %>% 
    summarise(mean_RMSE=mean(RMSE),
              mean_MAE=mean(MAE))  %>% 
    mutate(`Model ID`=as.factor(`Model ID`)) %>% 
    mutate(crop_no=as.factor(crop_no))
  
  
  dat <- dat %>% left_join(mean_rmse, by=c("crop_no", "Model ID"))
  
  crop.labs <- c("Maize","Rice","Soy","Wheat")
  names(crop.labs) <- c(1:4)
  
  plot <- ggplot(data=dat) +
    geom_point(
      aes(x=`Model ID`, y=RMSE,
           col=`Model ID`), size = 2, alpha=0.5) +
  # add mean RMSE
    geom_point(
      aes(x=`Model ID`, y=mean_RMSE), 
      col="black", shape=21, size = 2, alpha=1) +
    # scale_shape_discrete(
    #   name="Model ID",
    #   labels=c("GAM RS", "GAM RI", "GLM RS", "GLM RI", "LM"),
    #   breaks=c("1","2","3","4","5")
    # ) +
    facet_wrap(~crop_no, ncol=4,
               labeller=labeller(crop_no=crop.labs)
               ) +
    scale_colour_discrete(
      name="Model ID",
      labels=c("GAM RS", "GAM RI", "GLM RS", "GLM RI", "LM")#,
      #breaks=c("1","2","3","4","5")
    ) +
    labs(y="Mean Model RMSE from k-fold CV") +
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank()
          ) + 
    # to combine with dev.explained.all plot later
    theme(legend.position="none")
    # scale_x_discrete(name="Model ID",
    #                  labels=c("GAM RS", "GAM RI", "GLM RS", "GLM RI", "LM"),
    #                  breaks=c("1","2","3","4","5")) 
  # add mean RMSE across m

  ggplot2::ggsave(filename=path,
                  plot=plot,
                  width=7, height=6)

  plot
  # 
}
