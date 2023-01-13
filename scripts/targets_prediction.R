
tar_option_set(packages = packages,
               memory = "transient", # activate transient memory
               garbage_collection = TRUE, # activate garbage collection
               format = "qs" # efficient storage format, need to first install qs
) 

# dynamic branching targets
future::plan(future::multisession, workers = 5)

targets_prediction <- list(
  tar_target(crops, c("Maize", "Rice", "Soy", "Wheat")),
  tar_target(model_specs, 
             c(
               #GAMM RS
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
                 Temp.Change:adapt_dummy")),
  
  tar_target(imputations, paste0('m_', 1:5)),
  
  # transform data used for fitting model into tbls for dynamic branching
  tar_target(
    data_nested_list,
    cross_vars(
      data=crop_imputed_rst_data,
      crops=crops,
      imputations=imputations
    )
  ),
  tar_target(
    data_shallow_list,
    flattenlist(data_nested_list)
  ),
  tar_target(
    data_current,
    {lapply(1:20, function(x) {
      as_tibble(data_shallow_list[[x]]) 
    })
    }),
  
  # transform data for future prediction into tbls for dynamic branching
  tar_target(
    data_future_nested_list, # THIS HAS CHANGED BY DROPPING ONE GCM
    cross_vars_future(
      data=prediction_data_complete_cases,
      crops=crops,
      time_periods=time_periods,
      GCMs=GCMs
    )),
  tar_target(
    data_future,
    flattenlist(data_future_nested_list)
  ),
  # # prepare prediction data into nested list so that we can map in sequence
  tar_target(data_future_crop_nested, 
             
             {# 387 df x 60k rows
               y <- rbindlist(data_future)
               y <- y %>% dplyr::select(!crop) # duplicated column, use crop_pooled instead
               # split by crop - into list of 4 df of length 92x60k rows
               z <- split(y, y$crop_pooled)
               # split by time period x gcm combo
               # 1 list of 4 crop sublists of 4 time period sublists of 1405484 (60k x 23 gcm)
               a <- lapply(1:4, function(i){
                 split(z[[i]], 
                       z[[i]]$time_period
                 )      })
               
               
             }),
  
  # split out data future sublists by crop to prepare for mapping in sequence with crop-specific fit lists 
  tar_target(data_future_maize_nested, data_future_crop_nested[[1]]),
  tar_target(data_future_rice_nested, data_future_crop_nested[[2]]),
  tar_target(data_future_soy_nested, data_future_crop_nested[[3]]),
  tar_target(data_future_wheat_nested, data_future_crop_nested[[4]]),
  
  # append attributes so that we can identify model fit estimate dynamic branches
  tar_target(fit_attr,
             {
               f <- mgcv::gam(formula(str_replace_all(model_specs, "[\r\n]", "")),
                              method = 'REML',
                              family = 'gaussian',
                              data = data_current[[1]])
               
               attr(f, 'crop') <- data_current[[1]]$crop[1]
               attr(f, 'model_specs') <- model_specs[1] 
               attr(f, 'imputation') <- data_current[[1]]$imputation[1]
               #^ add attributes so we know what crop etc. states the model is using
               f
               
             }, 
             pattern=cross(model_specs, data_current),
             iteration='list'),
  
  # prepare fit estimates list for dynamic branching, split into three-level nested list of 
  # 4 crop sublists of length 5 sublists (model spec) of length 5 elements fit estimates (m) 
  tar_target(fit_crop_nested, {
    
    a <- split(fit_attr, sapply(
      fit_attr, function(x) {attr(x, "crop")}
    ))
    
    b <- lapply(1:4, function(i){
      split(a[[i]], sapply(
        a[[i]], function(x) {attr(x, "model_specs")}
      ))
    })
    b
  }
  ),
  
  # split out fit sublists by crop to prepare for mapping in sequence with crop-specific data future lists 
  tar_target(fit_maize_nested, 
             fit_crop_nested[[1]]),
  tar_target(fit_rice_nested, 
             fit_crop_nested[[2]]),
  tar_target(fit_soy_nested, 
             fit_crop_nested[[3]]),
  tar_target(fit_wheat_nested, 
             fit_crop_nested[[4]]),
  
  # following targets have been heavily optimised for memory constraints and subsequent pooling operations 
  # if neither fit nor data future are unnested - would build 2300 branches with 60k rows each
  # if nesting fit only over imputation- should create 460 branches with list of 5 dfs each
  # if nesting over fit-imputation and data future-gcm - should create 20 branches with 7m rows = 1405484 (23gcm x 60k rows) x 5 (imputation) rows
  #^ we choose the above as if we nested only over imputation then would still need to rbindlist over entire crop predict target of 27m rows later and memory may not be sufficient 
  # each branch has unique (just one) time period and model_spec but all combinations of m and gcm
  
  # 20 branches because fit_maize_nested is a list of 5 model specs (rbindlist predictions across 5 m fit)
  # and data_future_maize_nested is list of 4 time periods (with rbindlist of unique gcms)
  # so crossing both lists returns mega rbindlisted list of predictions with all combos
  
  
  ## MAIZE ##
  tar_target(prediction_level_5_maize, # 20 branches x 7m rows (5 imputation x 23 gcm x 60k rows)
             predict_level_5(
               fit=fit_maize_nested,
               data=data_future_maize_nested,
               ncores=3
             ),
             
             pattern=cross(fit_maize_nested, 
                           data_future_maize_nested),
             iteration='list',
             format='fst_tbl'
  ),
  
  tar_target(prediction_level_4_maize, # 20 branches x 1.4m rows (7m / 5 imputation)
             pool_across_m(
               predictions=prediction_level_5_maize
             ),
             pattern=map(prediction_level_5_maize), # iterates over each branch 
             format='fst_tbl'),
  
  tar_target(prediction_level_3_maize, # 20 branches x 60k rows (1.4m / 23 gcm)
             pool_across_gcm(
               predictions=prediction_level_4_maize,
               coords_countries=coords_countries
             ),
             pattern=map(prediction_level_4_maize),
             format='fst_tbl'),
  
  #prediction _level_3 should actually have combined all model_spec and time_period combinations into a tbl 
  # 20 branches x 60k = 1.2m rows
  
  ## RICE ##
  tar_target(prediction_level_5_rice, 
             predict_level_5(
               fit=fit_rice_nested,
               data=data_future_rice_nested,
               ncores=3
             ),
             pattern=cross(fit_rice_nested, 
                           data_future_rice_nested),
             iteration='list',
             format='fst_tbl'
  ),
  
  tar_target(prediction_level_4_rice, # 20 branches x 1.4m rows (7m / 5 imputation)
             pool_across_m(
               predictions=prediction_level_5_rice
             ),
             pattern=map(prediction_level_5_rice), # iterates over each branch 
             format='fst_tbl'),
  
  tar_target(prediction_level_3_rice, # 20 branches x 60k rows (1.4m / 23 gcm)
             pool_across_gcm(
               predictions=prediction_level_4_rice,
               coords_countries=coords_countries
             ),
             pattern=map(prediction_level_4_rice),
             format='fst_tbl'),
  
  ## SOY ##
  
  tar_target(prediction_level_5_soy, # 20 branches x 7m rows
             predict_level_5(
               fit=fit_soy_nested,
               data=data_future_soy_nested,
               ncores=3
             ),
             pattern=cross(fit_soy_nested, 
                           data_future_soy_nested),
             iteration='list',
             format='fst_tbl'
  ),
  
  tar_target(prediction_level_4_soy, # 20 branches x 1.4m rows (7m / 5 imputation)
             pool_across_m(
               predictions=prediction_level_5_soy
             ),
             pattern=map(prediction_level_5_soy), # iterates over each branch 
             format='fst_tbl'),
  
  tar_target(prediction_level_3_soy, # 20 branches x 60k rows (1.4m / 23 gcm)
             pool_across_gcm(
               predictions=prediction_level_4_soy,
               coords_countries=coords_countries
             ),
             pattern=map(prediction_level_4_soy),
             format='fst_tbl'),
  
  ## WHEAT ##
  tar_target(prediction_level_5_wheat, # move the below into a function to repeat over crops
             predict_level_5(
               fit=fit_wheat_nested,
               data=data_future_wheat_nested,
               ncores=3
             ),
             pattern=cross(fit_wheat_nested, 
                           data_future_wheat_nested),
             iteration='list',
             format='fst_tbl'
  ),
  
  tar_target(prediction_level_4_wheat, # 20 branches x 1.4m rows (7m / 5 imputation)
             pool_across_m(
               predictions=prediction_level_5_wheat
             ),
             pattern=map(prediction_level_5_wheat), # iterates over each branch 
             format='fst_tbl'),
  
  tar_target(prediction_level_3_wheat, # 20 branches x 60k rows (1.4m / 23 gcm)
             pool_across_gcm(
               predictions=prediction_level_4_wheat,
               coords_countries=coords_countries
             ),
             pattern=map(prediction_level_4_wheat),
             format='fst_tbl'),
  
  # when all level 3 crop predictions are done, could rbindlist these as well into 4 crops x 1.2m = tbl of 6m rows 
  tar_target(predictions_by_time_period,
             {
               t <- rbind(prediction_level_3_maize,
                          prediction_level_3_rice,
                          prediction_level_3_soy,
                          prediction_level_3_wheat)
               
               # create concordance for model_spec and gam_RI / gam_RS / glm_RI / glm_RS / LM etc
               df <- data.frame(model_spec=model_specs,
                                model=c("gam_RS",
                                        "gam_RI",
                                        "glm_RS",
                                        "glm_RI",
                                        "lm"))
               t %>% left_join(df, by=c("model_spec")) %>% 
                 relocate(model) %>% 
                 relocate(model_spec, .after=last_col()) %>% 
                 # for outliers, arbitrarily cut off at -200 for now - should mostly be Siwa desert
                 # note we don't use pred_adj in any way later, so can comment out
                 mutate(pred_adj=ifelse(pred_bar < -100, NA, pred_bar),
                        se_p_adj  = ifelse(is.na(pred_adj),NA,se_p),
                        lwr_p_adj  = ifelse(is.na(pred_adj),NA,lwr_p),
                        upr_p_adj  = ifelse(is.na(pred_adj),NA,upr_p))
               
             }
  ),
  # calculate global mean predictions
  # these predictions are not meaningful as they have not been adjusted for current crop spatial distribution
  # for example, soybean global mean yields are vastly different across gam RS and glm RS
  # these need to be recalculated with weightings (rasterise) and resaved to results/tables/ 
  tar_target(global_mean_predictions,
             {t <- predictions_by_time_period %>% 
               filter(!is.infinite(pred_bar)) %>% # for lm results
               group_by(model, time_period, crop_pooled) %>% 
               summarise(mean_yield_Change = mean(pred_bar, na.rm=T),
                         mean_se_p         = mean(se_p, na.rm=T),
                         mean_lwr_p        = mean(lwr_p, na.rm=T),
                         mean_upr_p        = mean(upr_p, na.rm=T),
                         # due to outliers, adjusted means
                         # note we don't use pred_adj in any way later, so can comment out
                         mean_yield_change_adj = mean(pred_adj, na.rm=T),
                         mean_se_p_adj         = mean(se_p_adj, na.rm=T),
                         mean_lwr_p_adj        = mean(lwr_p_adj, na.rm=T),
                         mean_upr_p_adj        = mean(upr_p_adj, na.rm=T)
               ) 
             
             t %>% readr::write_csv("processed/global_mean_predictions.csv")
             
             t
             
             }
             
  ),
  
  # note that the order of models here is not the order defined in model_specs
  # instead, they are in alphabetical order
  # so: gam_RI, gam_RS, glm_RI, glm_RS, lm
  
  
  # split into two level nested sublists to prepare for plotting predictions
  # [[model_spec]][[crop_pooled]]
  tar_target(predictions_nested,
             {
               one_level <- split(predictions_by_time_period, predictions_by_time_period$model)
               # split by crop
               two_level <- lapply(1:5, function(i){
                 split(one_level[[i]], 
                       one_level[[i]]$crop_pooled
                 )      })
               two_level
               
             }
             
  ),
  
  # extract fit estimates into datatable by attr
  tar_target(fit_estimates_tbl,
             
             {
               t <- lapply(1:length(fit_attr), function(i){ # do this for all 25 models of fit_attr
                 
                 data.frame(
                   crop=attr(fit_attr[[i]],"crop"),
                   imputation=attr(fit_attr[[i]],"imputation"),
                   model_spec=attr(fit_attr[[i]],"model_specs"),
                   r.sq=summary(fit_attr[[i]])$r.sq,
                   dev.expl=summary(fit_attr[[i]])$dev.expl,
                   n=summary(fit_attr[[i]])$n
                 ) 
               })
               
               t <- rbindlist(t)
               
               df <- data.frame(model_spec=model_specs,
                                model=c("gam_RS",
                                        "gam_RI",
                                        "glm_RS",
                                        "glm_RI",
                                        "lm"))
               t %>% left_join(df, by=c("model_spec")) %>% 
                 relocate(model) %>% 
                 dplyr::select(!model_spec)
               
               
               
             }
  ),
  
  # plot dev.expl for temperature and precipitation
  tar_target(dev_explained_plot,
             {
               dat <- fit_estimates_tbl %>% 
                 group_by(crop, model) %>% 
                 summarise(mean_r.sq=mean(r.sq),
                           mean_dev.expl=mean(dev.expl)) 
               
               plot <- ggplot(data=dat) +
                 geom_point(
                   aes(x=crop, y=mean_dev.expl,
                       shape=model, col=model), size = 4, alpha=0.7) +
                 # I want these to match with cv_plot in targets_cross_validation.R
                 # so that we can plot these on same plot
                 scale_shape_manual(
                   name="Model ID",
                   labels=c("GAM RS", "GAM RI", "GLM RS", "GLM RI", "LM"),
                   breaks=c("gam_RS","gam_RI","glm_RS","glm_RI","lm"),
                   values=c(16,17,15,3,7)
                 ) +
                 scale_colour_manual(
                   name="Model ID",
                   labels=c("GAM RS", "GAM RI", "GLM RS", "GLM RI", "LM"),
                   breaks=c("gam_RS","gam_RI","glm_RS","glm_RI","lm"),
                   values=c("darksalmon","darkkhaki","aquamarine3","deepskyblue","darkorchid1")
                 ) +
                 labs(x="Crop", y="Mean Deviance Explained") 
               
               ggplot2::ggsave(filename="results/figures/dev_explained_plot.png",
                               plot=plot,
                               width=7, height=6)
               
               plot
             }
  ),
  
  # combine cv_plot and dev_explained_plot
  tar_target(cv_devexp_plot,
             {library(patchwork)
               
               # remove duplicate legend as model colour + shape scale values now match
               
               dat <- model_cv_1to5_dt %>% 
                 group_by(crop_no, `Model ID`, Dependent, Fixed) %>% 
                 summarise(mean_RMSE=mean(RMSE),
                           mean_MAE=mean(MAE)) %>% 
                 mutate(`Model ID`=as.factor(`Model ID`)) %>% 
                 mutate(crop_no=as.factor(crop_no))
               
               cv_plot <- ggplot(data=dat) +
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
                                  breaks=c("1","2","3","4")) + 
                 theme(legend.position="none") # REMOVE HERE
               
               plot <-  cv_plot + dev_explained_plot
               ggplot2::ggsave(filename="results/figures/combined_cv_devexp_plot.png",
                               plot=plot,
                               width=7, height=6)
               
               plot
             }
  )
)




