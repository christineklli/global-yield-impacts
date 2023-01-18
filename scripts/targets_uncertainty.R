
tar_option_set(packages = packages,
               memory = "transient", # activate transient memory
               garbage_collection = TRUE, # activate garbage collection
               format = "qs" # efficient storage format, need to first install qs
) 

future::plan(future::multisession, workers = 5)

targets_uncertainty <- list(
  # create df of lon, lat, production (note that each production column is named differently)
  tar_target(crop_production_df,
             create_crop_production_df(
               raster=crop_production_raster_agg)),
  # calculate global weighted mean predictions using crop production volumes as weights
  tar_target(adj_global_prediction_level_5_maize,
             calc_global_weighted_mean_prediction(
               prediction=adj_prediction_level_5_maize,
               crop_production_df=crop_production_df,
               crop_no=1,
               model_specs=model_specs
             ),
             pattern=map(adj_prediction_level_5_maize), # do this iteratively
             format="fst_tbl" # rbindlist instead of iteration="list"
  ),
  tar_target(adj_global_prediction_level_5_rice,
             calc_global_weighted_mean_prediction(
               prediction=adj_prediction_level_5_rice,
               crop_production_df=crop_production_df,
               crop_no=2,
               model_specs=model_specs
             ),
             pattern=map(adj_prediction_level_5_rice), 
             format="fst_tbl" 
  ),
  tar_target(adj_global_prediction_level_5_soy,
             calc_global_weighted_mean_prediction(
               prediction=adj_prediction_level_5_soy,
               crop_production_df=crop_production_df,
               crop_no=3,
               model_specs=model_specs
             ),
             pattern=map(adj_prediction_level_5_soy), 
             format="fst_tbl" 
  ),
  tar_target(adj_global_prediction_level_5_wheat,
             calc_global_weighted_mean_prediction(
               prediction=adj_prediction_level_5_wheat,
               crop_production_df=crop_production_df,
               crop_no=4,
               model_specs=model_specs
             ),
             pattern=map(adj_prediction_level_5_wheat), 
             format="fst_tbl" 
  ),
  # list all of them
  tar_target(adj_global_prediction_level_5_all_crops,
             list(
               adj_global_prediction_level_5_maize,
               adj_global_prediction_level_5_rice,
               adj_global_prediction_level_5_soy,
               adj_global_prediction_level_5_wheat
             )),
  # plot density distributions by model spec (model uncertainty) & imputation (missing data uncertainty)
  # distributions of each model spec x m pooled across 23 GCMs
  # i.e. shows prediction data model uncertainty
  tar_target(
    model_m_distribution_plot,
    plot_model_m_distributions(
      predictions=adj_global_prediction_level_5_all_crops,
      time_periods=time_periods,
      crops=crops,
      path="results/figures/adjusted_model_m_distributions_%s.png"
    )
  ),
  # distributions of each model spec pooled across 5 m x 23 GCMs
  # ie shows both missing data & prediction data model uncertainty
  tar_target(
    model_pooled_m_distribution_plot,
    plot_model_pooled_m_distributions(
      predictions=adj_global_prediction_level_5_all_crops,
      time_periods=time_periods,
      crops=crops,
      path="results/figures/adjusted_model_pooled_m_distributions_%s.png"
    )
  ),
  # sampling uncertainty - block bootstrapping
  tar_target(
    block_bootstrap_samples,
    create_block_bootstrap_samples(
      data=crop_imputed_rst_data,
      ncores=3
    ) 
  ), 
  # unnest so that it is list of 20 models not 4
  tar_target(
    block_bootstrap_samples_unnested,
    {flattenlist(block_bootstrap_samples)}
  ), 
  tar_target(
    block_bootstrap_fit, # for some reason model nested column doesn't appear??
    fit_block_bootstrap(
      block_bootstrap_samples_unnested=block_bootstrap_samples_unnested,
      model_specs=model_specs,
      ncores=5
    ),
    pattern=cross(model_specs, block_bootstrap_samples_unnested),
    iteration='list'),
  # only keep fit estimates and identifiers for model spec, crop and m
  # trying this so that we don't have to renest fit estimates later on
  # tar_target(block_bootstrap_fit_nested,
  #            fit_block_bootstrap_nested(
  #              block_bootstrap_samples=block_bootstrap_samples,
  #              model_specs=model_specs,
  #              ncores=5
  #            ),
  #            pattern=cross(model_specs, block_bootstrap_samples),
  #            iteration='list' ),
  tar_target(
    block_bootstrap_fit_only,
    {t <- block_bootstrap_fit %>% dplyr::select(!c(data, splits))
    
    df <- data.frame(model_specs=model_specs,
                     model_spec=c("gam_RS",
                                  "gam_RI",
                                  "glm_RS",
                                  "glm_RI",
                                  "lm"))
    t %>% left_join(df, by=c("model_specs")) %>% 
      relocate(id, model_spec) %>% 
      dplyr::select(!model_specs)  
    
    },
    pattern=map(block_bootstrap_fit)
  ),
  # can't do data_og and data_adj in the one function - too big
  # need to nest data_future_maize_nested by gcm
  tar_target(data_og_2081_maize_nested_gcm,
             {
               split(data_future_maize_nested[[4]], 
                     data_future_maize_nested[[4]]$gcm)
             }),
  tar_target(data_adj_2081_maize_nested_gcm,
             {
               split(int_adj_future_maize[[4]],
                     int_adj_future_maize[[4]]$gcm)
             }),
  # prediction
  
  ## NOTE THAT PREDICT_BLOCK_BOOTSTRAP IS BEING REPLICATED x4
  ## THIS IS BECAUSE I FORGOT TO SPECIFY THE CROP PRODUCTION DF LIST ELEMENT
  ## IN THE TARGET COMMAND AND IT IS HARDCODED INTO THE FUNCTION
  ## DUE TO THE TIME SPEND FOR EACH TARGET, I HAVE JUST REPLICATED THE FUNCTION x4
  ## NOT IDEAL, COME BACK AND EDIT THE PREDICT BLOCK BOOTSTRAP FN LATER
  ## WHEN I HAVE 3 DAYS TO SPEND
  
  tar_target(block_bootstrap_prediction_maize,
             predict_block_bootstrap(
               fit=block_bootstrap_fit_only,
               data_og=data_og_2081_maize_nested_gcm,
               data_adj=data_adj_2081_maize_nested_gcm,
               crop_production_df=crop_production_df),
             pattern=cross(slice(block_bootstrap_fit_only, index=c(1:25)),
                           map(
                             data_og_2081_maize_nested_gcm,
                             data_adj_2081_maize_nested_gcm
                           )),
             iteration='list'
  ),
  # rice
  tar_target(data_og_2081_rice_nested_gcm,
             {
               split(data_future_rice_nested[[4]], 
                     data_future_rice_nested[[4]]$gcm)
             }),
  tar_target(data_adj_2081_rice_nested_gcm,
             {
               split(int_adj_future_rice[[4]],
                     int_adj_future_rice[[4]]$gcm)
             }),
  tar_target(block_bootstrap_prediction_rice,
             predict_block_bootstrap_rice(
               fit=block_bootstrap_fit_only,
               data_og=data_og_2081_rice_nested_gcm,
               data_adj=data_adj_2081_rice_nested_gcm,
               crop_production_df=crop_production_df),
             pattern=cross(slice(block_bootstrap_fit_only, index=c(26:50)),
                           map(
                             data_og_2081_rice_nested_gcm,
                             data_adj_2081_rice_nested_gcm
                           )),
             iteration='list'
  ),
  # soy
  tar_target(data_og_2081_soy_nested_gcm,
             {
               split(data_future_soy_nested[[4]], 
                     data_future_soy_nested[[4]]$gcm)
             }),
  tar_target(data_adj_2081_soy_nested_gcm,
             {
               split(int_adj_future_soy[[4]],
                     int_adj_future_soy[[4]]$gcm)
             }),
  tar_target(block_bootstrap_prediction_soy,
             predict_block_bootstrap_soy(
               fit=block_bootstrap_fit_only,
               data_og=data_og_2081_soy_nested_gcm,
               data_adj=data_adj_2081_soy_nested_gcm,
               crop_production_df=crop_production_df),
             pattern=cross(slice(block_bootstrap_fit_only, index=c(51:75)),
                           map(
                             data_og_2081_soy_nested_gcm,
                             data_adj_2081_soy_nested_gcm
                           )),
             iteration='list'
  ),
  # wheat
  tar_target(data_og_2081_wheat_nested_gcm,
             {
               split(data_future_wheat_nested[[4]], 
                     data_future_wheat_nested[[4]]$gcm)
             }),
  tar_target(data_adj_2081_wheat_nested_gcm,
             {
               split(int_adj_future_wheat[[4]],
                     int_adj_future_wheat[[4]]$gcm)
             }),
  tar_target(block_bootstrap_prediction_wheat,
             predict_block_bootstrap_wheat(
               fit=block_bootstrap_fit_only,
               data_og=data_og_2081_wheat_nested_gcm,
               data_adj=data_adj_2081_wheat_nested_gcm,
               crop_production_df=crop_production_df),
             pattern=cross(slice(block_bootstrap_fit_only, index=c(76:100)),
                           map(
                             data_og_2081_wheat_nested_gcm,
                             data_adj_2081_wheat_nested_gcm
                           )),
             iteration='list'
  ),
  # rbind all predictions
  tar_target(block_bootstrap_predictions,
             rbind_block_bootstrap_predictions(
               block_bootstrap_prediction_maize,
               block_bootstrap_prediction_rice,
               block_bootstrap_prediction_soy,
               block_bootstrap_prediction_wheat
             )),
  # calculating sampling uncertainty
  tar_target(sampling_uncertainty,
             {block_bootstrap_predictions %>% 
                 filter(!is.infinite(weighted_mean)) %>% 
                 group_by(
                   crop,
                   model_spec,
                   imputation,
                   gcm
                 ) %>% 
                 summarise(var=var(weighted_mean, na.rm=T),
                           sd=sd(weighted_mean, na.rm=T)) %>% 
                 group_by(crop) %>% 
                 summarise(var=mean(var, na.rm=T),
                           sd=mean(sd, na.rm=T))
             }),
  # calculating modelling uncertainty
  tar_target(model_uncertainty,
             {block_bootstrap_predictions %>% 
                 filter(!is.infinite(weighted_mean)) %>% 
                 group_by(
                   crop,
                   id,
                   imputation,
                   gcm
                 ) %>% 
                 summarise(var=var(weighted_mean, na.rm=T),
                           sd=sd(weighted_mean, na.rm=T)) %>% 
                 group_by(crop) %>% 
                 summarise(var=mean(var, na.rm=T),
                           sd=mean(sd,na.rm=T))
             }),
  # calculating GCM uncertainty
  tar_target(gcm_uncertainty,
             {block_bootstrap_predictions %>% 
                 filter(!is.infinite(weighted_mean)) %>% 
                 group_by(
                   crop,
                   id,
                   imputation,
                   model_spec
                 ) %>% 
                 summarise(var=var(weighted_mean, na.rm=T),
                           sd=sd(weighted_mean, na.rm=T)) %>% 
                 group_by(crop) %>% 
                 summarise(var=mean(var, na.rm=T),
                           sd=mean(sd, na.rm=T))
             }),
  # calculating missing data m uncertainty
  tar_target(missing_data_uncertainty,
             {block_bootstrap_predictions %>% 
                 filter(!is.infinite(weighted_mean)) %>% 
                 group_by(
                   crop,
                   id,
                   model_spec,
                   gcm
                 ) %>% 
                 summarise(var=var(weighted_mean, na.rm=T),
                           sd=sd(weighted_mean, na.rm=T)) %>% 
                 group_by(crop) %>% 
                 summarise(var=mean(var, na.rm=T),
                           sd=mean(sd, na.rm=T))
             }),

  # calculate total global crop production per crop
  tar_target(global_crop_production,
             {
               l <- lapply(1:4, function(i){
                 crop_production_df[[i]] %>% 
                   summarise(Y=sum(production))
               }) 
               names(l) <- c("Maize","Rice","Soybean","Wheat")
               rbindlist(l, use.names=TRUE,idcol="crop") 
               
             }),
  # combine all sources of uncertainty into tibble
  tar_target(all_uncertainty,
             {bind_rows(
               list(
                 sampling_uncertainty,
                 model_uncertainty,
                 gcm_uncertainty,
                 missing_data_uncertainty
               ), .id="uncertainty") %>% 
                 group_by(crop) %>% 
                 mutate(combined_sd=sum(sd),
                        combined_var=sum(var),
                        uncertainty=case_when(uncertainty==1 ~ "sampling",
                                              uncertainty==2 ~ "modelling",
                                              uncertainty==3 ~ "gcm",
                                              uncertainty==4 ~ "missing")
                        )
             })
)
