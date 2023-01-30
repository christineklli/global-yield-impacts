
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
  
  tar_target(block_bootstrap_fit_nested,
             fit_block_bootstrap_nested_first(
               block_bootstrap_samples=block_bootstrap_samples,
               model_specs=model_specs,
               ncores=5
             ),
             pattern=cross(model_specs, block_bootstrap_samples),
             iteration='list' ),
  
  tar_target(
    block_bootstrap_fit_only,
    
    {lapply(1:5, function(spec){
      t <- block_bootstrap_fit_nested[[spec]] %>% dplyr::select(!c(data, splits))
      
      df <- data.frame(model_specs=model_specs,
                       model_spec=c("gam_RS",
                                    "gam_RI",
                                    "glm_RS",
                                    "glm_RI",
                                    "lm"))
      t %>% left_join(df, by=c("model_specs")) %>%
        relocate(id, model_spec) %>%
        dplyr::select(!model_specs)
    })
    },
    pattern=map(block_bootstrap_fit_nested)
  ),
  
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
  
  tar_target(block_bootstrap_prediction_maize,
             predict_block_bootstrap_nested(
               fit=block_bootstrap_fit_only, 
               data_og=data_og_2081_maize_nested_gcm,
               data_adj=data_adj_2081_maize_nested_gcm,
               crop_production_df=crop_production_df[[1]]),
             pattern=cross(slice(block_bootstrap_fit_only,
                                 index=c(1,5,9,13,17)),
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
             predict_block_bootstrap_nested(
               fit=block_bootstrap_fit_only,
               data_og=data_og_2081_rice_nested_gcm,
               data_adj=data_adj_2081_rice_nested_gcm,
               crop_production_df=crop_production_df[[2]]),
             pattern=cross(slice(block_bootstrap_fit_only,
                                 index=c(2,6,10,14,18)),
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
             predict_block_bootstrap_nested(
               fit=block_bootstrap_fit_only,
               data_og=data_og_2081_soy_nested_gcm,
               data_adj=data_adj_2081_soy_nested_gcm,
               crop_production_df=crop_production_df[[3]]),
             pattern=cross(slice(block_bootstrap_fit_only,
                                 index=c(3,7,11,15,19)),
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
             predict_block_bootstrap_nested(
               fit=block_bootstrap_fit_only,
               data_og=data_og_2081_wheat_nested_gcm,
               data_adj=data_adj_2081_wheat_nested_gcm,
               crop_production_df=crop_production_df[[4]]),
             pattern=cross(slice(block_bootstrap_fit_only,
                                 index=c(4,8,12,16,20)),
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
                                              uncertainty==2 ~ "model",
                                              uncertainty==3 ~ "gcm",
                                              uncertainty==4 ~ "missing")
                 )
             }),
  # prep data for plotting distributions
  tar_target(block_bootstrap_predictions_crop_list,
             { maize <- reshape2::melt(block_bootstrap_prediction_maize, id.vars=c("id", "model_spec", "crop", "gcm"))
             rice <- reshape2::melt(block_bootstrap_prediction_rice, id.vars=c("id", "model_spec", "crop", "gcm"))
             soy <- reshape2::melt(block_bootstrap_prediction_soy, id.vars=c("id", "model_spec", "crop", "gcm"))
             wheat <- reshape2::melt(block_bootstrap_prediction_wheat, id.vars=c("id", "model_spec", "crop", "gcm"))
             
             l <- list(
               maize, rice, soy, wheat
             )
             
             lapply(1:4, function(i){
               l[[i]] %>% 
                 pivot_wider(., 
                             id_cols=c("id", "model_spec", "crop", "gcm"),
                             names_from=variable,
                             values_from=value,
                             values_fn=list,
                             names_expand=TRUE) %>% 
                 unnest(c(imputation, weighted_mean))
             })    
             
             }
             
  ),
  # plot distributions by gcm 
  tar_target(
    plot_bootstrap_distributions_2081_gcm,
    plot_bootstrap_distributions_gcm(
      predictions=block_bootstrap_predictions_crop_list,
      path="results/figures/adjusted_bootstrap_distributions_2081_gcm.png"
    )
    
  ),
  # plot distributions folding in gcm
  tar_target(
    plot_bootstrap_distributions_2081,
    plot_bootstrap_distributions(
      predictions=block_bootstrap_predictions_crop_list,
      path="results/figures/adjusted_bootstrap_distributions_2081.png"
    )
    
  )
)