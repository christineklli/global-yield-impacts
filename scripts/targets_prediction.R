
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
  # tar_target(named_data_current, name_list(
  #   x=data_current, name=paste0("data_", c(1:20))
  # )
  # ),
  # 
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
  
  # # prepare prediction data into nested list so that we can map in sequence
  # tar_target(data_future_crop,
  #            
  #            {# 387 df x 60k rows
  #              y <- rbindlist(data_future)
  #              # split by crop - into list of 4 df of length 96x60k rows
  #              z <- split(y, y$crop)
  #              # split by time period x gcm combo
  #              # 1 list of 4 crop sublists of 96 elements (df) of length 60k each
  #              a <- lapply(1:4, function(i){
  #                split(z[[i]], 
  #                      interaction(
  #                        z[[i]]$time_period,
  #                        z[[i]]$gcm
  #                      ))
  #              })}
  # ),
  # 
  # # fit model over combinations
  # tar_target(fit,
  #            mgcv::gam(formula(str_replace_all(model_specs, "[\r\n]", "")),
  #                      method = 'REML',
  #                      family = 'gaussian',
  #                      data = data_current[[1]]), 
  #            pattern=cross(model_specs, data_current),
  #            iteration='list'),
  
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
  
  # prepare fit estimates list for dynamic branching, split into nested list of 
  # 4 crop sublists of length 25 elements (5 spec x 5 m) of fit estimates each
  # tar_target(fit_crop, {
  #   
  #   split(fit_attr, sapply(
  #     fit_attr, function(x) {attr(x, "crop")}
  #   ))
  #   
  # }
  # ),
  # 
  
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
  # 
  # 
  # # note that attempt to nest cross() inside map() did not work,
  # # # so just split fit_crop and data_future_crop into independent targets
  # # # so that we can map these in sequence / vectorised
  # # # this is less parsimonious, and could've been collapsed into one step/target with earlier targets
  # tar_target(fit_maize, fit_crop[[1]]),
  # # tar_target(data_future_maize, data_future_crop[[1]]),
  # tar_target(fit_rice, fit_crop[[2]]),
  # # tar_target(data_future_rice, data_future_crop[[2]]),
  # tar_target(fit_soy, fit_crop[[3]]),
  # # tar_target(data_future_soy, data_future_crop[[3]]),
  # tar_target(fit_wheat, fit_crop[[4]]),
  # # tar_target(data_future_wheat, data_future_crop[[4]]),
  # # 
  # tar_target(fit_maize_nested, {
  #   
  #   split(fit_maize, sapply(
  #     fit_maize, function(x) {attr(x, "model_specs")}
  #   ))
  #   }),
  
 
  # following targets have been heavily optimised for memory constraints and subsequent pooling operations 
  # if neither fit nor data future are unnested - would build 2300 branches with 60k rows each
  # if nesting fit only over imputation- should create 460 branches with list of 5 dfs each
  # if nesting over fit-imputation and data future-gcm - should create 20 branches with 7m rows = 1405484 (23gcm x 60k rows) x 5 (imputation) rows
  #^ we choose the above as if we nested only over imputation then would still need to rbindlist over entire crop predict target of 27m rows later and memory may not be sufficient 
  # each branch has unique (just one) time period and model_spec but all combinations of m and gcm
  
  
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
  
  # rbindlist to combine all model_spec and time_period combinations into a tbl -> 20 branches x 60k = 1.2m rows
  # when all level 3 crop predictions are done, could rbindlist these as well into 4 crops x 1.2m = tbl of 6m rows 
  
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
             format='fst_tbl')
  
  
  
  # commence prediction for each crop in sequence
  # at a later point, migrate these functions out of targets pipeline
  # and into functions script
  # # once I am sure doing so won't recreate entire target from scratch
  # tar_target(
  #   predictions_level_5_maize,
  #   {p <- gammit::predict_gamm(fit_maize[[1]],
  #                              data_future_maize[[1]], 
  #                              re_form = c("s(Country2_fact)"),
  #                              keep_prediction_data = TRUE,
  #                              newdata.guaranteed = TRUE,
  #                              se.fit = TRUE)
  #   
  #   p %>% mutate(model_spec=attr(fit_maize[[1]], "model_specs"),
  #                m=attr(fit_maize[[1]], "imputation"))
  #   }
  #   ,
  #   # should create 2400 combinations - should i be doing this step differently?
  #   pattern=cross(fit_maize, 
  #                 data_future_maize),
  #   iteration='list',
  #   format='fst_tbl'),
  # tar_target(
  #   predictions_level_5_rice,
  #   {p <- gammit::predict_gamm(fit_rice[[1]],
  #                              data_future_rice[[1]], 
  #                              re_form = c("s(Country2_fact)"),
  #                              keep_prediction_data = TRUE,
  #                              newdata.guaranteed = TRUE,
  #                              se.fit = TRUE)
  #   
  #   p %>% mutate(model_spec=attr(fit_rice[[1]], "model_specs"),
  #                m=attr(fit_rice[[1]], "imputation"))
  #   }
  #   ,
  #   # should create 2400 combinations
  #   pattern=cross(fit_rice,
  #                 data_future_rice),
  #   iteration='list',
  #   format='fst_tbl'),
  # tar_target(
  #   predictions_level_5_soy,
  #   {p <- gammit::predict_gamm(fit_soy[[1]],
  #                              data_future_soy[[1]], 
  #                              re_form = c("s(Country2_fact)"),
  #                              keep_prediction_data = TRUE,
  #                              newdata.guaranteed = TRUE,
  #                              se.fit = TRUE)
  #   
  #   p %>% mutate(model_spec=attr(fit_soy[[1]], "model_specs"),
  #                m=attr(fit_soy[[1]], "imputation"))
  #   }
  #   ,
  #   # should create 2400 combinations
  #   pattern=cross(fit_soy,
  #                 data_future_soy),
  #   iteration='list',
  #   format='fst_tbl'),
  # tar_target(
  #   predictions_level_5_wheat,
  #   {p <- gammit::predict_gamm(fit_wheat[[1]],
  #                              data_future_wheat[[1]], 
  #                              re_form = c("s(Country2_fact)"),
  #                              keep_prediction_data = TRUE,
  #                              newdata.guaranteed = TRUE,
  #                              se.fit = TRUE)
  #   
  #   p %>% mutate(model_spec=attr(fit_wheat[[1]], "model_specs"),
  #                m=attr(fit_wheat[[1]], "imputation"))
  #   }
  #   ,
  #   # should create 2400 combinations
  #   pattern=cross(fit_wheat,
  #                 data_future_wheat),
  #   iteration='list',
  #   format='fst_tbl'),
  # 
  # tar_target(prediction_level_4_wheat,
  #            pool_across_m(predictions_level_5_wheat),
  #            pattern=cross(time_period, gcm)
  #            ),
  # # 
  
  # predictions_level_5_wheat_26fa7b65 # m5, 2061-2080, UKESM1-0-LL, LM
  # predictions_level_5_wheat_5bfb5083 # m5, 2081-2100, UKESM1-0-LL, LM
  # predictions_level_5_wheat_34db9d4a # m5, 2041-2060, UKESM1-0-LL, LM
  # these are not really ordered so can't use slice??
  
  
  # tar_target(
  #   data,
  #   data.frame(
  #     x = seq_len(6),
  #     id = rep(letters[seq_len(3)], each = 2)
  #   ) %>%
  #     group_by(id) %>%
  #     tar_group(),
  #   iteration = "group"
  # ),
  # tar_target(
  #   sums,
  #   sum(data$x),
  #   pattern = map(data),
  #   iteration = "vector"
  # ),
  # tar_read(predictions_level_5_maize_group_ae02b4c1)$tar_group
  # pool across m for ensemble estimates across imputed predictions
  # do this via dynamic branching
  # function keeps crop, model_spec, time_period, gcm columns to identify each element/branch 
  # remember to run this using tar_make_future() for parallelisation
  
  # tarchetypes::tar_group_by(grouplvl5maize, rbind_predictions(predictions_level_5_maize), 
  #                           lon, lat, crop, model_spec, time_period, gcm),
  # tar_target(predictions_level_5_maize_group,
  #            grouplvl5maize,
  #            pattern=map(grouplvl5maize), 
  # #            format='fst_tbl'),
  # 
  # # should i have tried to set explicit crossing parameters earlier? but not sure how?
  # # now this bypasses the need to group first, instead group and summarising in the same function
  # # may take a while - cannot allocate memory
  # tar_target(predictions_level_4_maize_again,
  #                       pool_across_m(predictions_level_5_maize),
  #                       format='fst_tbl'),  
  # 
  # 
  # tar_target(predictions_level_3_maize_again,
  #            pool_across_gcm(predictions_level_4_maize_again),
  #            format='fst_tbl'),
  # 
  # 
  # this 'only' took about 20 minutes but now having trouble reading it into the next target           
  # tar_target(predictions_level_5_maize_group,
  # #            
  # predictions_level_5_maize %>%
  # rbindlist(.)  %>%
  #  group_by(lon, lat, crop, model_spec, time_period, gcm) %>%
  #  tar_group(),
  #  iteration="group",
  # format='fst_tbl')
  
  # worked the second time by running tar_make instead of tar_make_future() 
  # , 140,548,400 rows x 21 columns
  # v. memory intensive
  # length(unique(tar_read(predictions_level_5_maize_group))) # 28109680 = 140548500 / 5
  # this means that group has worked correctly
  # and then we should be able to pool over the correct groups
  
  # this might be silent crash due to memory; predictions_level_5_maize_group is over 9GB
  # if this doesn't work, then can do slice(but need to work out indices and they must be in order)
  # also would need to do this iteratively still? lapply over pattern=slice()?
  # and rbindlist inside pool_across_m
  
  
  # 
  # 
  # tar_target(prediction_level_4_maize,
  #            
  #            group_pool_across_m(predictions_level_5_maize_group), # redundant grouping?
  #            pattern=map(predictions_level_5_maize_group), # or #?
  #            # ^ as long as I am mapping over this whole list, it seems that it
  #            # would be reundant for m's?
  #            # I have to rbindlist otherwise there are no duplicate m's
  #            iteration="list",
  #            format='fst_tbl'),
  # 
  # tar_target(prediction_level_4_maize_group,
  #            
  #            prediction_level_4_maize %>% 
  #            rbindlist(.)  %>%
  #              dplyr::select(!c(v_w, v_b, v_p, lwr_p, upr_p)) %>% 
  #              group_by(lon, lat, crop, model_spec, time_period) %>%
  #              tar_group(),
  #            iteration="group",
  #            format='fst_tbl'),
  # 
  # 
  # # need to then re-rbindlist prediction_level_4_maize to group it?
  # # and deselect  dplyr::select(!c(v_w, v_b, v_p, lwr_p, upr_p)) %>%
  # # then pass to group_pool_across_gcm
  # 
  # tar_target(prediction_level_3_maize, 
  #            group_pool_across_gcm( # pros and cons to running this in the same function
  #              # cannot see results separately for each gcm
  #              # might be faster to do it together
  #              # but also might be faster to use tar_group() for each gcm and dynamically branch that?
  #              predictions=prediction_level_4_maize_group,
  #              coords_countries=coords_countries
  #            ),
  #            pattern=map(prediction_level_4_maize_group), 
  #            iteration='list',
  #            format='fst_tbl')
  # # 
  # tar_target(predictions_level_4_maize,
  #            pool_across_m(
  #              predictions=predictions_level_5_maize
  #            ),
  #            pattern=map(predictions_level_5_maize), 
  #            # ^ can this be used to dynamically branch over variables in predictions_level_5_maize? 
  #            iteration='list',
  #            format='fst_tbl'),
  # tar_target(predictions_level_4_rice,
  #            pool_across_m(
  #              predictions=predictions_level_5_rice
  #            ),
  #            pattern=map(predictions_level_5_rice), 
  #            # ^ can this be used to dynamically branch over variables in predictions_level_5_maize? 
  #            iteration='list',
  #            format='fst_tbl'),
  # tar_target(predictions_level_4_soy,
  #            pool_across_m(
  #              predictions=predictions_level_5_soy
  #            ),
  #            pattern=map(predictions_level_5_soy), 
  #            # ^ can this be used to dynamically branch over variables in predictions_level_5_maize? 
  #            iteration='list',
  #            format='fst_tbl'),
  # tar_target(predictions_level_4_wheat,
  #            pool_across_m(
  #              predictions=predictions_level_5_wheat
  #            ),
  #            pattern=map(predictions_level_5_wheat), 
  #            # ^ can this be used to dynamically branch over variables in predictions_level_5_maize? 
  #            iteration='list',
  #            format='fst_tbl'),
  # 
  # # pool across GCM, but only the 23 GCMs!!
  # # not sure how to drop that BCC GCM here so have done it upstream
  # # in cross_vars_future()
  # # this shouldn't have taken so long as there shouldn't be that many
  # # instead it is calculating this for every single predictions_level_5_wheat
  # # there is some repetition here
  # # should I have done tar_group_by()?
  # tar_target(predictions_level_3_maize,
  #            pool_across_gcm(
  #              predictions=predictions_level_4_maize,
  #              coords_countries=coords_countries),
  #            pattern=map(predictions_level_4_maize),
  #            iteration='list',
  #            format='fst_tbl'
  # ),
  # tar_target(predictions_level_3_rice,
  #            pool_across_gcm(
  #              predictions=predictions_level_4_rice,
  #              coords_countries=coords_countries),
  #            pattern=map(predictions_level_4_rice),
  #            iteration='list',
  #            format='fst_tbl'
  # ),
  # tar_target(predictions_level_3_soy,
  #            pool_across_gcm(
  #              predictions=predictions_level_4_soy,
  #              coords_countries=coords_countries),
  #            pattern=map(predictions_level_4_soy),
  #            iteration='list',
  #            format='fst_tbl'
  # ),
  # tar_target(predictions_level_3_wheat,
  #            pool_across_gcm(
  #              predictions=predictions_level_4_wheat,
  #              coords_countries=coords_countries),
  #            pattern=map(predictions_level_4_wheat),
  #            iteration='list',
  #            format='fst_tbl'
  # )
  
)






# tar_target(
#   predictions_gridded_level_5,
#   gammit::predict_gamm(fit, 
#                        data_future[[1]], # cached data; if reading in from file, prediction_data
#                        re_form = c("s(Country2_fact)"), 
#                        keep_prediction_data = TRUE,
#                        newdata.guaranteed = TRUE,
#                        se.fit = TRUE),
#   # note there will be some redundance as there are 100 fits
#   # 100 = 5 specs x 4 crops x 5 m
#   # 384 data future sets = 4 crops x 4 time periods x 24 GCMs
#   # so rather than 100 * 384 there should only be 100 * 384 / 4  = 100 * 96 = 9,600
#   # ie we should map inside cross?
#   pattern=cross(fit, data_future),
#   # ^ map over fit and data_future together, predicting each model
#   # (element/branch of fit) to the corresponding future dataset
#   # (element/branch of data_future). This is why it's important that the order
#   # of variables in the `cross` for data_future and data_current is
#   # consistent.
#   iteration='list',
#   format='fst_tbl'
# ),

# # try john's code
# tar_target(
#   data_currentx,
#   # This is where you'd construct your model-fitting (current climate, current
#   # time) dataset for each combination of crop x spec x imputation method.
#   # Here I just create some random x and y, and add columns for crop, spec,
#   # and imputation to keep track of what's what (instead of adding columns,
#   # since they're constants for each combination.. you could add crop, spec,
#   # and impute method as attributes, but then you can't store as fst (would
#   # need to use the default 'rds' or 'qs' format).
#   tibble(Temp.Change=runif(100), Yield.Change=runif(100), x=runif(100), y=runif(100), crop=crops,
#          imputation=imputations),
#   pattern=cross(crops, imputations),
#   iteration='list',
#   format='fst_tbl'
# ),
# # try to create data current from current crop_imputed_rst_data using dynamic branching
# 
# tar_target(
#   data_futurex,
#   # This is where you construct the future conditions to which you want to
#   # predict the models. I think the best way to do this, to accommodate
#   # dynamic branching, is to have long format tables including all time
#   # periods x GCMs. We again have one table (and so one branch) for each
#   # combination of crop x spec x imputation.
#   replicate(
#     24*4,
#     tibble(Temp.Change=runif(10), Yield.Change=runif(10), x=runif(10), y=runif(10), crop=crops, imputation=imputations),
#     simplify=FALSE
#   ) %>% bind_rows() %>%
#     mutate(time_period=rep(time_periods, each=240),
#            gcm=rep(GCMs, each=40)),
#   # ^ add time period and gcm as columns so that we know what we're predicting
#   # to.
#   pattern=cross(crops, imputations),
#   # ^ important that the order of variables here matches the order for
#   # data_current
#   iteration='list',
#   format='fst_tbl'
# ),
# tar_target(model_specsx, c('Yield.Change ~ Temp.Change', 'Yield.Change ~ Temp.Change + 1')),
# tar_target(fitx,
#            lm(formula(model_specsx), data=named_data_current[[1]]), # doesn't work for data_current
#            pattern=cross(model_specsx, named_data_current),
#            iteration='list'),
# tar_target(
#   predictx,
#   mutate(data_futurex, pred=predict(fitx, data_futurex)),
#   pattern=cross(fitx, data_futurex),
#   # ^ map over fit and data_future together, predicting each model
#   # (element/branch of fit) to the corresponding future dataset
#   # (element/branch of data_future). This is why it's important that the order
#   # of variables in the `cross` for data_future and data_current is
#   # consistent.
#   iteration='vector',
#   format='fst_tbl'
# )
# )



# tar_target(
#   data_current, 
#   # This is where you'd construct your model-fitting (current climate, current
#   # time) dataset for each combination of crop x spec x imputation method. 
#   # Here I just create some random x and y, and add columns for crop, spec, 
#   # and imputation to keep track of what's what (instead of adding columns, 
#   # since they're constants for each combination.. you could add crop, spec,
#   # and impute method as attributes, but then you can't store as fst (would
#   # need to use the default 'rds' or 'qs' format). 
#   tibble(x=runif(100), y=runif(100), crop=crops, 
#          imputation=imputations),
#   pattern=cross(crops, imputations), 
#   iteration='list',
#   format='fst_tbl'
# )
# tar_target(
#   data_future,
#   # This is where you construct the future conditions to which you want to 
#   # predict the models. I think the best way to do this, to accommodate
#   # dynamic branching, is to have long format tables including all time 
#   # periods x GCMs. We again have one table (and so one branch) for each
#   # combination of crop x spec x imputation. 
#   replicate(
#     24*4, 
#     tibble(x=runif(10), y=runif(10), crop=crops, 
#            imputation=imputations),
#     simplify=FALSE
#   ) %>% bind_rows() %>% 
#     mutate(time_period=rep(time_periods, each=240),
#            gcm=rep(GCMs, each=40)), 
#   # ^ add time period and gcm as columns so that we know what we're predicting
#   # to.
#   pattern=cross(crops, imputations), 
#   # ^ important that the order of variables here matches the order for
#   # data_current
#   iteration='list',
#   format='fst_tbl'
# )



# 
# # run prediction over all models using purrr::map at level 5, store to local
# # pool across m - loop using purrr::map/branching for level 4, store to local
# # pool across GCMs - loop using purrr::map/branching for level 3, store to local
# # add coords
# 
# 
# targets_prediction <- list(
#   # read in prediction data as nested list of file paths
#   tar_target(prediction_data_nested_list, 
#              read_in_nested_data(path="processed/prediction_data")),
#   # predict
#   tar_target(prediction_gridded_level_5, 
#              {predict_gridded_level_5(
#                # this is where pmap/map2 might come in handy as iterating over dirs and models in parallel
#                fit = models, # 5 specs x 4 crops x 5 m
#                newdata = prediction_data_complete_cases, #prediction_data_nested_list,
#                model = c(1:5), 
#                crops = c("Maize", "Rice", "Soy", "Wheat"),
#                time_period = time_periods,
#                GCM = GCMs,
#                imputed = c(1:5),
#                ncores = 2)
#                # })
#                # Return list of files
#                list.files(path = "processed/predictions/level5", 
#                           full.names = TRUE,
#                           recursive = TRUE,
#                           pattern = ".RData")
#                
#              },
#              format ="file"),
#   
#   # pool over m=5 - how to ensure dependency from previous predictions??
#   # through changes in the files?
#   tar_target(prediction_gridded_level_4,
#              {pool_across_m(
#                data = prediction_gridded_level_5,
#                model = c(1:5), 
#                crops = c("Maize", "Rice", "Soy", "Wheat"), 
#                time_period = time_periods, 
#                GCM = GCMs, 
#                imputed = c(1:5),
#                ncores = 2
#              )
#                # Return list of files
#                list.files(path = "processed/predictions/level4", 
#                           full.names = TRUE,
#                           recursive = TRUE,
#                           pattern = ".RData")
#                
#              },
#              format ="file")
# )

# OLD CODE
# run prediction at the 5 level index
#tar_target(prediction_gridded_level_5_model1,
#           predict_gridded_level_5(
#             models=models,
#             data=prediction_data_complete_cases,
#             spec_no=1
#             # EDIT THIS -> IDEALLY run all 5 model specs in one chunk??
#           )),

# there are memory constraints here so have to do separately
# MODEL 1
# pool across m for maize
#tar_target(prediction_gridded_level_4_maize_model1,
#           pool_across_m(
#             predictions=prediction_gridded_level_5_model1,
#             crop_no=1
#           )),

# pool across 23 GCMs
# UPDATE TO INCLUDE k MODEL SPECS!!!
#tar_target(predictions_gridded_level_3,
#           pool_gcm_predictions(
#             predictions=predictions_gridded_level_4,
#             spec_no=c(1:5)
#           )),
# join full coordinates to predictions
#tar_target(predictions_gridded,
#           add_coords_to_predictions(
#             predictions=predictions_gridded_level_3,
#             coords_countries=coords_countries
#           ))




#tar_read(predictions_gridded) %>% saveRDS(here("processed", "predictions_gridded.RData"))
# tar_read(global_mean_predictions_gam) %>% saveRDS(here("processed", "global_mean_predictions_gam.RData"))
# tar_read(global_mean_predictions_lm) %>% saveRDS(here("processed", "global_mean_predictions_lm.RData"))