
tar_option_set(packages = packages,
               memory = "transient", # activate transient memory
               garbage_collection = TRUE, # activate garbage collection
               format = "qs" # efficient storage format, need to first install qs
) 

future::plan(future::multisession, workers = 5)

# create prediction data using CRU for 1991-2020

# do this by extracting and calculating baseline season gs temperature and precipitation
targets_historical <- list(
  # create CRU temp prediction data 
  tar_target(tmp_1901_2020_df,
             create_cru_prediction_data(
               file="cru_ts4.05.1901.2020.tmp.dat.nc",
               var_name="tmp"
             )),
  # create CRU pre prediction data
  tar_target(pre_1901_2020_df,
             create_cru_prediction_data(
               file="cru_ts4.05.1901.2020.pre.dat.nc",
               var_name="pre"
             )),
  # extract baseline period tmp and pre for 1980-1995
  # calculate avg bs gs tmp and pre for 1980-1995 (levels)
  # extract prediction period tmp and pre for 2000-2015
  # calculate avg bs gs tmp and pre for 2000-2015
  tar_target(hs_years_tmp,
             calc_hsbs_years_vars(
               hs_years=hs_years,
               calc_bs_var=calc_bs_var,
               data=tmp_1901_2020_df,
               var="tmp",
               crop_season_extended_subset=crop_season_extended_subset 
             )),
  tar_target(hs_years_pre,
             calc_hsbs_years_vars(
               hs_years=hs_years,
               calc_bs_var=calc_bs_var,
               data=pre_1901_2020_df,
               var="pre",
               crop_season_extended_subset=crop_season_extended_subset
             )),
  # group by year, extract only years in periods 1980-1995, 2000-2015 (remove 1995-2000)
  # average over years of each period
  # calculate changes in avg bs gs tmp and pre from 1980-1995 to 2000-2015
  tar_target(hs_tmp_pre_avg,
             get_hs_avg_vars(
               data1=hs_years_tmp,
               data2=hs_years_pre
             )),
  # repeat for CO2 (use global CO2 observation)
  tar_target(co2_hs_file,
             "data/co2_annmean_mlo.csv",
             format='file'),
  tar_target(hs_co2,
             {readr::read_csv("data/co2_annmean_mlo.csv", skip=59)}
  ),
  # calculate change in CO2 across periods, then f_CO2
  tar_target(hs_co2_change_avg,
             calculate_hs_avg_co2(hs_co2)
  ),
  
  # merge w 2000 yields
  tar_target(hs_tmp_pre_yld_avg,
             merge_cru_monf_prediction_data(
               data1=hs_tmp_pre_avg,
               data2=crop_yield_monf_dt
             )),
  # merge w country names
  tar_target(hs_vars_avg,
             create_bs_2015_vars(
               data1=hs_tmp_pre_yld_avg,
               data2=coords_countries
             )),
  # join everything and filter for complete cases
  tar_target(hs_prediction_data_avg,
             create_hs_prediction_data(
               hs_vars=hs_vars_avg,
               hs_co2_change=hs_co2_change_avg
             )),
  # create prediction data where climate change vars = 0 for post-adj
  tar_target(hs_adj_prediction_data_avg,
             {lapply(1:4, function(crop){
               hs_prediction_data_avg[[crop]] %>% 
                 mutate(Temp.Change=0,
                        Precipitation.change=0,
                        f_CO2=0) %>% 
                 dplyr::filter(complete.cases(.))
             })}),
  tar_target(fit_crop_nested_shallow,
             {# need to have 2 level list, so that spec and m are mixed in same list
               a <- split(fit_attr, sapply(
                 fit_attr, function(x) {attr(x, "crop")}
               ))
               a # length 4 of 25 fits each
             }),
  tar_target(hs_og_predictions_avg,
             predict_level_5_avg_hs(
               fit=fit_crop_nested_shallow,
               data=hs_prediction_data_avg,
               ncores=5 
             ),
             pattern=map(fit_crop_nested_shallow,
                         hs_prediction_data_avg)),
  tar_target(hs_adj_predictions_avg,
             predict_level_5_avg_hs(
               fit=fit_crop_nested_shallow,
               data=hs_adj_prediction_data_avg,
               ncores=5 
             ),
             pattern=map(fit_crop_nested_shallow,
                         hs_adj_prediction_data_avg),           
             
  ),
  tar_target(adjusted_hs_predictions_avg,
             {hs_og_predictions_avg %>% 
                 left_join(
                   hs_adj_predictions_avg, by=c(
                     "lon","lat", "crop_pooled", "model_spec","m", 
                     "Baseline_tmp", "Baseline_pre", "Baseline_yield", "C3", "C4", "adapt_dummy", "Country2_fact")
                   # there will be duplicated variables for Temp.Change; Precipitation.change, f_CO2 as intended
                 ) %>% 
                 mutate(prediction.fit=prediction.fit.x-prediction.fit.y)},
             pattern=map(hs_og_predictions_avg,
                         hs_adj_predictions_avg)),
  # pool across m
  tar_target(adjusted_hs_predictions_avg_level_3,
             pool_across_m_hs_avg(
               predictions=adjusted_hs_predictions_avg,
               model_specs==model_specs
             )),
  # nest predictions by crop and model spec
  tar_target(hs_predictions_nested,
             {
                 z <- split(adjusted_hs_predictions_avg_level_3, 
                            adjusted_hs_predictions_avg_level_3$model_spec)
                 # split by time period x gcm combo
                 # 1 list of 4 crop sublists of 4 time period sublists of 1405484 (60k x 23 gcm)
                 a <- lapply(1:5, function(i){
                   split(z[[i]], 
                         z[[i]]$crop_pooled
                   )      })
                 a
             }),
  # rasterise
  tar_target(hs_predictions_raster,
             rasterise_hs_predictions(
               predictions=hs_predictions_nested
             )),
  # map
  tar_target(hs_predictions_plot,
             plot_hs_predictions(
               predictions=hs_predictions_raster,
               returnStack=TRUE,
               World=World,
               model_spec_alphabetical=model_spec_alphabetical,
               crops=crops,
               path="results/figures/historical_predictions/historical_predictions_%s.png"
             )),

  # extract actual yields from GDHY
  tar_target(crops_lwr,
             c("maize", "rice", "soybean", "wheat")),
  tar_target(gdhy_ts_data,
             get_gdhy_yield_data(crops_lwr=crops_lwr)),
  tar_target(gdhy_ts_df, # 36 million rows
             {library(dplyr)
               
               df <- lapply(1:4, function(crop){
               data.table::rbindlist(gdhy_ts_data[[crop]], idcol="year_id")
             }) %>% 
                 data.table::rbindlist(., idcol="crop_id")
             
             crop_concord <- tribble(~crop_id, ~crop_pooled,
                                     1, "Maize",
                                     2, "Rice",
                                     3, "Soy",
                                     4, "Wheat")
             
             year_concord <- data.frame(year_id=c(1:35), year=c(1981:2015))
             
             df %>% left_join(crop_concord, by="crop_id") %>% 
               left_join(year_concord, by="year_id") %>% 
               dplyr::select(!c("crop_id", "year_id"))
             }),
  # calculate average yields for each period
  tar_target(gdhy_ts_avg,
             {library(dplyr)
               gdhy_ts_df %>% 
                 filter(year %in% c(1981:1995, 2000:2015)) %>% 
                 mutate(period=case_when(year %in% c(1981:1995) ~ "1981_1995",
                                         year %in% c(2000:2015) ~ "2000_2015")) %>% 
                 group_by(lon, lat, crop_pooled, period) %>% 
                 summarise(avg_yield=mean(bs_yield, na.rm=T)) %>% 
                 tidyr::pivot_wider(values_from=avg_yield, 
                                    names_from=period) %>% 
                 mutate(Yield.Change = (`2000_2015` - `1981_1995`)/`1981_1995`) %>% 
                 group_by(crop_pooled) %>% 
                 group_split()
               }),
  tar_target(gdhy_ts_raster,
             {lapply(1:4, function(crop){  
               x <- gdhy_ts_avg[[crop]]
               r <- x[c("lon","lat","Yield.Change")]
               r <- raster::rasterFromXYZ(r)
               raster::crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
               r
             })
             }),
  tar_target(gdhy_plot,
             plot_gdhy_yields(
               predictions=gdhy_ts_raster,
               returnStack=TRUE,
               World=World,
               crops=crops,
               path="results/figures/historical_predictions/gdhy_yield_change.png"
               
             )),
  
  
  # calculate variation in actual yields relative to expected yields
  
  # redoing with annual years from 2000-2015 --------------------------------
  # 
  # 
  # # redoing this as a time series for each year from 2000-2015 relative to avg baseline period 1980-1995!
  tar_target(hs_tmp_pre,
             get_hs_vars(
               data1=hs_years_tmp,
               data2=hs_years_pre
             )),
  tar_target(hs_tmp_pre_annual,
             calc_hs_annual_change(
               hs_tmp_pre=hs_tmp_pre
             )),
  # # repeat for CO2 (use global CO2 observation)
  # # tar_target(co2_hs_file,
  # #            "data/co2_annmean_mlo.csv",
  # #            format='file'),
  # # tar_target(hs_co2,
  # #            {readr::read_csv("data/co2_annmean_mlo.csv", skip=59)}
  # #            ),
  # # calculate change in CO2 across periods, then f_CO2
  tar_target(hs_co2_change,
             calculate_hs_co2(hs_co2)
  ),
  # 
  # # merge w 2000 yields
  tar_target(hs_tmp_pre_yld,
             merge_cru_monf_prediction_data(
               data1=hs_tmp_pre_annual,
               data2=crop_yield_monf_dt
             )),
  # merge w country names
  tar_target(hs_vars,
             create_bs_2015_vars(
               data1=hs_tmp_pre_yld,
               data2=coords_countries
             )),
  # join everything and filter for complete cases
  tar_target(hs_prediction_data,
             create_hs_prediction_data(
               hs_vars=hs_vars,
               hs_co2_change=hs_co2_change
             )),
  # create prediction data where climate change vars = 0 for post-adj
  tar_target(hs_adj_prediction_data,
             {lapply(1:4, function(crop){
               hs_prediction_data[[crop]] %>%
                 mutate(Temp.Change=0,
                        Precipitation.change=0,
                        f_CO2=0) %>%
                 dplyr::filter(complete.cases(.))
             })}),
  # # tar_target(fit_crop_nested_shallow,
  # #            {# need to have 2 level list, so that spec and m are mixed in same list
  # #              a <- split(fit_attr, sapply(
  # #                fit_attr, function(x) {attr(x, "crop")}
  # #              ))
  # #              a # length 4 of 25 fits each
  # #            }),
  # #split prediction data by crop
  tar_target(hs_og_prediction_data_maize,
             hs_prediction_data[[1]]),
  tar_target(hs_og_prediction_data_rice,
             hs_prediction_data[[2]]),
  tar_target(hs_og_prediction_data_soy,
             hs_prediction_data[[3]]),
  tar_target(hs_og_prediction_data_wheat,
             hs_prediction_data[[4]]),
  tar_target(hs_adj_prediction_data_maize,
             hs_adj_prediction_data[[1]]),
  tar_target(hs_adj_prediction_data_rice,
             hs_adj_prediction_data[[2]]),
  tar_target(hs_adj_prediction_data_soy,
             hs_adj_prediction_data[[3]]),
  tar_target(hs_adj_prediction_data_wheat,
             hs_adj_prediction_data[[4]]),
  #predict level 4 og using all model specs
  tar_target(hs_og_predictions_maize,
             predict_level_5_hs(
               fit=fit_maize_nested,
               data=hs_og_prediction_data_maize,
               ncores=5
             ), # however we don't need to cross, just map
             # crop fit to crop data (and inside crop fit, we have spec and m in list)
             pattern=cross(fit_maize_nested,
                           hs_og_prediction_data_maize),
             iteration='list',
             format='fst_tbl')
  # tar_target(hs_og_predictions_rice,
  #            predict_level_5_hs(
  #              fit=fit_rice_nested,
  #              data=hs_og_prediction_data_rice,
  #              ncores=5
  #            ), # however we don't need to cross, just map
  #            # crop fit to crop data (and inside crop fit, we have spec and m in list)
  #            pattern=cross(fit_rice_nested, 
  #                          hs_og_prediction_data_rice),
  #            iteration='list',
  #            format='fst_tbl'),
  # tar_target(hs_og_predictions_soy,
  #            predict_level_5_hs(
  #              fit=fit_soy_nested,
  #              data=hs_og_prediction_data_soy,
  #              ncores=5
  #            ), # however we don't need to cross, just map
  #            # crop fit to crop data (and inside crop fit, we have spec and m in list)
  #            pattern=cross(fit_soy_nested, 
  #                          hs_og_prediction_data_soy),
  #            iteration='list',
  #            format='fst_tbl'),
  # tar_target(hs_og_predictions_wheat,
  #            predict_level_5_hs(
  #              fit=fit_wheat_nested,
  #              data=hs_og_prediction_data_wheat,
  #              ncores=5
  #            ), # however we don't need to cross, just map
  #            # crop fit to crop data (and inside crop fit, we have spec and m in list)
  #            pattern=cross(fit_wheat_nested, 
  #                          hs_og_prediction_data_wheat),
  #            iteration='list',
  #            format='fst_tbl'),
  # # predict level 4 adj 
  # tar_target(hs_adj_predictions_maize,
  #            predict_level_5_hs(
  #              fit=fit_maize_nested,
  #              data=hs_adj_prediction_data_maize,
  #              ncores=5
  #            ),
  #            pattern=cross(fit_maize_nested, 
  #                          hs_adj_prediction_data),
  #            iteration='list',
  #            format='fst_tbl'),
  # tar_target(hs_adj_predictions_rice,
  #            predict_level_5_hs(
  #              fit=fit_rice_nested,
  #              data=hs_adj_prediction_data_rice,
  #              ncores=5
  #            ),
  #            pattern=cross(fit_rice_nested, 
  #                          hs_adj_prediction_data_rice),
  #            iteration='list',
  #            format='fst_tbl'),
  # tar_target(hs_adj_predictions_soy,
  #            predict_level_5_hs(
  #              fit=fit_soy_nested,
  #              data=hs_adj_prediction_data_soy,
  #              ncores=5
  #            ),
  #            pattern=cross(fit_soy_nested, 
  #                          hs_adj_prediction_data_soy),
  #            iteration='list',
  #            format='fst_tbl'),
  # tar_target(hs_adj_predictions_wheat,
  #            predict_level_5_hs(
  #              fit=fit_wheat_nested,
  #              data=hs_adj_prediction_data_wheat,
  #              ncores=5
  #            ),
  #            pattern=cross(fit_wheat_nested, 
  #                          hs_adj_prediction_data_wheat),
  #            iteration='list',
  #            format='fst_tbl')
  
  #### FIX THIS BEFORE RUNNING ####
  # adjust
  # tar_target(adjusted_hs_predictions_level_4,
  #            {hs_og_predictions %>% 
  #              left_join(
  #                hs_adj_predictions, by=c(
  #                  "lon","lat", "crop_pooled", "model_spec","m", 
  #                  "Baseline_tmp", "Baseline_pre", "Baseline_yield", "C3", "C4", "adapt_dummy", "Country2_fact")
  #                # there will be duplicated variables for Temp.Change; Precipitation.change, f_CO2 as intended
  #              ) %>% 
  #              mutate(prediction.fit=prediction.fit.x-prediction.fit.y)},
  #            pattern=map(hs_adj_predictions,
  #                        hs_og_predictions),
  #            iteration='list',
  #            format='fst_tbl'),
  # # mutate with model spec names & remove formulas
  # # pool across m to get to level 3
  # # these predictions give 'expected yields'
  # tar_target(adj_hs_predictions_level_3,
  #            pool_across_m_hs(
  #              predictions=adjusted_hs_predictions_level_4,
  #              model_specs=model_specs
  #            ))
)


# calculate probability of 20% crop loss event

# extract future predictions for this region for each future time period; 

# calculate annual probabilities of 20% crop loss events in each future time period

# calculate & plot average return period of 20% crop loss event in each future time period