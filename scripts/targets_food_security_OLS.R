
packages <- c("dplyr", "tidyr", "data.table", "lubridate", "viridis", "sp", "sf",
              "rgdal", "broom", "raster", "ncdf4", "here", "rworldmap", "R.utils",
              "stringr", "mice", "cleangeo", "renv", "targets",
              "readr", "terra", "qs", "rlang", "future", "purrr", "parallelly", "fst",
              "tmap", "cowplot", "ggpubr", "rsample", "furrr", "spData")

tar_option_set(packages=packages,
               memory="transient",
               garbage_collection=TRUE,
               format="qs")

targets_food_security_lm <- list(
  
  # estimate future production -------------------------------------------
  
  # adj_predictions_by_time_period but filter for glm rs and 2021-2040, remove model_spec column
  # then split by crop 
  # call yield change predictions in 2021-2040 and multiply by 2015 yield levels
  tar_target(predictions_lm_future,
             select_lm_predictions(
               predictions=adj_predictions_by_time_period
             )
  ),
  
  
  
  ## estimate projected production relative to production in 2015 from GAEZ data
  
  tar_target(yields_lm_future_raster,
             calc_future_yields(
               yields_data=grogan_yield_raster,
               predictions=predictions_lm_future
             )
             
  ),
  # resample yields raster to be ahigher resolution
  tar_target(yields_resampled_future_raster_lm,
             lapply(1:4, function(time_period){
               lapply(1:4, function(crop){
                 resample(yields_lm_future_raster[[time_period]][[crop]], 
                          grogan_hectares_raster[[crop]], 
                          method="bilinear")
               })
             })
  ),
  
  
  tar_target(predictions_lm_future_raster,
             {lapply(1:4, function(time_period){
               lapply(1:4, function(crop){
                 
                 yields_resampled_future_raster_lm[[time_period]][[crop]] * grogan_hectares_raster[[crop]] * 1000 
               })
               
             })
               
             }),
  
  tar_target(country_predictions_lm_future_list,
             extract_country_production_predictions(
               predictions=predictions_lm_future_raster,
               World=World
             )
  ),
  
  
  # calculate exports and imports ------------------------------------------------
  
  
  # rbindlist long - country production in tons
  tar_target(country_production_future_df_lm,
             rbind_country_production_predictions(
               predictions=country_predictions_lm_future_list
             )),
  
  # compare baseline and future production in tons
  tar_target(country_baseline_future_production_lm,
             compare_baseline_future_production(
               predictions=country_predictions_lm_future_list,
               worldmap_clean=worldmap_clean,
               fao_production_2015_check=fao_production_2015_check
             )
  ),
  # current production from fao and from grogan yield x hectares data
  tar_target(country_baseline_future_production_df_lm,
             rbind_country_baseline_future_production_by_crop(
               country_baseline_future_production=country_baseline_future_production_lm,
               crops=crops
             )
  ),
  # left join to future yield production by Reporter Countries = Country and crops
  tar_target(est_trade_exports_lm,
             calc_exports(
               fao_export_share=fao_export_share,
               future_production=country_production_future_df_lm,
               crops=crops,
               baseline_production=country_baseline_future_production_df_lm
             )
  ),
  
  
  # # calculate export prop by partner
  
  tar_target(est_trade_partners_lm,
             calc_export_partner_share(
               fao_export_data=fao_export_data,
               est_exports=est_trade_exports_lm,
               concord=fao_item_concord
               
             )
  ),
  # calculate future imports for each country (sum of exports across all export partners)
  
  tar_target(est_trade_imports_lm,
             
             calc_imports(
               est_trade_partners=est_trade_partners_lm
             )
             
  ),   # expressed in tons
  
  
  # calculate calorie conversions -------------------------------------------
  
  
  # calculate future food gap -----------------------------------------------
  
  # sum future production and imports
  tar_target(fao_future_food_supply_lm,
             calc_future_food_supply(
               est_imports=est_trade_imports_lm,
               est_exports=est_trade_exports_lm,
               fao_crop_allocation_multiyear=fao_crop_allocation_multiyear
             )
             
             
  ),
  
  # left join feed and food calorie conversion back to fao_future_food_supply
  tar_target(future_calories_lm,
             calc_total_future_calories(
               fao_future_food_supply=fao_future_food_supply_lm,
               calorie_conversion_by_element=calorie_conversion_by_element
             )
             
  ),
  tar_target(future_total_calories_lm,
             future_calories_lm %>% 
               group_by(`Partner Countries`, `Partner Country Code (ISO2)`, time_period) %>% 
               summarise(total_calories=sum(total_calories, na.rm=T))
  ),
  # join with country_pop_mder for country annual mder shortfall
  tar_target(future_food_gap_lm,
             calc_future_food_gap(
               future_total_calories=future_total_calories_lm,
               worldmap_clean=worldmap_clean,
               country_pop_mder=country_pop_ader_future
             )
  ),

  tar_target(future_calorie_gap_lm,
             calc_future_calorie_gap(
               future_food_gap=future_food_gap_lm,
               country_pop_mder=country_pop_ader_future,
               fao_staple_share=fao_staple_share
             )
  ),
  
  
  # calculate baseline food gap -------------------------------------------
  
  
  # left join crop conversions to calculate total calories in baseline and future 
  tar_target(baseline_2015_calories_by_crop_lm,
             calc_baseline_calories_by_crop(
               country_baseline_future_production_df=country_baseline_future_production_df_lm,
               fao_crop_allocation_multiyear=fao_crop_allocation_multiyear,
               calorie_conversion_by_element=calorie_conversion_by_element,
               est_imports=est_trade_imports_lm,
               est_exports=est_trade_exports_lm,
               crops=crops
             )
  ),
  
  
  tar_target(baseline_2015_calorie_gap_lm,
             calc_baseline_calorie_gap(
               baseline_2015_calories_by_crop=baseline_2015_calories_by_crop_lm,
               country_pop_mder_2015=country_pop_ader_2015,
               fao_staple_share=fao_staple_share
             )
             
  ),
  
  
  # calculate change and plots  ---------------------------------------------
  
  
  # calculate total production in tons across all crops

  tar_target(country_baseline_future_production_totals_lm,
             aggregate_country_baseline_future_production(
               country_baseline_future_production_df=country_baseline_future_production_df_lm
             )
  ),
  
  
  # break down the difference between current production & future production
  tar_target(calorie_gap_change_lm,
             calc_calorie_gap_change(
               future_calorie_gap=future_calorie_gap_lm,
               baseline_2015_calorie_gap=baseline_2015_calorie_gap_lm,
               outfile="processed/calorie_gap_change_lm_RCP8.5.csv")
  ),
  # count frequ
  tar_target(freq_FI_status_change_lm,
             {
               lvls <- unique(unlist(calorie_gap_change_lm$FI_status_change))
               
               freq <- sapply(calorie_gap_change_lm,
                              function(x) table(factor(x, levels = lvls,
                                                       ordered=TRUE)))
             }),
  
  # heat map of just the four categorical variables of FI status change
  tar_target(FI_status_change_map_lm,
             plot_FI_status_change(
               calorie_gap_change=calorie_gap_change_lm,
               World=World,
               outfile="results/figures/food security/change_in_FI_status_lm_RCP8.5.png"
             )
  ),

  # map change in supply in % terms from baseline supply
  tar_target(change_supply_pct_map_lm,
             map_change_supply_pct(
               future_calorie_gap=future_calorie_gap_lm,
               baseline_2015_calorie_gap=baseline_2015_calorie_gap_lm,
               World=World,
               outfile="results/figures/food security/change_supply_pct_map_lm_RCP8.5.png"
               
             )
  )
  
)
