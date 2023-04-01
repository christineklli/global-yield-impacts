
packages <- c("dplyr", "tidyr", "data.table", "lubridate", "viridis", "sp", "sf",
              "rgdal", "broom", "raster", "ncdf4", "here", "rworldmap", "R.utils",
              "stringr", "mice", "cleangeo", "renv", "targets",
              "readr", "terra", "qs", "rlang", "future", "purrr", "parallelly", "fst",
              "tmap", "cowplot", "ggpubr", "rsample", "furrr", "spData")

tar_option_set(packages=packages,
               memory="transient",
               garbage_collection=TRUE,
               format="qs")

targets_food_security <- list(
  
  
  # read in future population data  ----------------------------------------
  
  
  tar_target(pop_10km_file,
             {here("data", "Food security data", "RCP8.5_10000m.tif")}),
  tar_target(pop_10km_data,
             rasterise_pop_10km_file(
               file=pop_10km_file,
               outfile="processed/pop_10km_data_RCP8.5.tif"
             )
  ), 
  tar_target(pop_10km_raster_file,
             {pop_10km_data},
             format="file"),
  
  tar_target(pop_country_future_worldmap, # 177 countries ("name_long")
             calc_pop(pop_10km_raster_file)
  ),
  # join to iso_a3 (which should match mder three-digit Code)
  # minimum daily energy requirements
  tar_target(mder_data,
             read_csv(here("data", "Food security data", "minimum-requirement-calories.csv"))), # 194 countries ("Entity")
  # this is a time series from 2000-2020 so take only 2020?
  # also variable is kcal/person/day, multiply by 365 for annual kcal per capita needs?
  # join pop and mder data
  # add average daily energy requirements
  # there are some NAs
  tar_target(ader_data,
             read_csv(here("data", "Food security data", "FAOSTAT_data_en_4-1-2023_ader.csv"))),
  
  tar_target(country_pop_mder_future,
             calc_country_mder(
               mder_data = mder_data,
               pop_data = pop_country_future_worldmap
             ) # 194-177=17 countries will not have population information
  ),
  tar_target(country_pop_ader_future,
             calc_country_ader(
               ader_data = ader_data,
               pop_data = pop_country_future_worldmap,
               worldmap_clean=worldmap_clean
             ) # 194-177=17 countries will not have population information
  ),
  
  # read in grogan data  ----------------------------------------------------
  
  
  tar_target(grogan_hectares_data,
             c("data/GAEZ data/GAEZAct2015_HarvArea_Maize_Total.tif",
               "data/GAEZ data/GAEZAct2015_HarvArea_Rice_Total.tif",
               "data/GAEZ data/GAEZAct2015_HarvArea_Soybean_Total.tif",
               "data/GAEZ data/GAEZAct2015_HarvArea_Wheat_Total.tif"),
             format="file"
  ),
  tar_target(grogan_hectares_raster,
             { # calculate total number of hectares per pixel - this is in 1000 ha units
               r <- raster::stack(grogan_hectares_data)
               #raster::aggregate(r, c(0.5,0.5)/res(r), sum)
               r
             }
  ),
  tar_target(grogan_yield_data,
             c("data/GAEZ data/GAEZAct2015_Yield_Maize_Mean.tif",
               "data/GAEZ data/GAEZAct2015_Yield_Rice_Mean.tif",
               "data/GAEZ data/GAEZAct2015_Yield_Soybean_Mean.tif",
               "data/GAEZ data/GAEZAct2015_Yield_Wheat_Mean.tif"),
             format="file"
  ),
  tar_target(grogan_yield_raster,
             { # calculate mean baseline yield across the pixel 
               r <- raster::stack(grogan_yield_data)
               #raster::aggregate(r, c(0.5,0.5)/res(r), mean)
               r
             }
  ),
  
  # baseline production checks ------------------------------------------------------------------
  
  #FAO vs Grogan
  
  
  # check baseline 2015 production against FAO data - testing this method of converting yields to production volumes
  tar_target(baseline_yields_production,
             calc_baseline_production(
               hectares_data=grogan_hectares_raster,
               yields_data=grogan_yield_raster,
               World=World
             )
  ),
  
  # get FAO to ISO_A2 correspondance
  tar_target(fao_iso2,
             read_iso2_correspondance(
               file="data/Food security data/iso2.csv"
             )),
  # get FAO 2015 production data for comparison (actually averaged 2014-2016 like in Grogan et al. 2022)
  tar_target(fao_production_2015_data,
             {data <- read_csv("data/Food security data/FAOSTAT_data_en_3-29-2023_production_201416.csv")
             data %>% 
               mutate(Value=ifelse(is.na(Value),0,Value)) %>% 
               mutate(`Area Code (ISO2)`=ifelse(Area=="Namibia","NA",`Area Code (ISO2)`))
             }),
  # compare with FAO data - 
  tar_target(fao_production_2015_check,
             check_baseline_production(
               fao_production=fao_production_2015_data,
               grogan_production=baseline_yields_production
             )),
  tar_target(write_baseline_production_check,
             {fao_production_2015_check %>% 
                 rbindlist(., idcol="crop") %>% 
                 write_csv("processed/baseline_production_fao_grogan_check.csv")
             }),
  tar_target(global_comparison_check, # note this takes grogan GAEZ data at 5 arcminute resolution
             {lapply(1:4, function(i) {
               fao_production_2015_check[[i]] %>% 
                 summarise(sum_production_calc=sum(Value_calc, na.rm=T),
                           sum_production_fao=sum(Value_fao, na.rm=T)) %>% 
                 mutate(calc_fao_prop = sum_production_calc/sum_production_fao)
             })
             }
             
  ),
  
  # estimate future production -------------------------------------------
  
  # adj_predictions_by_time_period but filter for glm rs and 2021-2040, remove model_spec column
  # then split by crop 
  # call yield change predictions in 2021-2040 and multiply by 2015 yield levels
  tar_target(predictions_glmrs_future,
             format_predictions(
               predictions=adj_predictions_by_time_period
             )
  ),
  
  
  
  ## estimate projected production relative to production in 2015 from GAEZ data
  
  tar_target(yields_glmrs_future_raster,
             calc_future_yields(
               yields_data=grogan_yield_raster,
               predictions=predictions_glmrs_future
             )
             
  ),
  # resample yields raster to be ahigher resolution
  tar_target(yields_resampled_future_raster,
             lapply(1:4, function(time_period){
               lapply(1:4, function(crop){
                 resample(yields_glmrs_future_raster[[time_period]][[crop]], 
                          grogan_hectares_raster[[crop]], 
                          method="bilinear")
               })
             })
  ),
  
  
  tar_target(predictions_glmrs_future_raster,
             {lapply(1:4, function(time_period){
               lapply(1:4, function(crop){
                 
                 yields_resampled_future_raster[[time_period]][[crop]] * grogan_hectares_raster[[crop]] * 1000 
               })
               
             })
               
             }),
  
  tar_target(country_predictions_glmrs_future_list,
             extract_country_production_predictions(
               predictions=predictions_glmrs_future_raster,
               World=World
             )
  ),
  
  
  
  
  # read in crop allocation data ---------------------------------------------------------
  
  
  
  # FAO crop allocation data
  tar_target(fao_crop_allocation_files,
             {
               list.files(here(
                 "data", 
                 "Food security data",
                 "FAOSTAT_FBS_staple_201416"), 
                 pattern="^.*\\.csv$")
             }
             
  ),
  tar_target(fao_crop_allocation_data,
             {y <- lapply(fao_crop_allocation_files, function(x){
               read_csv(
                 
                 sprintf("data/Food security data/FAOSTAT_FBS_staple_201416/%s",x)
               )
               
             })
             rbindlist(y) %>% 
               mutate(Value=ifelse(is.na(Value),0,Value)) %>% 
               mutate(`Area Code (ISO2)`=ifelse(Area=="Namibia","NA",`Area Code (ISO2)`))
             }
  ),
  # calculate domestic crop use proportions
  tar_target(fao_crop_allocation_pct,
             calc_crop_allocation(
               data=fao_crop_allocation_data
             )
             
  ),
  # calculate domestic crop use proportions averaged over 2018-2020
  tar_target(fao_crop_allocation_multiyear,
             calc_multiyear_crop_allocation(
               data=fao_crop_allocation_data
             )),
  # now to calculate importing nations allocations
  # read in TM data
  
  # read in trade data  -----------------------------------------------------------------
  
  # read in all trade files
  tar_target(fao_full_trade_files,
             {
               list.files(here(
                 "data", 
                 "Food security data",
                 "FAOSTAT_trade_data_201416"), 
                 pattern="^.*\\.csv$")
             }
  ),
  # read data and rbind them all together
  tar_target(fao_trade_full_data,
             {
               y <- lapply(fao_full_trade_files, function(x){
                 read_csv(
                   
                   sprintf("data/Food security data/FAOSTAT_trade_data_201416/%s",x)
                 )
                 
               })
               rbindlist(y)
             }),
  
  # check whether 'rice' is a total of the other rice categories
  # as it is not included in the default composition of rice and other products
  # in the FBS definitions and standards
  tar_target(fao_trade_rice_checks,
             check_fao_trade_rice(
               data=fao_trade_full_data
             )
  ),
  # ascertain that item code 30 is the sum of all the other rice categories
  tar_target(fao_export_data,
             {data <- fao_trade_full_data %>% 
               filter(!`Item Code (FAO)` %in% c(32, 28, 29, 31, 27)) %>% 
               filter(Element=="Export Quantity") %>% 
               mutate(`Partner Country Code (ISO2)`=ifelse(
                 `Partner Countries`=="Namibia", 
                 "NA", 
                 `Partner Country Code (ISO2)`))
             
             data %>% 
               mutate(Value=ifelse(is.na(Value),0,Value)) 
             # now append missing Reporter countries as having export quantity == NA or 0?
             }
  ),
  
  # read in concordance data
  tar_target(fao_item_concord,
             {
               readxl::read_xlsx("data/Food security data/FAO_trade_FBS_concordance.xlsx")}),
  
  
  # tar_target(fao_trade_filtered,
  #            read_csv(
  #              "data/Food security data/FAOSTAT_data_en_3-25-2023_trade_201416.csv"
  #            )),
  # tar_target(fao_crops,
  #            unique(fao_crop_allocation_data$Item)),
  # 
  # tar_target(concord,
  #            {data.frame(
  #              alloc_crops = fao_crops, # same order
  #              trade_crops = c("Wheat", "Rice", "Maize (corn)", "Soya beans")
  #            )}),
  # check that export quantities across FBS and TM datasets per country and crop match
  # as crop labels do not match
  tar_target(fao_fbs_trade_checks,
             check_fbs_tm_data(
               fao_export_data=fao_export_data,
               fbs_data=fao_crop_allocation_data,
               concord=fao_item_concord,
               outfile="processed/fao_fbs_trade_checks.csv"
             )),
  
  tar_target(fao_trade_export,
             process_export_data(
               fao_export_data=fao_export_data,
               concord=fao_item_concord,
               fbs_data=fao_crop_allocation_pct
               
             )
  ),
  
  # calculate total export proportion today
  tar_target(fao_export_share,
             calc_fao_export_share(
               fao_trade_export=fao_trade_export
             )),
  
  
  # calculate exports and imports ------------------------------------------------
  
  
  # rbindlist long - country production in tons
  tar_target(country_production_future_df,
             rbind_country_production_predictions(
               predictions=country_predictions_glmrs_future_list
             )),
  
  # compare baseline and future production in tons
  tar_target(country_baseline_future_production,
             compare_baseline_future_production(
               predictions=country_predictions_glmrs_future_list,
               worldmap_clean=worldmap_clean,
               fao_production_2015_check=fao_production_2015_check
             )
  ),
  # current production from fao and from grogan yield x hectares data
  tar_target(country_baseline_future_production_df,
             rbind_country_baseline_future_production_by_crop(
               country_baseline_future_production=country_baseline_future_production,
               crops=crops
             )
  ),
  # left join to future yield production by Reporter Countries = Country and crops
  tar_target(est_trade_exports,
             calc_exports(
               fao_export_share=fao_export_share,
               future_production=country_production_future_df,
               crops=crops,
               baseline_production=country_baseline_future_production_df
             )
  ),
  
  
  # # calculate export prop by partner
  
  tar_target(est_trade_partners,
             calc_export_partner_share(
               fao_export_data=fao_export_data,
               est_exports=est_trade_exports,
               concord=fao_item_concord
               
             )
  ),
  # calculate future imports for each country (sum of exports across all export partners)
  
  tar_target(est_trade_imports,
             
             calc_imports(
               est_trade_partners=est_trade_partners
             )
             
  ),   # expressed in tons
  
  # read in import quantities from 2014-2016 of crops from FAO by country from FBS
  # for checks
  tar_target(fao_import_export_2015,
             process_fao_import_export_2015_data(
               fbs_data=fao_crop_allocation_data
             )
  ),
  
  # check and compare baseline estimated/imputed imports/exports and FAO imports/exports
  tar_target(baseline_import_export_checks,
             
             check_baseline_import_export(
               data=fao_import_export_2015,
               est_imports=est_trade_imports,
               est_exports=est_trade_exports,
               outfile="processed/baseline_import_export_checks.csv"
             )
  ),
  
  
  # calculate calorie conversions -------------------------------------------
  
  
  
  # calculate food use in tons 
  # convert to food calories
  # calculate feed allocation in tons
  # convert feed calories to food calories
  tar_target(fao_animal_production_data,
             {
               data <- read_csv("data/Food security data/FAOSTAT_data_en_3-29-2023_feed_production_201416.csv")
               data %>% 
                 mutate(Value=ifelse(is.na(Value),0,Value)) %>% 
                 mutate(`Area Code (ISO2)`=ifelse(Area=="Namibia","NA",`Area Code (ISO2)`))
               
             }
             
  ),
  
  tar_target(fao_animal_production_feed,
             {  # sum production per product over the years
               country_item_production <- fao_animal_production_data %>%
                 group_by(Area, `Area Code (ISO2)`, Item) %>% 
                 summarise(Sum_Value = sum(Value, na.rm=T))
               
             }),
  # read in world bank country economic group
  tar_target(world_bank_country_list,
             readxl::read_xlsx("data/Food security data/World_Bank_3-13-2023_CLASS.xlsx",
                               sheet="List of economies")),
  # add Cassidy et al. 2011 grazing systems estimate from SI
  tar_target(grazing_income_group,
             {tribble(
               ~`Income group`, ~grazing_prop, ~Item,
               "High income", 0.16, "Bovine Meat",
               "Upper middle income", 0.16, "Bovine Meat",
               "Lower middle income",  0.32, "Bovine meat",
               "Low income", 0.32, "Bovine Meat"
             )}),
  # livestock feed conversion efficiencies from Cassidy et al. 2011 Table S6
  tar_target(livestock_feed_conversion,
             {tribble(
               ~Item, ~conversion,
               "Bovine Meat", 0.0308,
               "Pigmeat", 0.1043,
               "Poultry Meat", 0.1178,
               "Eggs", 0.2207,
               "Milk - Excluding Butter", 0.4025
             )}),
  # join both to fao_animal_production_feed
  tar_target(fao_animal_product_feed_calories,
             process_animal_feed_data(
               animal_production_data=fao_animal_production_feed,
               country_income_data=world_bank_country_list,
               grazing_income_group=grazing_income_group,
               livestock_feed_conversion=livestock_feed_conversion
             )
  ),
  
  # # calculate weighted average feed-to-calories conversion factor by country
  tar_target(fao_country_feed_calories,
             {
               fao_animal_product_feed_calories %>%
                 group_by(Area, `Area Code (ISO2)`) %>% 
                 summarise(feed_conversion = weighted.mean(
                   conversion, Prop
                 )) 
             }),
  # create tribble of food to calories conversion from Cassidy et al. 2013 for 4 crops
  tar_target(crop_calorie_conversion,
             {tribble(
               ~crop_conversion, ~`Item (FBS)`,
               3580802.60, "Maize and products",
               2800000.00, "Rice and products",
               3596499.11, "Soyabeans",
               3284000.00, "Wheat and products"
             )}),
  
  # staple product calorie share
  tar_target(fao_all_products_kcal,
             {
               data <- read_csv("data/Food security data/FAOSTAT_data_en_3-29-2023_kcal_201416.csv")
               
               data %>% 
                 mutate(Value=ifelse(is.na(Value),0,Value)) %>% 
                 mutate(`Area Code (ISO2)`=ifelse(Area=="Namibia","NA",`Area Code (ISO2)`))
             }),
  tar_target(fao_staple_share,
             calc_staple_share_calories(
               fao_all_products_kcal=fao_all_products_kcal,
               world_bank_country_list=world_bank_country_list,
               grazing_income_group=grazing_income_group
             )
  ),
  
  
  # calculate future food gap -----------------------------------------------
  
  # sum future production and imports
  tar_target(fao_future_food_supply,
             calc_future_food_supply(
               est_imports=est_trade_imports,
               est_exports=est_trade_exports,
               fao_crop_allocation_multiyear=fao_crop_allocation_multiyear
             )
             
             
  ),
  
  # left join feed and food calorie conversion back to fao_future_food_supply
  tar_target(future_calories,
             calc_total_future_calories(
               fao_future_food_supply=fao_future_food_supply,
               fao_country_feed_calories=fao_country_feed_calories,
               crop_calorie_conversion=crop_calorie_conversion
             )
             
  ),
  tar_target(future_total_calories,
             future_calories %>% 
               group_by(`Partner Countries`, `Partner Country Code (ISO2)`, time_period) %>% 
               summarise(total_calories=sum(total_calories, na.rm=T))
  ),
  # join with country_pop_mder for country annual mder shortfall
  tar_target(future_food_gap,
             calc_future_food_gap(
               future_total_calories=future_total_calories,
               worldmap_clean=worldmap_clean,
               country_pop_mder=country_pop_ader_future
             )
  ),
  tar_target(global_food_gap, # this needs to be redone 
             # as should be excluding imports/exports? just count production?
             {future_food_gap %>% ungroup() %>% group_by(time_period) %>% 
                 summarise(total_calorie_supply = sum(calories_supply, na.rm=T),
                           total_calorie_demand = sum(calories_demand, na.rm=T),
                           calorie_gap = total_calorie_demand/total_calorie_supply)}),
  # # calculate only calories from food (not feed) and total calories from crops other than maize/rice/soy/wheat
  # tar_target(future_crop_calories,
  #            {future_calories %>% 
  #                group_by(`Partner Countries`) %>% 
  #                summarise(total_food_calories=sum(food_calories, na.rm=T)) %>% 
  #                mutate(total_calories = total_food_calories/0.6417) # from tilman; maize+rice+soy+wheat=0.6417 (not including feed)
  #            }),
  
  # calculate prevalence of undernourishment
  # (supply/0.7 - demand)/(mder per person per year) and /population
  tar_target(future_calorie_gap,
             calc_future_calorie_gap(
               future_food_gap=future_food_gap,
               country_pop_mder=country_pop_ader_future,
               fao_staple_share=fao_staple_share
             )
  ),
  
  
  # calculate baseline food gap -------------------------------------------
  
  
  # left join crop conversions to calculate total calories in baseline and future 
  tar_target(baseline_2015_calories_by_crop,
             calc_baseline_calories_by_crop(
               country_baseline_future_production_df=country_baseline_future_production_df,
               fao_crop_allocation_multiyear=fao_crop_allocation_multiyear,
               fao_country_feed_calories=fao_country_feed_calories,
               crop_calorie_conversion=crop_calorie_conversion,
               est_imports=est_trade_imports,
               est_exports=est_trade_exports,
               crops=crops
             )
  ),
  
  # need average 2015 (2014-2016) population from FAO
  tar_target(fao_population_2015_data,
             {
               
               data <- read_csv("data/Food security data/FAOSTAT_data_en_3-29-2023_population_201416.csv")
               data %>% 
                 mutate(Value=ifelse(is.na(Value),0,Value)) %>% 
                 mutate(`Area Code (ISO2)`=ifelse(Area=="Namibia","NA",`Area Code (ISO2)`))
               
             }),
  
  tar_target(fao_population_2015,
             calc_pop_2015(
               data=fao_population_2015_data,
               worldmap_clean=worldmap_clean
             )
             
             
  ),
  # need 2015 mder_annual_2015
  tar_target(country_pop_mder_2015,
             calc_country_pop_mder_2015(
               mder_data=mder_data,
               fao_population_2015=fao_population_2015
             )
             
  ),
  tar_target(country_pop_ader_2015,
             calc_country_pop_ader_2015(
               ader_data=ader_data,
               fao_population_2015=fao_population_2015,
               worldmap_clean=worldmap_clean
             )
             
  ),
  
  tar_target(baseline_2015_calorie_gap,
             calc_baseline_calorie_gap(
               baseline_2015_calories_by_crop=baseline_2015_calories_by_crop,
               country_pop_mder_2015=country_pop_ader_2015,
               fao_staple_share=fao_staple_share
             )
             
  ),
  
  # check FAO global calories delivered by country, how it compares to our calcs of total_calorie_supply in 2015
  tar_target(comparison_2015_calories_supply,
             check_est_fao_total_calories(
               fao_all_products_kcal=fao_all_products_kcal,
               world_bank_country_list=world_bank_country_list,
               grazing_income_group=grazing_income_group,
               baseline_2015_calorie_gap=baseline_2015_calorie_gap
             )
             
  ),
  # check calorie gap by country if using FAO calorie supply data
  tar_target(comparison_2015_calorie_gap,
             check_est_fao_calorie_gap(
               comparison_2015_calories_supply=comparison_2015_calories_supply,
               worldmap_clean=worldmap_clean,
               country_pop_mder_2015=country_pop_ader_2015,
               outfile="processed/comparison_2015_calorie_gap.csv"
             )
  ),
  tar_target(baseline_global_food_gap,
             {comparison_2015_calorie_gap %>% 
                 ungroup() %>%  
                 summarise(calorie_supply = sum(calories_supply_total_gaez, na.rm=T), 
                           calorie_demand = sum(calories_demand,na.rm=T), 
                           calorie_gap = calorie_demand/calorie_supply)}
  ),
  
  
  # calculate change and plots  ---------------------------------------------
  
  
  # calculate total production in tons across all crops
  # this looks pretty reasonable in terms of changes, am guessing most of the 
  # shortfall in demand comes from growth in population
  tar_target(country_baseline_future_production_totals,
             aggregate_country_baseline_future_production(
               country_baseline_future_production_df=country_baseline_future_production_df
             )
  ),
  
  
  # write out pou_rates future and present into csv
  tar_target(calorie_gap_by_country_future_csv,
             write_csv(future_calorie_gap, "processed/future_calorie_gap_RCP8.5.csv")),
  tar_target(calorie_gap_by_country_baseline_csv,
             write_csv(baseline_2015_calorie_gap, "processed/baseline_2015_calorie_gap_RCP8.5.csv")),
  
  # break down the difference between current production & future production
  tar_target(calorie_gap_change,
             calc_calorie_gap_change(
               future_calorie_gap=future_calorie_gap,
               baseline_2015_calorie_gap=baseline_2015_calorie_gap,
               outfile="processed/calorie_gap_change_RCP8.5.csv")
  ),
  # count frequ
  tar_target(freq_FI_status_change,
             {
               lvls <- unique(unlist(calorie_gap_change$FI_status_change))
               
               freq <- sapply(calorie_gap_change,
                              function(x) table(factor(x, levels = lvls,
                                                       ordered=TRUE)))
             }),
  
  tar_target(calorie_gap_persons_future_map,
             map_calorie_gap_persons(
               data=future_calorie_gap,
               World=world,
               outfile="results/figures/food security/calorie_gap_persons_future_RCP8.5.png"
             )
  ),
  # heat map of just the four categorical variables of FI status change
  tar_target(FI_status_change_map,
             plot_FI_status_change(
               calorie_gap_change=calorie_gap_change,
               World=World,
               outfile="results/figures/food security/change_in_FI_status_RCP8.5.png"
             )
  ),
  # heat map of change in PoU rate 
  # increase in the proportion of food insecure persons (as a percentage point increase)
  tar_target(rate_change_map,
             plot_rate_change(
               calorie_gap_change=calorie_gap_change,
               World=World,
               outfile="results/figures/food security/rate_change_RCP8.5.png"
             )
  ),
  
  # ccalculate change in calories from population growth and production
  tar_target(decomposed_change_in_calories,
             decompose_calorie_gap_change(
               future_calorie_gap=future_calorie_gap,
               baseline_2015_calorie_gap=baseline_2015_calorie_gap,
               comparison_2015_calorie_gap=comparison_2015_calorie_gap # to compare gap if using FAO supply to calculate calorie gap
               
             )
  ),
  # only select 42 countries that were secure and will become insecure
  tar_target(countries_becoming_insecure_2030,
             {
               calorie_gap_change %>% 
                 filter(time_period=="2021-2040") %>% 
                 filter(FI_status_change=="become insecure") %>% 
                 dplyr::select("Country.x", "ISO_A3")
             }),
  # sset of countries that remain insecure
  tar_target(countries_remaining_insecure_2030,
             {
               calorie_gap_change %>% 
                 filter(time_period=="2021-2040") %>% 
                 filter(FI_status_change=="remain insecure") %>% 
                 dplyr::select("Country.x", "ISO_A3")
             }),
  # plot
  # have to join this data with population data and mder data
  tar_target(rate_change_become_insecure_plot,
             plot_rate_change_become_insecure(
               decomposed_change_in_calories=decomposed_change_in_calories,
               countries_becoming_insecure=countries_becoming_insecure_2030,
               time="2021-2040",
               label_future="Demand - Supply, 2030",
               outfile="results/figures/food security/rate_change_become_insecure_RCP8.5.png"
             )
  )
  # tar_target(calorie_gap_become_insecure_plot,
  #            plot_calorie_gap_become_insecure(
  #              decomposed_change_in_calories=decomposed_change_in_calories,
  #              countries_becoming_insecure=countries_becoming_insecure,
  #              outfile="results/figures/food security/calorie_gap_become_insecure_RCP8.5.png"
  #            )
  # ),
  # repeat but in terms of # of hungry people or in terms of hungry people/population
  
  # in terms of hungry persons
  # tar_target(persons_change_decomposed_become_insecure_plot,
  #            plot_persons_change_decomposed_become_insecure(
  #              decomposed_change_in_calories=decomposed_change_in_calories,
  #              countries_becoming_insecure=countries_becoming_insecure,
  #              outfile="results/figures/food security/persons_change_become_insecure_RCP8.5.png"
  #            )
  # ),
  
  # tar_target(rate_change_decomposed_remain_insecure_plot,
  #            plot_rate_change_decomposed_remain_insecure(
  #              decomposed_change_in_calories=decomposed_change_in_calories,
  #              countries_remaining_insecure=countries_remaining_insecure,
  #              outfile="results/figures/food security/rate_change_remain_insecure_RCP8.5.png"
  #            )
  # )
)
