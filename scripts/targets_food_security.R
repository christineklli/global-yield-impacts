
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
  tar_target(pop_10km_file,
             {here("data", "Food security data", "RCP8.5_10000m.tif")},
             format="file"),
  tar_target(pop_10km_data,
             {# somehow terra::rast results in Error: external pointer is not valid
               all <- raster::stack(pop_10km_file) 
               # need only 2021-2040
               near <- raster::subset(all, 11:30)
               # average population over 2021-2040
               mean <- calc(near, fun = mean)
               outfile <- "processed/pop_10km_data.tif"
               writeRaster(mean, outfile, overwrite=TRUE)
               outfile
             }
  ), 
  tar_target(pop_10km_raster_file,
             {pop_10km_data},
             format="file"),
  
  tar_target(pop_country_2021_2040_worldmap, # 177 countries ("name_long")
             {r <- raster::raster(pop_10km_raster_file) 
             worldmap <- cleangeo::clgeo_Clean(rworldmap::countriesCoarse)
             df <- exactextractr::exact_extract(
               r,
               worldmap,
               fun='sum'
             )
             df %>% as_tibble() %>% 
               rename(pop_2021_2040=1) %>% 
               cbind(
                 iso_a3=worldmap$ISO_A3,
                 name=worldmap$NAME
               ) %>% 
               relocate(iso_a3, name, pop_2021_2040) 
             }
  ),
  # join to iso_a3 (which should match mder three-digit Code)
  # minimum daily energy requirements
  tar_target(mder_data,
             read_csv(here("data", "Food security data", "minimum-requirement-calories.csv"))), # 194 countries ("Entity")
  # this is a time series from 2000-2020 so take only 2020?
  # also variable is kcal/person/day, multiply by 365 for annual kcal per capita needs?
  # join pop and mder data
  tar_target(country_pop_mder,
             {mder_data %>% 
                 filter(Year==2020) %>% 
                 left_join(pop_country_2021_2040_worldmap, by = c("Code"="iso_a3")) %>% 
                 dplyr::select(!c("Year")) %>% 
                 rename(mder_daily_2020 = 3) %>% 
                 mutate(mder_annual_2020 = mder_daily_2020 * 365.25,
                        mder_pop_daily_2021_2040 = mder_daily_2020 * pop_2021_2040,
                        mder_pop_annual_2021_2040 = mder_annual_2020 * pop_2021_2040)
             } # 194-177=17 countries will not have population information
  ),
  # get yields data for 2015 from GDHY
  tar_target(yields_gdhy_2015_df,
             {
               lapply(1:4, function(j){
                 
                 lapply(35, function(i) {
                   
                   crops_lwr <- c("maize", "rice", "soybean", "wheat")
                   
                   crop_yields_ts <- list.files(here("data", "GDHY data", crops_lwr[[j]]), pattern = "^.*\\.(nc4)$")
                   
                   data <- nc_open(here("data", "GDHY data", crops_lwr[[j]], crop_yields_ts[[i]]))
                   
                   lon <- ncvar_get(data, "lon")
                   
                   lon[lon > 180] <- lon[lon > 180] - 360
                   
                   lat <- ncvar_get(data, "lat")
                   yields <- ncvar_get(data, "var")
                   
                   fillvalue <- ncatt_get(data, "var", "_FillValue")
                   
                   yields[yields == fillvalue$value] <- NA
                   
                   # set dimension names and values to corresponding lon and lat values
                   dimnames(yields) <- list(lon = lon, lat = lat)
                   
                   as.data.frame.table(yields, responseName = "yields") %>% 
                     mutate(lon=as.character(lon),
                            lon=as.numeric(lon),
                            lat=as.character(lat),
                            lat=as.numeric(lat))
                   
                   
                 })
               })
             }
  ),

  # adj_predictions_by_time_period but filter for glm rs and 2021-2040, remove model_spec column
  # then split by crop 
  # call yield change predictions in 2021-2040 and multiply by 2015 yield levels
  tar_target(predictions_glmrs_2021_2040,
             {
               df <- adj_predictions_by_time_period %>% 
                 filter(model=="glm_RS" & time_period=="2021-2040") %>% 
                 dplyr::select(!c("v_w", "v_b", "v_p", "se_p")) 
               
               df <- split(df, df$crop_pooled)
               
               lapply(1:4, function(i) 
               {
                 df <- df[[i]] %>% 
                   left_join(
                     yields_gdhy_2015_df[[i]][[1]], by = c("lon", "lat")
                   ) %>% 
                   mutate(yield_levels = yields * (1+pred_bar/100)) %>% 
                   dplyr::select(lon, lat, yield_levels)
                 
               })
             }),
  tar_target(predictions_glmrs_2021_2040_raster,
             lapply(1:4, function(i){
               
               rasterFromXYZ(predictions_glmrs_2021_2040[[i]],
                             crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
             })),
  
  # calculate area of future yields raster cells
  tar_target(area_predictions_raster,
             {lapply(1:4, function(i) {
               raster::area(predictions_glmrs_2021_2040_raster[[i]],
                            na.rm=T)
             })
             }),
  # note there are 100 hectares in 1 square kilometr
  # so we can multiply yield levels in each pixel by 100 for total tons per pixel
  # aggregate to country levels (no need to apply production weight again)
  # too many countries have 0 yield data for wheat in Central America and Africa 
  # unless the NAs are crop specific and reflective of reality?
  tar_target(country_predictions_glmrs_2021_2040,
             {lapply(1:4, function(i){
               # multiply yields raster x area raster x 100
               predictions_tons <- area_predictions_raster[[i]] * predictions_glmrs_2021_2040_raster[[i]] * 100
               
               production <- exactextractr::exact_extract(
                 predictions_tons,
                 World,
                 fun='sum') 
               
               data.frame(
                 name=World$name_long,
                 iso_a2=World$iso_a2,
                 production=production
               )
             })
               
             }),
  
  
  # check baseline 2015 production against FAO data - testing this method of converting yields to production volumes
  tar_target(baseline_yields_production,
             {
               yields <- lapply(1:4, function(i){
                 rasterFromXYZ(yields_gdhy_2015_df[[i]][[1]])
               })
               
               lapply(1:4, function(i){
                 
                 production_tons <- area_predictions_raster[[i]] * yields[[i]] * 100
                 
                 production <- exactextractr::exact_extract(
                   production_tons,
                   World,
                   fun='sum') 
                 
                 data.frame(
                   name=World$name_long,
                   iso_a2=World$iso_a2,
                   production=production)
                 
               })
               
               
             }),
  
  # FAO crop allocation data
  tar_target(fao_crop_allocation_data,
             read_csv(
               "data/Food security data/FAOSTAT_data_en_2-23-2023_FBS_201820.csv")
  ),
  # calculate domestic crop use proportions
  tar_target(fao_crop_allocation_pct,
             {fao_crop_allocation_data %>% 
                 left_join(fao_crop_allocation_data %>% 
                             filter(Element == "Domestic supply quantity") %>% 
                             dplyr::select(Value,Area,Item,Year),
                           by = c("Area","Item","Year"),
                           suffix = c('','_group')) %>% 
                 mutate(Total = Value_group,
                        Prop = Value / Total)
             }),
  # calculate domestic crop use proportions averaged over 2018-2020
  tar_target(fao_crop_allocation_multiyear,
             {x <- fao_crop_allocation_data %>% 
               group_by(Area,Item, Element) %>% 
               summarise(Alloc_Total = sum(Value, na.rm=T))
             
             x %>% 
               left_join(fao_crop_allocation_data %>%
                           filter(Element == "Domestic supply quantity") %>% 
                           group_by(Area,Item) %>% 
                           summarise(Total_Supply = sum(Value, na.rm=T)),
                         by=c("Area","Item")) %>% 
               mutate(Prop = Alloc_Total/Total_Supply)
             # note that only the crop uses sum to 1 = Domestic Supply Quantity:
             # Feed, Food, Seed, Losses, Other uses, Residuals, Seed 
             # and so only those as a proportion of domestic supply quantity should be considered
             # Domestic supply quantity = Production + Import Quantity - Export Quantity - Stock Variation
             }),
  # now to calculate importing nations allocations
  # read in TM data
  tar_target(fao_trade_data,
             read_csv(
               "data/Food security data/Trade_DetailedTradeMatrix_E_All_Data.csv"
             )),
  tar_target(fao_crops,
             unique(fao_crop_allocation_data$Item)),
  tar_target(fao_trade_filtered_complex,
             fao_trade_data %>% 
               dplyr::select(1:12, Y2018, Y2019, Y2020) %>% 
               filter(Item %in% c( "Wheat", 
                                   "Wheat and meslin flour", 
                                   "Germ of wheat",
                                   "Rice", 
                                   "Bran of rice", 
                                   "Rice, broken", 
                                   "Husked rice", 
                                   "Rice, milled (Husked)", 
                                   "Rice, milled", 
                                   "Rice paddy (rice milled equivalent)", 
                                   "Maize (corn)",
                                   "Cake of maize",
                                   "Germ of maize", 
                                   "Oil of maize", 
                                   "Flour of maize",
                                   "Green corn (maize)", 
                                   "Soya beans",
                                   "Soya bean oil"),
                      Element == "Export Quantity") 
             # may need to include + products, as classified in fao_crop_allocation_data
  ),
  tar_target(fao_trade_filtered,
             fao_trade_data %>% 
               dplyr::select(1:12, Y2018, Y2019, Y2020) %>% 
               filter(Item %in% c( "Wheat", 
                                   "Rice", 
                                   "Maize (corn)",
                                   "Soya beans"),
                      Element == "Export Quantity") 
             # may need to include + products, as classified in fao_crop_allocation_data
  ),
  tar_target(concord,
             {data.frame(
               alloc_crops = fao_crops, # same order
               trade_crops = c("Wheat", "Rice", "Maize (corn)", "Soya beans")
             )}),
  # check that export quantities across FBS and TM datasets per country and crop match
  # as crop labels do not match
  tar_target(fao_trade_checks,
             {
               # sum exports by reporting country, by item and year
               trade <- fao_trade_filtered %>% 
                 group_by(`Reporter Countries`, Item) %>% 
                 summarise(sum_2018 = sum(Y2018, na.rm=T),
                           sum_2019 = sum(Y2019, na.rm=T),
                           sum_2020 = sum(Y2020, na.rm=T)) %>% 
                 pivot_longer(., cols = c("sum_2018", "sum_2019", "sum_2020"),
                              names_to = "Year", names_prefix = "sum_",
                              values_to = "Exports_trade") %>% 
                 mutate(Year=as.numeric(Year),
                        Exports_trade = Exports_trade/1000) # for comparison with alloc data
               
               concord <- data.frame(
                 alloc_crops = fao_crops, # same order
                 trade_crops = c("Wheat", "Rice", "Maize (corn)", "Soya beans")
               )
               
               alloc <- fao_crop_allocation_data %>% 
                 dplyr::select(Area, Year, Element, Item, Value) %>% 
                 filter(Element == "Export Quantity") %>% 
                 group_by(Area, Item, Year) %>% 
                 pivot_wider(., names_from = Element, values_from = Value) %>%
                 left_join(concord, by=c("Item"="alloc_crops")) %>% 
                 rename(alloc_crops=Item)
               
               trade %>% 
                 left_join(alloc, by = c("Reporter Countries"="Area",
                                         "Year",
                                         "Item"="trade_crops")) %>% 
                 mutate(dev=abs(`Export Quantity`-Exports_trade))
               
               
               # these are remarkably well matched considering trade crop categories
               # do not include 'other products'
               # so on the whole slightly lower volumes than crop allocation data
               
             }),
  
  tar_target(fao_trade_export,
             {fao_trade_filtered %>% 
                 group_by(`Reporter Countries`, `Partner Countries`, Item) %>% 
                 summarise(sum_2018 = sum(Y2018, na.rm=T),
                           sum_2019 = sum(Y2019, na.rm=T),
                           sum_2020 = sum(Y2020, na.rm=T)) %>% 
                 pivot_longer(., cols = c("sum_2018", "sum_2019", "sum_2020"),
                              names_to = "Year", names_prefix = "sum_",
                              values_to = "Exports_trade") %>% 
                 mutate(Year=as.numeric(Year),
                        Exports_trade = Exports_trade/1000) %>%
                 left_join(concord, by = c("Item"="trade_crops")) %>% 
                 left_join(
                   dplyr::select(
                     fao_crop_allocation_pct,
                     Area,
                     Element,
                     Item,
                     Year,
                     Value,  
                     Prop), 
                   # # unlike previous fao_trade_alloc this joins by exporter
                   by = c(`Reporter Countries`= "Area",
                          "alloc_crops"="Item",
                          "Year")) %>% 
                 dplyr::select(!c("Prop")) %>% 
                 filter(Element %in% c("Export Quantity", "Production")) %>% 
                 pivot_wider(names_from = Element,
                             values_from = Value)
             }),
  
  # calculate total export proportion today
  tar_target(fao_trade_future,
             {# remember that export quantity doesn't vary by partner country
               # only by reporter country (it is from fao crop allocation data)
               fao_trade_export %>% 
                 group_by(`Reporter Countries`, Item, alloc_crops, Year) %>% 
                 summarise(Exports_total = mean(`Export Quantity`, na.rm=T),
                           Production_total = mean(Production, na.rm=T),
                           Exports_trade_total = sum(Exports_trade, na.rm=T), # as this is summing across exports to all countries for an exporting country 
                           Exports.total.prop = Exports_total/Production_total) %>% 
                 # average over years
                 group_by(`Reporter Countries`, Item, alloc_crops) %>% 
                 summarise(Exports_alloc_total = sum(Exports_total, na.rm=T), # export column from alloc dataset
                           Exports_trade_total = sum(Exports_trade_total, na.rm=T), # export column from trade dataset
                           Production_total = sum(Production_total, na.rm=T), # production column from alloc dataset
                           Exports.alloc.total.prop = Exports_alloc_total/Production_total) %>% 
                 relocate(`Reporter Countries`, Item, alloc_crops, Exports.alloc.total.prop, Production_total)
             }),
  # rbindlist long - country production in tons
  tar_target(country_production_2021_2040_df,
             {country_predictions_glmrs_2021_2040 %>% 
                 rbindlist(., idcol = "crop") %>% 
                 left_join(data.frame(
                   crop = c(1:4),
                   alloc_crops = c("Maize and products",
                                   "Rice and products",
                                   "Soyabeans",
                                   "Wheat and products") # target
                 ), by = "crop")}),
  
  # left join to future yield production by Reporter Countries = Country and crops
  tar_target(fao_trade_future_exports,
             {
               fao_trade_future %>% 
                 left_join(country_production_2021_2040_df, 
                           by=c("Reporter Countries" = "name", "alloc_crops")) %>% 
                 # multiply export.prop by future production for total future exports
                 mutate(exports_future = Exports.alloc.total.prop * production)
             }),
  
  # # calculate export prop by partner
  tar_target(fao_trade_future_partners,
             {# start with trade matrix data
               x <- fao_trade_filtered %>% 
                 group_by(Item, `Reporter Countries`, `Partner Countries`) %>% 
                 pivot_longer(., cols = c("Y2018", "Y2019", "Y2020"), 
                              names_to = "Year", names_prefix = "Y",
                              values_to = "Exports_trade") %>% 
                 mutate(Year=as.numeric(Year),
                        Exports_trade = Exports_trade/1000) %>% 
                 ungroup()
               
               # calculate total exports by importer/exporter pair summed across years
               x %>% group_by(`Reporter Countries`, `Partner Countries`, Item) %>% 
                 summarise(sum_exports_partner = sum(Exports_trade, na.rm=T)) %>% 
                 # calculate / left join total exports by exporter and summed across years
                 left_join(x %>% group_by(Item, `Reporter Countries`) %>% 
                             summarise(sum_exports_total = sum(Exports_trade, na.rm=T)) %>%  # sum across importers
                             dplyr::select(Item, 
                                           `Reporter Countries`,
                                           sum_exports_total),
                           by = c("Reporter Countries","Item")) %>% 
                 
                 # calculate importer-share of total exports by importer/exporter pair (sum-years importer only /sum-years total)
                 mutate(partner_export_share = sum_exports_partner/sum_exports_total) %>% 
                 # calculate future exports by importer/exporter pair (importer-share * total future exports of each exporter)
                 left_join(dplyr::select(fao_trade_future_exports,
                                         `Reporter Countries`,
                                         Item,
                                         exports_future), 
                           by = c("Reporter Countries", "Item")) %>% 
                 mutate(partner_exports_future = exports_future * partner_export_share)   # expressed in tons
               
             }),
  # calculate future imports for each country (sum of exports across all export partners)
  
  tar_target(fao_trade_future_imports,
             fao_trade_future_partners %>% 
               group_by(`Partner Countries`, Item) %>% 
               summarise(imports_future = sum(partner_exports_future, na.rm=T))),   # expressed in tons
  # sum future production and imports
  tar_target(fao_future_food_supply,
             {fao_trade_future_imports %>% 
                 left_join(dplyr::select(
                   fao_trade_future_exports,
                   `Reporter Countries`,
                   Item,
                   production
                   
                 ), by = c("Partner Countries"="Reporter Countries", "Item")) %>% 
                 rowwise() %>% 
                 mutate(total_supply = sum(imports_future, production, na.rm=T)) %>% 
                 left_join(concord, by = c("Item"="trade_crops")) %>% 
                 
                 # apply crop allocation percentages
                 left_join(dplyr::select(
                   fao_crop_allocation_multiyear,
                   Area,
                   Item,
                   Element, 
                   Prop
                 ), by = c("Partner Countries"="Area", "alloc_crops"="Item")) %>%  
                 
                 filter(Element %in% c(
                   "Feed",
                   "Food",
                   "Losses",
                   "Other uses (non-food)",
                   "Residuals",
                   "Seed",
                   "Tourist consumption",
                   "Processing"
                 ))
               
             } 
             
  ),
  # calculate food use in tons 
  # convert to food calories
  # calculate feed allocation in tons
  # convert feed calories to food calories
  tar_target(fao_animal_production_data,
             read_csv("data/Food security data/FAOSTAT_data_en_3-13-2023_FBS_feed_products.csv")),
  
  tar_target(fao_animal_production_feed,
             {  # sum production per product over the years
               country_item_production <- fao_animal_production_data %>%
                 group_by(Area, Item) %>% 
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
             {
               df <- fao_animal_production_feed %>% 
                 left_join(dplyr::select(world_bank_country_list,
                                         Economy,
                                         `Income group`),
                           by=c("Area"="Economy")) %>% 
                 left_join(grazing_income_group,
                           by=c("Income group", "Item")) %>%
                 left_join(livestock_feed_conversion,
                           by=c("Item")) %>% 
                 # re-account for grazing via adjusted proportions
                 mutate(Sum_adj = case_when(
                   !is.na(grazing_prop) ~ Sum_Value*(1-grazing_prop),
                   is.na(grazing_prop) ~ Sum_Value)
                 ) 
                 # summarise across items as a separate table, then left-join back to df by country
                 country_total_production <- df %>% 
                   group_by(Area) %>% 
                   summarise(Total_Value = sum(Sum_adj, na.rm=T))
                 # mutate proportion column
                 df %>% 
                   left_join(country_total_production, by = c("Area")) %>% 
                   mutate(Prop = Sum_adj/Total_Value)
             }),
  
  # # calculate weighted average feed-to-calories conversion factor by country
  tar_target(fao_country_feed_calories,
             {
               fao_animal_product_feed_calories %>%
                 group_by(Area) %>% 
                 summarise(feed_conversion = weighted.mean(
                   conversion, Prop
                 ))
             }),
  # create tribble of food to calories conversion from Cassidy et al. 2013 for 4 crops
  tar_target(crop_calorie_conversion,
             {tribble(
               ~Item, ~crop_conversion,
               "Maize (corn)", 3580802.60,
               "Rice", 2800000.00,
               "Soya beans", 3596499.11,
               "Wheat", 3284000.00
             )})
  # left join feed and food calorie conversion back to fao_future_food_supply
)
# tar_target(fao_trade_export_prop_partner,
#            {
#              fao_trade_export %>%  
#                group_by(`Reporter Countries`, `Partner Countries`, Item, alloc_crops) %>%
#                # average across 2018-20
#                summarise(Production_present = sum(Production, na.rm=T),
#                          Exports_present = sum(exports_trade, na.rm=T), 
#                          # ^ **** note that we are using exports_trade as this DOES vary by partner countries
#                          # it may be a problem that exports_trade =/= Export Quantity
#                          Export.Prop = Exports_present/Production_present)
#            })
# left join to future yield production for future exports by partner countries
# i.e. future imports
# tar_target(fao_trade_checks_complex,
#            {
#              # sum exports by reporting country, by item and year
#              trade <- fao_trade_filtered_complex %>% 
#                group_by(`Reporter Countries`, Item) %>% 
#                summarise(sum_2018 = sum(Y2018, na.rm=T),
#                          sum_2019 = sum(Y2019, na.rm=T),
#                          sum_2020 = sum(Y2020, na.rm=T)) %>% 
#                pivot_longer(., cols = c("sum_2018", "sum_2019", "sum_2020"),
#                             names_to = "Year", names_prefix = "sum_",
#                             values_to = "Exports_trade") %>% 
#                mutate(Year=as.numeric(Year),
#                       Exports_trade = Exports_trade/1000) # for comparison with alloc data
#              
#              # now try all other products in trade matrix
#              concord_complex <- tribble(~trade_crops, ~alloc_crops,
#                                         "Wheat", "Wheat and products", 
#                                         "Wheat and meslin flour", "Wheat and products",
#                                         "Germ of wheat", "Wheat and products",
#                                         "Rice", "Rice and products",
#                                         "Bran of rice", "Rice and products",
#                                         "Rice, broken", "Rice and products",
#                                         "Husked rice", "Rice and products",
#                                         "Rice, milled (Husked)", "Rice and products",
#                                         "Rice, milled", "Rice and products",
#                                         "Rice paddy (rice milled equivalent)", "Rice and products",
#                                         "Maize (corn)", "Maize and products",
#                                         "Cake of maize", "Maize and products",
#                                         "Germ of maize", "Maize and products",
#                                         "Oil of maize", "Maize and products",
#                                         "Flour of maize", "Maize and products",
#                                         "Green corn (maize)", "Maize and products",
#                                         "Soya beans", "Soyabeans",
#                                         "Soya bean oil", "Soyabeans")
#              
#              alloc <- fao_crop_allocation_data %>% 
#                dplyr::select(Area, Year, Element, Item, Value) %>% 
#                filter(Element == "Export Quantity") %>% 
#                group_by(Area, Item, Year) %>% 
#                pivot_wider(., names_from = Element, values_from = Value) %>%
#                rename(alloc_crops=Item)
#              
#              trade %>% 
#                left_join(concord_complex,
#                          by = c("Item"="trade_crops")) %>% 
#                group_by(`Reporter Countries`, Year, alloc_crops) %>% 
#                summarise(Exports_trade = sum(Exports_trade, na.rm=T)) %>% 
#                left_join(alloc, by = c("Reporter Countries"="Area",
#                                        "Year",
#                                        "alloc_crops")) %>% 
#                # for comparability with fao_trade_checks
#                relocate(`Reporter Countries`, alloc_crops, Year) %>% 
#                arrange(`Reporter Countries`, alloc_crops) %>% 
#                mutate(dev=abs(`Export Quantity`-Exports_trade))
#              
#            }),
# tar_read(fao_trade_checks_complex) %>% ungroup() %>% summarise(sum=sum(dev, na.rm=T))
# 37392
# tar_read(fao_trade_checks) %>% ungroup() %>% summarise(sum=sum(dev, na.rm=T))
# 29640
# this suggests that sticking to the basic categories is more 'accurate'

# create a dataset copy of TM datasets, for each TM exporting country of crop c, 
# identify the importing countries as df and 
# join each of these dfs (nested tibble) to FBS 
# so that each TM importing country crop allocation use is identified 
# and then calculate the weighted mean   
# tar_target(fao_trade_alloc,
#            {
#              fao_trade_filtered %>% 
#                group_by(`Reporter Countries`, `Partner Countries`, Item) %>% 
#                summarise(sum_2018 = sum(Y2018, na.rm=T),
#                          sum_2019 = sum(Y2019, na.rm=T),
#                          sum_2020 = sum(Y2020, na.rm=T)) %>% 
#                pivot_longer(., cols = c("sum_2018", "sum_2019", "sum_2020"),
#                             names_to = "Year", names_prefix = "sum_",
#                             values_to = "Exports_trade") %>% 
#                mutate(Year=as.numeric(Year),
#                       Exports_trade = Exports_trade/1000) %>%
#                left_join(concord, by = c("Item"="trade_crops")) %>% 
#                left_join(
#                  dplyr::select(
#                    fao_crop_allocation_pct,
#                    Area,
#                    Element,
#                    Item,
#                    Year,
#                    Value,
#                    Prop), by = c(`Partner Countries`= "Area",
#                                  "alloc_crops"="Item",
#                                  "Year")
#                )
#              
#            }),
# tar_target(fao_trade_export_alloc,
#            {
#              x <- fao_trade_alloc %>% 
#                group_by(`Reporter Countries`, Item, alloc_crops, Year, Element) %>%
#                # weighted by value imported by each importer country
#                summarise(sum = sum(Value, na.rm=T)) 
#                
#                x %>% left_join(x %>% 
#                            filter(Element == "Domestic supply quantity"),
#                          by = c("Reporter Countries","Item","alloc_crops", "Year"),
#                          suffix = c('','_group')) %>% 
#                mutate(Prop = sum / sum_group)
#                
#            }),
# so now we have, for each exporting country, item and year, 
# the average allocations of its trade partner countries
# filter these for feed, food, losses, other, processing, seed, residuals, tourist consumption
# i.e. filter out Domestic supply, Export quantity, Import Quantity, Production, Stock Variation

# ^ ignore above



