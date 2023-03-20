
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
  

# future population calorie demand ----------------------------------------


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
  # tar_target(yields_gdhy_2015_df,
  #            {
  #              lapply(1:4, function(j){
  #                
  #                lapply(35, function(i) {
  #                  
  #                  crops_lwr <- c("maize", "rice", "soybean", "wheat")
  #                  
  #                  crop_yields_ts <- list.files(here("data", "GDHY data", crops_lwr[[j]]), pattern = "^.*\\.(nc4)$")
  #                  
  #                  data <- nc_open(here("data", "GDHY data", crops_lwr[[j]], crop_yields_ts[[i]]))
  #                  
  #                  lon <- ncvar_get(data, "lon")
  #                  
  #                  lon[lon > 180] <- lon[lon > 180] - 360
  #                  
  #                  lat <- ncvar_get(data, "lat")
  #                  yields <- ncvar_get(data, "var")
  #                  
  #                  fillvalue <- ncatt_get(data, "var", "_FillValue")
  #                  
  #                  yields[yields == fillvalue$value] <- NA
  #                  
  #                  # set dimension names and values to corresponding lon and lat values
  #                  dimnames(yields) <- list(lon = lon, lat = lat)
  #                  
  #                  as.data.frame.table(yields, responseName = "yields") %>% 
  #                    mutate(lon=as.character(lon),
  #                           lon=as.numeric(lon),
  #                           lat=as.character(lat),
  #                           lat=as.numeric(lat))
  #                  
  #                  
  #                })
  #              })
  #            }
  # ),

# future production of food supply ----------------------------------------


  # adj_predictions_by_time_period but filter for glm rs and 2021-2040, remove model_spec column
  # then split by crop 
  # call yield change predictions in 2021-2040 and multiply by 2015 yield levels
  tar_target(predictions_glmrs_2021_2040,
             {
               df <- adj_predictions_by_time_period %>% 
                 filter(model=="glm_RS" & time_period=="2021-2040") %>% 
                 dplyr::select(c("lon","lat","pred_bar", "crop_pooled")) 
               
               df <- split(df, df$crop_pooled)
               
               lapply(1:4, function(i){
                 df[[i]] %>% dplyr::select(!c("crop_pooled"))
               })
               
               # lapply(1:4, function(i) 
               # {
               #   df <- df[[i]] %>% 
               #     left_join(
               #       yields_gdhy_2015_df[[i]][[1]], by = c("lon", "lat")
               #     ) %>% 
               #     mutate(yield_levels = yields * (1+pred_bar/100)) %>% 
               #     dplyr::select(lon, lat, yield_levels)
               #   
               # })
             }),

  # calculate area of future yields raster cells
  # tar_target(area_predictions_raster,
  #            {lapply(1:4, function(i) {
  #              raster::area(predictions_glmrs_2021_2040_raster[[i]],
  #                           na.rm=T)
  #            })
  #            }),
  # note there are 100 hectares in 1 square kilometr
  # so we can multiply yield levels in each pixel by 100 for total tons per pixel
  # aggregate to country levels (no need to apply production weight again)
  # too many countries have 0 yield data for wheat in Central America and Africa 
  # unless the NAs are crop specific and reflective of reality?
  # tar_target(monfreda_hectares_data,
  #            c("data/Monfreda data/maize_HarvestedAreaHectares.tif",
  #              "data/Monfreda data/rice_HarvestedAreaHectares.tif",
  #              "data/Monfreda data/soybean_HarvestedAreaHectares.tif",
  #              "data/Monfreda data/wheat_HarvestedAreaHectares.tif"),
  #            format="file"
  #            ),
  # tar_target(monfreda_hectares_raster,
  #            { # calculate total number of hectares per pixel
  #              r <- raster::stack(monfreda_hectares_data)
  #              raster::aggregate(r, c(0.5,0.5)/res(r), sum)
  #            }
  #            ),
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

# checks ------------------------------------------------------------------

 
  
  
  # check baseline 2015 production against FAO data - testing this method of converting yields to production volumes
  tar_target(baseline_yields_production,
             {
               
               lapply(1:4, function(i){
                 
                 production_tons <- grogan_hectares_raster[[i]] * grogan_yield_raster[[i]] * 1000 # for tons
                 
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
  # get FAO 2015 production data for comparison (actually averaged 2014-2016 like in Grogan et al. 2022)
  tar_target(fao_production_2015_data,
             read_csv("data/Food security data/FAOSTAT_data_en_3-16-2023_production_201416.csv")),
  # get FAO to ISO_A2 correspondance
  tar_target(fao_iso2,
             {data <- read_csv("data/Food security data/iso2.csv") 
             data %>% 
               mutate(iso2=ifelse(iso2=="C_", "CN", iso2),
                      iso2=ifelse(name=="Namibia", "NA", iso2),
                      iso2=ifelse(name=="China", "C_", iso2),
                      name=ifelse(iso2=="GB", "United Kingdom of Great Britain and Northern Ireland", name)
                      )
             
             }),
  
  # compare with FAO data - 
  tar_target(fao_production_comparison,
             {
               fao <- fao_production_2015_data %>%  
                 dplyr::select(Area, Item, Value, Year) %>% # units are in tons
                 group_by(Area, Item) %>% 
                 summarise(Value_fao = mean(Value, na.rm=T)) %>% # average over 2014-2016 for mean production centred around 2015
                 arrange(Item) %>%  # sort crops alphabetically
                 left_join(fao_iso2, by = c("Area"="name"))
               
               fao <- split(fao, fao$Item) # so that list will be matched by crop index
               
               # World and FAO country names do not match; e.g. United States
               lapply(1:4, function(i){
                 baseline_yields_production[[i]] %>% 
                   rename(Value_calc = production) %>% 
                   left_join(fao[[i]],
                             by=c("iso_a2"="iso2")
                   ) %>% 
                   mutate(Diff = round((Value_calc - Value_fao)/Value_fao,2)) %>% 
                   relocate(name, iso_a2, Diff)
               })
             }),
  tar_target(global_comparison_check, # note this takes grogan GAEZ data at 5 arcminute resolution
             {lapply(1:4, function(i) {
               fao_production_comparison[[i]] %>% 
                 summarise(sum_production_calc=sum(Value_calc, na.rm=T),
                           sum_production_fao=sum(Value_fao, na.rm=T)) %>% 
                 mutate(calc_fao_prop = sum_production_calc/sum_production_fao)
             })
             }
             
  ),

# estimate projected production -------------------------------------------


  ## estimate projected production relative to production in 2015 from GAEZ data
  
  tar_target(yields_glmrs_2021_2040_raster,
             lapply(1:4, function(i){
               
               yield <- raster::aggregate(grogan_yield_raster[[i]], 
                                          c(0.5, 0.5)/res(grogan_yield_raster), 
                                          mean)
             
               pred_r <- rasterFromXYZ(predictions_glmrs_2021_2040[[i]],
                                       crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
              
               # future yield = (1 + yield change/100) * baseline yield
               # total production per pixel = yield in tons/hectare x number of hectares 
               (1+pred_r/100) * yield # * hectares * 1000
               
             })),
 # resample yields raster to be ahigher resolution
tar_target(yields_resampled_raster,
           lapply(1:4, function(i){
             resample(yields_glmrs_2021_2040_raster[[i]], grogan_hectares_raster[[i]], method="bilinear")
           })),

  # tar_target(grogan_hectares_raster_0.5, {
  #   lapply(1:4, function(i){
  #     raster::aggregate(grogan_hectares_raster[[i]], 
  #                       c(0.5,0.5)/res(grogan_hectares_raster),
  #                       sum)
  #   })
  # }),
  tar_target(predictions_glmrs_2021_2040_raster,
             {
               lapply(1:4, function(i){
                 
               
                 yields_resampled_raster[[i]] * grogan_hectares_raster[[i]] * 1000 
               })
             }),
  
  tar_target(country_predictions_glmrs_2021_2040,
             {lapply(1:4, function(i){
              
               production <- exactextractr::exact_extract(
                 predictions_glmrs_2021_2040_raster[[i]],
                 World,
                 fun='sum') 
               
               data.frame(
                 name=World$name_long,
                 iso_a2=World$iso_a2,
                 production=production
               )
             })
               
             }),

# compare baseline and future production in tons
  tar_target(country_baseline_future_production,
             {
               lapply(1:4, function(i){
                 
                 country_predictions_glmrs_2021_2040[[i]] %>% 
                   left_join(dplyr::select(fao_production_comparison[[i]],
                                           name,
                                           iso_a2,
                                           Value_calc,
                                           Value_fao),
                             by=c("name", "iso_a2")) %>% 
                   rename(production_2015_gaez=Value_calc,
                          production_2015_fao=Value_fao,
                          production_2021_2040=production) %>% 
                   mutate(pct_change=round((production_2021_2040-production_2015_gaez)/production_2015_gaez,2)) %>% 
                   dplyr::select(!c("iso_a2", "production_2015_fao"))
               })
             }),


# crop allocation ---------------------------------------------------------


  
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

# future exports and imports -----------------------------------------------------------------


  tar_target(fao_trade_filtered,
             read_csv(
               "data/Food security data/FAOSTAT_data_en_3-20-2023_filtered_trade_matrix.csv"
             )),
  tar_target(fao_crops,
             unique(fao_crop_allocation_data$Item)),
  # tar_target(fao_trade_filtered_complex,
  #            fao_trade_data %>% 
  #              dplyr::select(1:12, Y2018, Y2019, Y2020) %>% 
  #              filter(Item %in% c( "Wheat", 
  #                                  "Wheat and meslin flour", 
  #                                  "Germ of wheat",
  #                                  "Rice", 
  #                                  "Bran of rice", 
  #                                  "Rice, broken", 
  #                                  "Husked rice", 
  #                                  "Rice, milled (Husked)", 
  #                                  "Rice, milled", 
  #                                  "Rice paddy (rice milled equivalent)", 
  #                                  "Maize (corn)",
  #                                  "Cake of maize",
  #                                  "Germ of maize", 
  #                                  "Oil of maize", 
  #                                  "Flour of maize",
  #                                  "Green corn (maize)", 
  #                                  "Soya beans",
  #                                  "Soya bean oil"),
  #                     Element == "Export Quantity") 
  #            # may need to include + products, as classified in fao_crop_allocation_data
  # ),
  # tar_target(fao_trade_filtered,
  #            fao_trade_data %>% 
  #              dplyr::select(1:12, Y2018, Y2019, Y2020) %>% 
  #              filter(Item %in% c( "Wheat", 
  #                                  "Rice", 
  #                                  "Maize (corn)",
  #                                  "Soya beans"),
  #                     Element == "Export Quantity") 
  #            # may need to include + products, as classified in fao_crop_allocation_data
  # ),
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
                 group_by(`Reporter Countries`, Item, Year) %>% 
                 summarise(Exports_trade=sum(Value,na.rm=T)) %>% 
                 mutate(
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
                 dplyr::select(`Reporter Countries`, `Partner Countries`, Item, Year, Value) %>% 
                 mutate(Exports_trade = Value/1000) %>%
                 dplyr::select(!c("Value")) %>% 
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
                 
                 mutate(
                        Exports_trade = Value/1000) %>% 
                 dplyr::select(!c("Value")) %>% 
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
                 mutate(total_supply = case_when(
                   !is.infinite(imports_future) & !is.infinite(production) ~ sum(imports_future, production, na.rm=T),
                   !is.infinite(imports_future) & is.infinite(production) ~ imports_future,
                   is.infinite(imports_future) & !is.infinite(production) ~ production)
                   ) %>% 
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


# convert future production to calories -----------------------------------


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
               ~Item, ~crop_conversion, ~alloc_crops,
               "Maize (corn)", 3580802.60, "Maize and products",
               "Rice", 2800000.00, "Rice and products",
               "Soya beans", 3596499.11, "Soyabeans",
               "Wheat", 3284000.00, "Wheat and products"
             )}),
  # left join feed and food calorie conversion back to fao_future_food_supply
  tar_target(future_food_calories,
             {fao_future_food_supply %>% 
                 filter(Element %in% c("Feed", "Food")) %>% 
                 left_join(fao_country_feed_calories, by=c("Partner Countries"="Area")) %>% 
                 left_join(crop_calorie_conversion, by=c("Item", "alloc_crops")) %>% 
                 # pivot_longer(., c("crop_conversion", "feed_conversion"), 
                 #              names_to = "conversion_element", 
                 #              values_to= "conversion_factor")
                mutate(food_calories=total_supply*Prop*crop_conversion,
                       feed_calories=total_supply*Prop*feed_conversion,
                       total_calories=food_calories+feed_calories) %>% 
                 group_by(`Partner Countries`) %>% 
                 summarise(total_calories=sum(total_calories, na.rm=T))
               }),
# join with country_pop_mder for country annual mder shortfall
  tar_target(future_food_gap,
             {future_food_calories %>% 
                 left_join(fao_iso2, by=c("Partner Countries"="name")) %>% 
                 left_join(dplyr::select(
                   as.data.frame(worldmap_clean),
                 ISO_A2, ISO_A3), by=c("iso2"="ISO_A2")) %>% 
                 left_join(dplyr::select(country_pop_mder,
                                         Entity,
                                         Code,
                                         mder_pop_annual_2021_2040), 
                           by=c("ISO_A3"="Code")) %>% 
                 rename(calories_supply=total_calories,
                        calories_demand=mder_pop_annual_2021_2040) %>% 
                 mutate(
                        calorie_gap_prop = calories_demand/calories_supply,
                        calorie_gap_prop = round(calorie_gap_prop,2)) %>% 
                 dplyr::select(!c("Entity")) %>% 
                 rename(Country=`Partner Countries`)
               }),
tar_target(global_food_gap,
           {future_food_gap %>% 
               summarise(total_calorie_supply = sum(calories_supply, na.rm=T),
                         total_calorie_demand = sum(calories_demand, na.rm=T),
                         calorie_gap = total_calorie_demand/total_calorie_supply)})
# also break down the difference between current production & future production
# and attribution to population growth vs production change - waterfall graph?
)