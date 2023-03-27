
rasterise_pop_10km_file <- function(file){# somehow terra::rast results in Error: external pointer is not valid
  
  all <- raster::stack(pop_10km_file) 
  # need only 2021-2040
  near <- raster::subset(all, 11:30)
  # average population over 2021-2040
  mean <- calc(near, fun = mean)
  outfile <- "processed/pop_10km_data.tif"
  writeRaster(mean, outfile, overwrite=TRUE)
  outfile
  
}

calc_pop <- function(file){
  r <- raster::raster(file) 
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

calc_country_mder <- function(mder_data, pop_data){
  mder_data %>% 
    filter(Year==2020) %>% 
    left_join(pop_data, by = c("Code"="iso_a3")) %>% 
    dplyr::select(!c("Year")) %>% 
    rename(mder_daily_2020 = 3) %>% 
    mutate(mder_annual_2020 = mder_daily_2020 * 365.25,
           mder_pop_daily_2021_2040 = mder_daily_2020 * pop_2021_2040,
           mder_pop_annual_2021_2040 = mder_annual_2020 * pop_2021_2040)
}

format_predictions <- function(predictions){
  df <- adj_predictions_by_time_period %>% 
    filter(model=="glm_RS" & time_period=="2021-2040") %>% 
    dplyr::select(c("lon","lat","pred_bar", "crop_pooled")) 
  
  df <- split(df, df$crop_pooled)
  
  lapply(1:4, function(i){
    df[[i]] %>% dplyr::select(!c("crop_pooled"))
  })
  
  
}

calc_baseline_production <- function(hectares_data, yields_data, World)     {
  
  lapply(1:4, function(i){
    
    production_tons <- hectares_data[[i]] * yields_data[[i]] * 1000 # for tons
    
    production <- exactextractr::exact_extract(
      production_tons,
      World,
      fun='sum') 
    
    data.frame(
      name=World$name_long,
      iso_a2=World$iso_a2,
      production=production)
    
  })
  
  
}

read_iso2_correspondance <- function(file){
  
  data <- read_csv(file) 
  data %>% 
    mutate(iso2=ifelse(iso2=="C_", "CN", iso2),
           iso2=ifelse(name=="Namibia", "NA", iso2),
           iso2=ifelse(name=="China", "C_", iso2),
           name=ifelse(iso2=="GB", "United Kingdom of Great Britain and Northern Ireland", name)
    )
  
}

check_baseline_production <- function(fao_production, grogan_production){
  fao <- fao_production %>%  
    dplyr::select(Area, Item, Value, Year) %>% # units are in tons
    group_by(Area, Item) %>% 
    summarise(Value_fao = mean(Value, na.rm=T)) %>% # average over 2014-2016 for mean production centred around 2015
    arrange(Item) %>%  # sort crops alphabetically
    left_join(fao_iso2, by = c("Area"="name"))
  
  fao <- split(fao, fao$Item) # so that list will be matched by crop index
  
  # World and FAO country names do not match; e.g. United States
  lapply(1:4, function(i){
    grogan_production[[i]] %>% 
      rename(Value_calc = production) %>% 
      left_join(fao[[i]],
                by=c("iso_a2"="iso2")
      ) %>% 
      mutate(Diff = round((Value_calc - Value_fao)/Value_fao,2)) %>% 
      relocate(name, iso_a2, Diff)
  })
}

calc_future_yields <- function(yields_data, predictions){
  lapply(1:4, function(i){
    
    yield <- raster::aggregate(yields_data[[i]], 
                               c(0.5, 0.5)/res(yields_data), 
                               mean)
    
    pred_r <- rasterFromXYZ(predictions[[i]],
                            crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    # future yield = (1 + yield change/100) * baseline yield
    # total production per pixel = yield in tons/hectare x number of hectares 
    (1+pred_r/100) * yield # * hectares * 1000
    
  })
}

extract_country_production_predictions <- function(predictions, World){
  lapply(1:4, function(i){
    
    production <- exactextractr::exact_extract(
      predictions[[i]],
      World,
      fun='sum') 
    
    data.frame(
      name=World$name_long,
      iso_a2=World$iso_a2,
      production=production
    )
  })
  
}

compare_baseline_future_production <- function(predictions, worldmap_clean, fao_production_comparison) {
  lapply(1:4, function(i){
    
    predictions[[i]] %>% 
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
      
      left_join(dplyr::select(
        as.data.frame(worldmap_clean),
        ISO_A2, ISO_A3), by=c("iso_a2"="ISO_A2"))
  })
}

calc_crop_allocation <- function(data){
  data %>% 
    left_join(data %>% 
                filter(Element == "Domestic supply quantity") %>% 
                dplyr::select(Value,Area,Item,Year),
              by = c("Area","Item","Year"),
              suffix = c('','_group')) %>% 
    mutate(Total = Value_group,
           Prop = Value / Total)
}

calc_multiyear_crop_allocation <- function(data, fao_iso2){
  x <- data %>% 
    group_by(Area,Item, Element) %>% 
    summarise(Alloc_Total = sum(Value, na.rm=T))
  
  x %>% 
    left_join(data %>%
                filter(Element == "Domestic supply quantity") %>% 
                group_by(Area,Item) %>% 
                summarise(Total_Supply = sum(Value, na.rm=T)),
              by=c("Area","Item")) %>% 
    mutate(Prop = Alloc_Total/Total_Supply) %>% 
    left_join(fao_iso2, by=c("Area"="name"))
  # note that only the crop uses sum to 1 = Domestic Supply Quantity:
  # Feed, Food, Seed, Losses, Other uses, Residuals, Seed 
  # and so only those as a proportion of domestic supply quantity should be considered
  # Domestic supply quantity = Production + Import Quantity - Export Quantity - Stock Variation
}

check_fbs_tm_data <- function(trade_data, fao_crops, fbs_data, concord, alloc){
  {
    # sum exports by reporting country, by item and year
    trade <- trade_data %>% 
      group_by(`Reporter Countries`, Item, Year) %>% 
      summarise(Exports_trade=sum(Value,na.rm=T)) %>% 
      mutate(
        Exports_trade = Exports_trade/1000) # for comparison with alloc data
    
    concord <- data.frame(
      alloc_crops = fao_crops, # same order
      trade_crops = c("Wheat", "Rice", "Maize (corn)", "Soya beans")
    )
    
    alloc <- fbs_data %>% 
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
    
  }
}

process_export_data <- function(trade_data, concord, fbs_data){
  trade_data %>% 
    filter(Element=="Export Quantity") %>% 
    dplyr::select(`Reporter Countries`, `Partner Countries`, Item, Year, Value, `Reporter Country Code (M49)`) %>%
    mutate(Exports_trade = Value/1000) %>%
    dplyr::select(!c("Value")) %>% 
    left_join(concord, by = c("Item"="trade_crops")) %>% 
    left_join(
      dplyr::select(
        fbs_data,
        Area,
        `Area Code (M49)`,
        Element,
        Item,
        Year,
        Value,  
        Prop), 
      # # unlike previous fao_trade_alloc this joins by exporter
      by = c("Reporter Country Code (M49)" = "Area Code (M49)",
             "alloc_crops"="Item",
             "Year")) %>% 
    dplyr::select(!c("Prop")) %>% 
    filter(Element %in% c("Export Quantity", "Production")) %>% 
    pivot_wider(names_from = Element,
                values_from = Value)
}

calc_fao_export_share <- function(fao_trade_export){# remember that export quantity doesn't vary by partner country
  # only by reporter country (it is from fao crop allocation data)
  fao_trade_export %>% 
    group_by(`Reporter Countries`, Item, alloc_crops, Year) %>%  # summarise across partner countries
    summarise(Exports_total = sum(`Export Quantity`, na.rm=T), # mean before
              Production_total = sum(Production, na.rm=T), # mean before
              #  Exports_trade_total = sum(Exports_trade, na.rm=T), # as this is summing across exports to all countries for an exporting country 
              Exports.total.prop = Exports_total/Production_total) %>% 
    # average over years
    group_by(`Reporter Countries`, Item, alloc_crops) %>% 
    summarise(Exports_alloc_total = sum(Exports_total, na.rm=T), # export column from alloc dataset
              #  Exports_trade_total = sum(Exports_trade_total, na.rm=T), # export column from trade dataset
              Production_total = sum(Production_total, na.rm=T), # production column from alloc dataset
              Exports.alloc.total.prop = Exports_alloc_total/Production_total) %>% 
    relocate(`Reporter Countries`, Item, alloc_crops, Exports.alloc.total.prop, Production_total)
}

rbind_country_production_predictions <- function(predictions){
  predictions %>% 
    rbindlist(., idcol = "crop") %>% 
    left_join(data.frame(
      crop = c(1:4),
      alloc_crops = c("Maize and products",
                      "Rice and products",
                      "Soyabeans",
                      "Wheat and products") # target
    ), by = "crop")}

calc_exports <- function(fao_export_share, 
                         fao_iso2, 
                         future_production, 
                         crops, 
                         baseline_production){
  fao_export_share %>% 
    left_join(fao_iso2,by=c("Reporter Countries"="name")) %>% 
    left_join(future_production, 
              by=c("iso2" = "iso_a2", "alloc_crops")) %>% 
    rename(crop_no=crop) %>% 
    left_join(data.frame(crop_no=c(1:4),crop=crops)) %>% 
    # add baseline exports as well to calculate baseline imports
    left_join(dplyr::select(baseline_production, # using GAEZ production data
                            iso_a2,
                            crop,
                            production_2015_gaez,
                            production_2015_fao  # check export calc methodology using FAO production data
    ),
    by=c("iso2" = "iso_a2", "crop")) %>% 
    # check export calc methodology using FAO production data
    
    # multiply export.prop by future production for total future exports
    mutate(exports_future = Exports.alloc.total.prop * production,
           exports_baseline = Exports.alloc.total.prop * production_2015_gaez,
           exports_fao = Exports.alloc.total.prop * production_2015_fao)
}

calc_export_partner_share <- function(trade_data, fao_trade_exports){# start with trade matrix data
  x <- trade_data %>% 
    filter(Element=="Export Quantity") %>% 
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
    left_join(dplyr::select(fao_trade_exports,
                            `Reporter Countries`,
                            Item,
                            exports_future,
                            exports_baseline,
                            exports_fao), 
              by = c("Reporter Countries", "Item")) %>% 
    mutate(partner_exports_future = exports_future * partner_export_share,
           partner_exports_baseline = exports_baseline * partner_export_share,
           partner_exports_fao = exports_fao * partner_export_share)   # expressed in tons
  
}

calc_imports <- function(fao_trade_partners, fao_iso2){
  fao_trade_partners %>% 
    group_by(`Partner Countries`, Item) %>% 
    summarise(imports_future = sum(partner_exports_future, na.rm=T),
              imports_baseline = sum(partner_exports_baseline, na.rm=T),
              imports_fao = sum(partner_exports_fao, na.rm=T)) %>% 
    left_join(fao_iso2, by=c("Partner Countries"="name"))
}

check_baseline_import_export <- function(trade_data, est_imports, est_exports, concord){
  trade_data %>% 
    left_join(concord, by=c("Item"="alloc_crops")) %>% 
    left_join(dplyr::select(est_imports, iso2, imports_baseline, imports_fao, Item), by=c("iso2", "trade_crops"="Item")) %>% 
    left_join(dplyr::select(est_exports, iso2, exports_baseline, exports_fao, alloc_crops), by=c("iso2", "Item"="alloc_crops"))
  # actuals are imports_2015 and exports_2015, estimates are imports_baseline and exports_baseline
  # they should be near identical!
}

calc_future_food_supply <- function(est_imports, 
                                    est_exports, 
                                    concord, 
                                    fao_crop_allocation_multiyear){
  est_imports %>% 
    left_join(dplyr::select(
      est_exports,
      `Reporter Countries`,
      Item,
      production,
      exports_future
      
    ), by = c("Partner Countries"="Reporter Countries", "Item")) %>% 
    mutate(production=ifelse(
      is.na(production) | is.infinite(production), 0, production),
      imports_future=ifelse(
        is.na(imports_future) | is.infinite(imports_future), 0, imports_future),
      exports_future=ifelse(
        is.na(exports_future) | is.infinite(exports_future), 0, exports_future)) %>% 
    rowwise() %>% 
    mutate(total_supply = production + imports_future - exports_future) %>% 
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

process_animal_feed_data <- function(animal_production_data, 
                                     country_income_data, 
                                     grazing_income_group, 
                                     livestock_feed_conversion){
  df <- animal_production_data %>% 
    left_join(dplyr::select(country_income_data,
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
}

calc_total_future_calories <- function(fao_future_food_supply,
                                       fao_country_feed_calories,
                                       crop_calorie_conversion){
  fao_future_food_supply %>% 
    filter(Element %in% c("Feed", "Food")) %>% 
    left_join(fao_country_feed_calories, by=c("Partner Countries"="Area")) %>% 
    left_join(crop_calorie_conversion, by=c("Item", "alloc_crops")) %>% 
    # pivot_longer(., c("crop_conversion", "feed_conversion"), 
    #              names_to = "conversion_element", 
    #              values_to= "conversion_factor")
    mutate(food_calories=total_supply*Prop*crop_conversion,
           feed_calories=total_supply*Prop*feed_conversion,
           total_calories=food_calories+feed_calories)
}

calc_future_food_gap <- function(future_total_calories,
                                 fao_iso2,
                                 worldmap_clean,
                                 country_pop_mder){
  future_total_calories %>% 
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
}

calc_staple_share_calories <- function(fao_all_products_kcal,
                                       world_bank_country_list,
                                       grazing_income_group,
                                       fao_iso2){
  data <- fao_all_products_kcal %>% 
    left_join(dplyr::select(world_bank_country_list,
                            Economy,
                            `Income group`),
              by=c("Area"="Economy")) %>% 
    left_join(grazing_income_group,
              by=c("Income group","Item")) %>% 
    mutate(Value_adj = case_when(
      !is.na(grazing_prop) ~ Value*(1-grazing_prop),
      is.na(grazing_prop) ~ Value
    )) %>% 
    dplyr::select(Area, Item, Element, Year, Value_adj) %>% 
    group_by(Area,Item) %>% 
    # sum across years
    summarise(Value_adj=sum(Value_adj,na.rm=T)) %>% 
    mutate(sum_total_items=sum(Value_adj, na.rm=T)) %>% 
    # then filter for only the products of interest
    filter(Item %in% c(
      "Maize and products",
      "Rice and products",
      "Soyabeans",
      "Wheat and products",
      "Bovine Meat",
      "Pigmeat",
      "Poultry Meat",
      "Eggs",
      "Milk - Excluding Butter"
    )) %>%
    # summarise new column sum of these products
    # then mutate another column to calculate the share for each country
    mutate(sum_staple_items = sum(Value_adj, na.rm=T)) %>%
    summarise(sum_staple_items=sum(sum_staple_items),
              sum_total_items=sum(sum_total_items,na.rm=T),
              staple_item_share=sum_staple_items/sum_total_items) %>%
    left_join(fao_iso2,by=c("Area"="name"))
  
  # check global staple share, should be close to 0.64 from Tilman 2011
  # data %>% 
  #                  summarise(sum_staple_items=sum(sum_staple_items),
  #                            sum_total_items=sum(sum_total_items),
  #                            staple_item_share=sum_staple_items/sum_total_items)
  # 0.571, this is closer to 0.64; could be lower because we look at 186 countries
  # rather than 100 countries as in Tilman
  # and the staple share may be lower in the remaining 86 countries?
  
  data
  
}

check_est_fao_total_calories <- function(fao_all_products_kcal,
                                         world_bank_country_list,
                                         grazing_income_group,
                                         baseline_2015_total_calories,
                                         fao_iso2){
  data <- fao_all_products_kcal %>% 
  left_join(dplyr::select(world_bank_country_list,
                          Economy,
                          `Income group`),
            by=c("Area"="Economy")) %>% 
  left_join(grazing_income_group,
            by=c("Income group","Item")) %>% 
  mutate(Value_adj = case_when(
    !is.na(grazing_prop) ~ Value*(1-grazing_prop),
    is.na(grazing_prop) ~ Value
  )) %>% 
  dplyr::select(Area, Item, Element, Year, Value_adj) %>% 
  group_by(Area,Item) %>% 
  # sum across years
  summarise(Value_adj=mean(Value_adj,na.rm=T)) %>% 
  group_by(Area) %>% 
  summarise(sum_total_items=sum(Value_adj, na.rm=T)) %>% 
  left_join(fao_iso2, by=c("Area"="name"))


data %>% 
  # sum_total_items
  left_join(dplyr::select(
    baseline_2015_total_calories,
    iso_a2,
    calories_supply_total_gaez),
    by=c("iso2"="iso_a2")) %>% 
  mutate(fao_total_calories=sum_total_items*10^6) %>% 
  dplyr::select(Area,iso2, fao_total_calories, calories_supply_total_gaez)

}

check_est_fao_calorie_gap <- function(comparison_2015_calories_supply,
                                      worldmap_clean,
                                      country_pop_mder_2015,
                                      outfile){
  data <- comparison_2015_calories_supply %>% 
  left_join(dplyr::select(
    as.data.frame(worldmap_clean),
    ISO_A2, ISO_A3), by=c("iso2"="ISO_A2")) %>% 
  left_join(dplyr::select(
    country_pop_mder_2015, 
    Code, mder_pop_annual_2015, mder_annual_2015, pop_2015), 
    by = c("ISO_A3"="Code")) %>% 
  rename(calories_demand=mder_pop_annual_2015) %>% 
  mutate( 
    food_insecure_persons_fao = (calories_demand - fao_total_calories)/mder_annual_2015,
    food_insecure_persons_gaez = (calories_demand - calories_supply_total_gaez)/mder_annual_2015,
    pou_rate_fao = food_insecure_persons_fao/pop_2015,
    pou_rate_gaez = food_insecure_persons_gaez/pop_2015) %>% 
  filter(ISO_A3 != "Ashm") %>% 
  dplyr::select(Area, iso2, ISO_A3, fao_total_calories, calories_supply_total_gaez, calories_demand, pou_rate_fao, pou_rate_gaez) 

data %>% write_csv(outfile)
data
}

calc_future_calorie_gap <- function(future_food_gap,
                                    country_pop_mder,
                                    fao_staple_share){
  future_food_gap %>% 
    left_join(dplyr::select(country_pop_mder, Code, mder_annual_2020, pop_2021_2040), 
              by = c("ISO_A3"="Code")) %>% 
    left_join(dplyr::select(fao_staple_share, iso2, staple_item_share),
              by=c("iso2")) %>% # this line is new
    mutate(
      calories_supply_total = calories_supply / staple_item_share, # this line is new
      #calories_supply_total = calories_supply/0.7,
      food_insecure_persons = (calories_demand - calories_supply_total)/mder_annual_2020,
      pou_rate = food_insecure_persons/pop_2021_2040) %>% 
    filter(ISO_A3 != "Ashm")
  
}

rbind_country_baseline_future_production_by_crop <- function(country_baseline_future_production,
                                                             crops){
  rbindlist(country_baseline_future_production,
            idcol="crop_no") %>% 
    left_join(data.frame(crop_no=c(1:4),
                         crop=crops), by="crop_no") %>% 
    dplyr::select(!c("crop_no"))
}

aggregate_country_baseline_future_production <- function(country_baseline_future_production_df){
  country_baseline_future_production_df %>% 
    group_by(name,iso_a2,ISO_A3) %>% 
    summarise(total_production_2021_2040=sum(production_2021_2040,na.rm=T),
              total_production_2015_gaez=sum(production_2015_gaez,na.rm=T),
              total_production_2015_fao=sum(production_2015_fao,na.rm=T))
}

process_fao_import_export_2015_data <- function(fbs_data, fao_iso2){
  fbs_data %>%
    filter(Element %in% c("Import Quantity",
                          "Export Quantity")) %>%
    group_by(Area, Item, Element) %>% # summarise across years
    summarise(mean_value = mean(Value, na.rm=T)*1000) %>% # expressed in tons
    pivot_wider(., names_from=Element, values_from=mean_value) %>%
    rename(exports_2015 = `Export Quantity`,
           imports_2015 = `Import Quantity`) %>%
    left_join(fao_iso2, by=c("Area"="name"))
}

calc_baseline_calories_by_crop <- function(country_baseline_future_production_df,
                                           alloc_crops,
                                           fao_crop_allocation_multiyear,
                                           fao_country_feed_calories,
                                           crop_calorie_conversion,
                                           est_imports,
                                           est_exports){
  country_baseline_future_production_df %>% 
    left_join(data.frame(crop=crops, alloc_crops=c("Maize and products",
                                                   "Rice and products",
                                                   "Soyabeans",
                                                   "Wheat and products"))) %>% 
    
    left_join(dplyr::select(fao_crop_allocation_multiyear, Area, iso2, Item, Element, Prop), 
              by=c("iso_a2"="iso2", "alloc_crops"="Item")) %>% 
    filter(Element %in% c("Feed", "Food")) %>% 
    left_join(dplyr::select(fao_country_feed_calories, feed_conversion, iso2), by=c("iso_a2"="iso2")) %>% 
    left_join(crop_calorie_conversion, by=c("alloc_crops")) %>% # need to left join import quantities in 2015 from FAO
    # left_join(fao_import_export_2015,
    #           by=c("iso_a2"="iso2", "alloc_crops"="Item")) %>% 
    left_join(dplyr::select(est_imports,
                            iso2,
                            Item,
                            imports_baseline),
              
              by=c("iso_a2"="iso2", "Item")) %>% # we have lost 177-160 = 17 countries here?
    rename(imports_2015 = imports_baseline) %>% 
    left_join(dplyr::select(est_exports,
                            iso2,
                            Item,
                            exports_baseline),
              by=c("iso_a2"="iso2", "Item")) %>% 
    rename(exports_2015 = exports_baseline) %>% 
    
    dplyr::select(!c("Area", "alloc_crops", "pct_change", "production_2021_2040")) %>% 
    mutate(imports_2015 = ifelse(
      is.na(imports_2015) | is.infinite(imports_2015), 0, imports_2015),
      production_2015_gaez = ifelse(
        is.na(production_2015_gaez) | is.infinite(production_2015_gaez), 0, production_2015_gaez
      ),
      exports_2015 = ifelse(
        is.na(exports_2015) | is.infinite(exports_2015), 0, exports_2015
      )
    ) %>% 
    rowwise() %>% 
    # is it appropriate to multiply non-crop-specific feed conversion factor by crop-specific supply?
    # yes, because feed is not differentiated by feed type, only by animal type
    mutate(
      #gaez
      total_supply_gaez = production_2015_gaez + imports_2015 - exports_2015,
      # #gaez
      food_calories_gaez=total_supply_gaez*Prop*crop_conversion,
      feed_calories_gaez=total_supply_gaez*Prop*feed_conversion,
      total_calories_gaez=food_calories_gaez+feed_calories_gaez#,
    ) 
}

calc_pop_2015 <- function(data,
                          fao_iso2,
                          worldmap_clean){
  data %>% 
    group_by(Area) %>% 
    summarise(Value=mean(Value,na.rm=T)) %>% 
    mutate(pop_2015=Value*1000) %>% 
    left_join(fao_iso2, by=c("Area"="name")) %>% 
    left_join(dplyr::select(
      as.data.frame(worldmap_clean),
      ISO_A2, ISO_A3), by=c("iso2"="ISO_A2"))
  }

calc_country_pop_mder_2015 <- function(mder_data,
                                       fao_population_2015){
  mder_data %>% 
    filter(Year==2015) %>% 
    left_join(fao_population_2015, by = c("Code"="ISO_A3")) %>% 
    dplyr::select(!c("Year")) %>% 
    rename(mder_daily_2015 = 3) %>% 
    mutate(mder_annual_2015 = mder_daily_2015 * 365.25,
           mder_pop_daily_2015 = mder_daily_2015 * pop_2015,
           mder_pop_annual_2015 = mder_annual_2015 * pop_2015)
} # 194-177=17 countries will not have population information

calc_baseline_calorie_gap <- function(baseline_2015_calorie_gap_by_crop,
                                      country_pop_mder_2015,
                                      fao_staple_share){# only 145 countries
  baseline_2015_calorie_gap_by_crop %>% 
    group_by(name,ISO_A3,iso_a2) %>% 
    summarise(total_calories_gaez=sum(total_calories_gaez,na.rm=T)#,
              # total_calories_fao=sum(total_calories_fao,na.rm=T)
    ) %>% 
    left_join(dplyr::select(
      country_pop_mder_2015, 
      Code, mder_pop_annual_2015, mder_annual_2015, pop_2015), 
      by = c("ISO_A3"="Code")) %>% 
    rename(calories_demand=mder_pop_annual_2015) %>% 
    left_join(dplyr::select(fao_staple_share, iso2, staple_item_share),
              by=c("iso_a2"="iso2")) %>% # this line is new
    mutate( 
      #calories_supply_total_gaez = total_calories_gaez/0.7,
      #       calories_supply_total_fao = total_calories_fao/0.7,
      calories_supply_total_gaez = total_calories_gaez/staple_item_share, # /0.7
      # calories_supply_total_fao = total_calories_fao/staple_item_share, # /0.7
      food_insecure_persons_gaez = (calories_demand - calories_supply_total_gaez)/mder_annual_2015,
      pou_rate_gaez = food_insecure_persons_gaez/pop_2015#,
      # food_insecure_persons_fao = (calories_demand - calories_supply_total_fao)/mder_annual_2015,
      # pou_rate_fao = food_insecure_persons_fao/pop_2015
    ) %>% 
    filter(ISO_A3 != "Ashm")
}

calc_calorie_gap_change <- function(future_2030_calorie_gap,
                                    baseline_2015_calorie_gap,
                                    outfile){
  future <- future_2030_calorie_gap %>% 
  dplyr::select(c("Country", "iso2","ISO_A3","calories_demand","calories_supply_total","pou_rate")) %>% 
  rename(calories_demand_2030=calories_demand,
         calories_supply_total_2030=calories_supply_total,
         pou_rate_2030 = pou_rate) %>% 
  mutate(FI_status_2030 = ifelse(pou_rate_2030 < 0, 0, 1)) # 1=insecure

baseline <- baseline_2015_calorie_gap %>% 
  dplyr::select(c("name", "ISO_A3", "calories_supply_total_gaez", "calories_demand", "pou_rate_gaez")) %>% 
  rename(Country=name,
         calories_supply_total_2015 = calories_supply_total_gaez,
         calories_demand_2015 = calories_demand,
         pou_rate_2015 = pou_rate_gaez) %>% 
  mutate(FI_status_2015 = ifelse(pou_rate_2015 < 0, 0, 1)) # 1=insecure

data <- baseline %>% left_join(future,
                               by=c("ISO_A3")) %>% 
  mutate(FI_status_change = case_when(FI_status_2015==0 & FI_status_2030==1 ~ "become insecure",
                                      FI_status_2015==0 & FI_status_2030==0 ~ "remain secure",
                                      FI_status_2015==1 & FI_status_2030==1 ~ "remain insecure",
                                      FI_status_2015==1 & FI_status_2030==0 ~ "become secure"))

data %>% write_csv(outfile)

data


}

plot_FI_status_change <- function(calorie_gap_change,
                                  World,
                                  outfile){
  data <- calorie_gap_change %>% 
    dplyr::select(iso2, ISO_A3, FI_status_change) 
  
  dat <- World %>% left_join(data, by=c("iso_a2"="iso2"))
  
  plot <- tmap::tm_shape(dat) +
    tm_fill("FI_status_change",
            palette = c("orange", "red3","darkgreen","black"),
            #palette=c("orange","lightgreen","red3","darkgreen"),
            title= '2015 to 2021-2040') +
    tmap::tm_shape(World) +
    tmap::tm_borders("grey", lwd =1) 
  
  tmap::tmap_save(plot, filename=outfile, height=4, width=10, asp=0)
  plot
}

plot_pou_rate_change_capped <- function(calorie_gap_change,
                                        World,
                                        outfile){
  data <- calorie_gap_change %>% 
  # disregard food excess supply (net exporting countries) as the focus is on food security
  # the minimum pou rate is 0
  mutate(pou_rate_2015=ifelse(pou_rate_2015<0, 0, pou_rate_2015),
         pou_rate_2030=ifelse(pou_rate_2030<0, 0, pou_rate_2030),
         pou_rate_change=pou_rate_2030-pou_rate_2015)
  # for uncapped, mutate (pou_rate_change=(pou_rate_2030-pou_rate_2015)/pou_rate_2015)

dat <- World %>% left_join(data, by=c("iso_a2"="iso2"))

plot <- tmap::tm_shape(dat) +
  tm_fill("pou_rate_change",
          palette=c("yellowgreen","lightyellow","khaki1","orange","red3"), 
          
          #   palette=c("darkgreen", "yellowgreen","lightyellow","khaki1","orange","red3", "brown"), 
          breaks=c(-1.0,-0.8,-0.6,-0.4,-0.2, 0, 0.2,0.4,0.6,0.8, 1.0),
          midpoint=0,
          title= '2015 to 2021-2040') +
  tmap::tm_shape(World) +
  tmap::tm_borders("grey", lwd =1) 

tmap::tmap_save(plot, filename=outfile, height=4, width=10, asp=0)
plot
# 
}

decompose_calorie_gap_change <- function(future_2030_calorie_gap,
                                         baseline_2015_calorie_gap,
                                         comparison_2015_calorie_gap){
  future <- future_2030_calorie_gap %>% 
    dplyr::select(c("Country", "ISO_A3","calories_demand","calories_supply_total",
                    "mder_annual_2020","pop_2021_2040")) %>% 
    rename(calories_demand_2030=calories_demand,
           calories_supply_total_2030=calories_supply_total) 
  
  baseline <- baseline_2015_calorie_gap %>% 
    dplyr::select(c("ISO_A3", "calories_supply_total_gaez", "calories_demand",
                    "mder_annual_2015","pop_2015")) %>% 
    rename(calories_supply_total_2015 = calories_supply_total_gaez,
           calories_demand_2015 = calories_demand) 
  
  data <- baseline %>% left_join(future,
                                 by=c("ISO_A3")) %>% 
    left_join(dplyr::select(comparison_2015_calorie_gap,
                            Area,
                            ISO_A3,
                            fao_total_calories),
              by=c("ISO_A3")) %>% 
    rowwise() %>% 
    mutate(change_demand_calories = calories_demand_2030-calories_demand_2015,
           change_supply_calories = calories_supply_total_2030-calories_supply_total_2015,
           calorie_gap_2030 = calories_demand_2030-calories_supply_total_2030, # gap = negative
           calorie_gap_2015 = calories_demand_2015-calories_supply_total_2015, # excess supply = positive
           calorie_gap_2015_fao = calories_demand_2015-fao_total_calories) 
  # how many and which countries to plot this for?
  # those who were secure and now become insecure?
  # all of those that are insecure?
  
}

plot_pou_change_decomposed_become_insecure <- function(decomposed_change_in_calories,
                                                       outfile,
                                                       countries_becoming_insecure){
  data <- decomposed_change_in_calories %>% 
    dplyr::select(!c("name")) %>% 
    mutate(pou_gap_2030=calorie_gap_2030/mder_annual_2020/pop_2021_2040,
           pou_gap_2015=calorie_gap_2015/mder_annual_2015/pop_2015,
           pou_gap_2015_fao=calorie_gap_2015_fao/mder_annual_2015/pop_2015,
           change_demand_pou=change_demand_calories/mder_annual_2020/pop_2021_2040, # instead of 2015/2015?
           change_supply_pou=change_supply_calories/mder_annual_2020/pop_2021_2040) %>% 
    filter(
      ISO_A3 %in% countries_becoming_insecure$ISO_A3) %>% # outlier that is skewing the bars 
    pivot_longer(.,
                 c("pou_gap_2030",
                   "pou_gap_2015",
                   "pou_gap_2015_fao",
                   "change_demand_pou",
                   "change_supply_pou",),
                 names_to="measure",
                 values_to="PoU") 
  
  data$measure <- factor(data$measure,
                         levels=c("pou_gap_2015_fao",
                                  "pou_gap_2015",
                                  "pou_gap_2030",
                                  "change_demand_pou",
                                  "change_supply_pou"))
  
  lbls=c(
    "Demand - Supply FAO, 2015",
    "Demand - Supply, 2015",
    "Demand - Supply, 2030",
    "Change in demand",
    "Change in supply"
  )
  
  plot <- ggplot(data %>% 
                   dplyr::select(c(ISO_A3, measure,PoU)), 
                 aes(measure, PoU,fill=measure))+
    geom_bar(stat="identity")+
    facet_wrap(~ISO_A3) +
    theme(axis.text.x = element_blank(),
          legend.position = "right")+
    scale_fill_discrete(
      limits=c("pou_gap_2015_fao",
               "pou_gap_2015",
               "pou_gap_2030",
               "change_demand_pou",
               "change_supply_pou"),
      labels=str_wrap(lbls, 10),
      name="Measure"
    ) +
    scale_y_continuous(name="Persons / Population")
  
  plot
  ggsave(outfile)
  plot
}

plot_calorie_gap_decomposed_become_insecure <- function(decomposed_change_in_calories,
                                                        outfile,
                                                        countries_becoming_insecure){
  data <- decomposed_change_in_calories %>% 
    dplyr::select(!c("name","calories_supply_total_2015","calories_demand_2015")) %>% 
    filter(
      ISO_A3 %in% countries_becoming_insecure$ISO_A3) %>% # outlier that is skewing the bars 
    pivot_longer(.,
                 c("calorie_gap_2030",
                   "calorie_gap_2015",
                   "change_demand_calories",
                   "change_supply_calories",
                   "calories_demand_2030",
                   "calories_supply_total_2030"),
                 names_to="measure",
                 values_to="calories") %>% 
    mutate(calories_billions=calories/10^9) %>% 
    filter(!measure %in% c("calories_demand_2030",
                           "calories_supply_total_2030"))
  
  
  plot <- ggplot(data %>% 
                   dplyr::select(c(ISO_A3, measure,calories_billions)), 
                 aes(measure, calories_billions,fill=measure))+
    geom_bar(stat="identity")+
    facet_wrap(~ISO_A3) +
    theme(axis.text.x = element_blank())+
    scale_fill_discrete(
      limits=c("calorie_gap_2015",
               "calorie_gap_2030",
               "change_demand_calories",
               "change_supply_calories"),
      labels=c(
        "Demand - Supply, 2015",
        "Demand - Supply, 2030",
        "Change in demand (popn.)",
        "Change in supply (prodn.)"
      ),
      name="Measure"
    ) +
    scale_y_continuous(name="Calories (billions)")
  
  plot
  ggsave(outfile)
  plot
}

plot_persons_change_decomposed_become_insecure <- function(decomposed_change_in_calories,
                                                           countries_becoming_insecure,
                                                           outfile){
  data <- decomposed_change_in_calories %>% 
    dplyr::select(!c("name")) %>% 
    mutate(pou_gap_2030=calorie_gap_2030/mder_annual_2020,
           pou_gap_2015=calorie_gap_2015/mder_annual_2015,
           change_demand_pou=change_demand_calories/mder_annual_2020, # instead of 2015/2015?
           change_supply_pou=change_supply_calories/mder_annual_2020) %>% 
    filter(
      ISO_A3 %in% countries_becoming_insecure$ISO_A3) %>% # outlier that is skewing the bars 
    pivot_longer(.,
                 c("pou_gap_2030",
                   "pou_gap_2015",
                   "change_demand_pou",
                   "change_supply_pou",),
                 names_to="measure",
                 values_to="persons") %>% 
    mutate(persons_millions = persons/10^6)
  
  data$measure <- factor(data$measure,
                         levels=c("pou_gap_2015",
                                  "pou_gap_2030",
                                  "change_demand_pou",
                                  "change_supply_pou"))
  
  lbls=c(
    "Demand - Supply, 2015",
    "Demand - Supply, 2030",
    "Change in demand",
    "Change in supply"
  )
  
  ggplot(data %>% 
           dplyr::select(c(ISO_A3, measure,persons_millions)), 
         aes(measure, persons_millions,fill=measure))+
    geom_bar(stat="identity")+
    facet_wrap(~ISO_A3) +
    theme(axis.text.x = element_blank(),
          legend.position = "right")+
    scale_fill_discrete(
      limits=c("pou_gap_2015",
               "pou_gap_2030",
               "change_demand_pou",
               "change_supply_pou"),
      labels=str_wrap(lbls, 10),
      name="Measure"
    ) +
    scale_y_continuous(name="Persons (millions)")
  
  
  ggsave(outfile)
  
}

plot_pou_change_decomposed_remain_insecure <- function(decomposed_change_in_calories,
                                                       countries_remaining_insecure,
                                                       outfile){
  data <- decomposed_change_in_calories %>% 
    dplyr::select(!c("name")) %>% 
    mutate(pou_gap_2030=calorie_gap_2030/mder_annual_2020/pop_2021_2040,
           pou_gap_2015=calorie_gap_2015/mder_annual_2015/pop_2015,
           change_demand_pou=change_demand_calories/mder_annual_2020/pop_2021_2040, # instead of 2015/2015?
           change_supply_pou=change_supply_calories/mder_annual_2020/pop_2021_2040) %>% 
    filter(
      ISO_A3 %in% countries_remaining_insecure$ISO_A3) %>% # outlier that is skewing the bars 
    pivot_longer(.,
                 c("pou_gap_2030",
                   "pou_gap_2015",
                   "change_demand_pou",
                   "change_supply_pou",),
                 names_to="measure",
                 values_to="PoU") 
  
  data$measure <- factor(data$measure,
                         levels=c("pou_gap_2015",
                                  "pou_gap_2030",
                                  "change_demand_pou",
                                  "change_supply_pou"))
  
  lbls=c(
    "Demand - Supply, 2015",
    "Demand - Supply, 2030",
    "Change in demand",
    "Change in supply"
  )
  
  ggplot(data %>% 
           dplyr::select(c(ISO_A3, measure,PoU)), 
         aes(measure, PoU,fill=measure))+
    geom_bar(stat="identity")+
    facet_wrap(~ISO_A3) +
    theme(axis.text.x = element_blank(),
          legend.position = "right")+
    scale_fill_discrete(
      limits=c("pou_gap_2015",
               "pou_gap_2030",
               "change_demand_pou",
               "change_supply_pou"),
      labels=str_wrap(lbls, 10),
      name="Measure"
    ) +
    scale_y_continuous(name="Persons / Population")
  
  
  ggsave(outfile)
  
}