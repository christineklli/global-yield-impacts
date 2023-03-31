
rasterise_pop_10km_file <- function(file, 
                                    outfile){# somehow terra::rast results in Error: external pointer is not valid
  
  all <- raster::stack(file) 
  # need only 2021-2040
  pop_2021_2040 <- raster::subset(all, 12:31)
  # average population over 2021-2040
  pop_2030 <- calc(pop_2021_2040, fun = mean)
  pop_2041_2060 <- raster::subset(all, 32:51)
  pop_2050 <- calc(pop_2041_2060, fun = mean)
  pop_2061_2080 <- raster::subset(all, 52:71)
  pop_2070 <- calc(pop_2061_2080, fun = mean)
  pop_2081_2100 <- raster::subset(all, 72:91)
  pop_2090 <- calc(pop_2081_2100, fun = mean)
  
  pop_all <- raster::stack(pop_2030, pop_2050, pop_2070, pop_2090)
  writeRaster(pop_all, outfile, overwrite=TRUE)
  outfile
  
}

calc_pop <- function(file){
  r <- raster::stack(file) 
  worldmap <- cleangeo::clgeo_Clean(rworldmap::countriesCoarse)
  df <- exactextractr::exact_extract(
    r,
    worldmap,
    fun='sum'
  )
  df %>% as_tibble() %>% 
    rename(`2021-2040`=1,
           `2041-2060`=2,
           `2061-2080`=3,
           `2081-2100`=4) %>% 
    cbind(
      iso_a3=worldmap$ISO_A3,
      name=worldmap$NAME
    ) %>% 
    pivot_longer(., c("2021-2040", 
                      "2041-2060",
                      "2061-2080",
                      "2081-2100"),
                 names_to="time_period",
                 values_to="pop_future")
}

calc_country_mder <- function(mder_data, pop_data){

  
  mder_data %>% 
    filter(Year==2020) %>% 
    left_join(pop_data, by = c("Code"="iso_a3")) %>% 
    dplyr::select(!c("Year")) %>% 
    rename(mder_daily_2020 = 3) %>% 
    mutate(mder_annual_2020 = mder_daily_2020 * 365.25,
           #mder_pop_daily_2030 = mder_daily_2020 * pop_2030,
           mder_pop_annual_future = mder_annual_2020 * pop_future)
}

format_predictions <- function(predictions){
  df <- predictions %>% 
    filter(model=="glm_RS" & time_period %in% c("2021-2040",
                                                "2041-2060",
                                                "2061-2080",
                                                "2081-2100")) %>% 
    dplyr::select(c("lon","lat","pred_bar", "crop_pooled", "time_period")) 
  
  # split into three level nested list
  
  df <- split(df, df$time_period)
  
  lapply(1:4, function(i){
    df <- split(df[[i]], df[[i]]$crop_pooled)
    lapply(1:4, function(j){df[[j]] %>% dplyr::select(!c("crop_pooled", "time_period"))
      })
      
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
    dplyr::select(Area, `Area Code (ISO2)`, Item, `Item Code (CPC)`, Value, Year) %>% # units are in 1000 tons
    group_by(Area, `Area Code (ISO2)`, Item, `Item Code (CPC)`) %>% 
    summarise(Value_fao = mean(Value, na.rm=T)) %>% # average over 2014-2016 for mean production centred around 2015
    mutate(Value_fao=Value_fao*1000) %>%  # units in tons
    arrange(Item)  # sort crops alphabetically

  
  fao <- split(fao, fao$Item) # so that list will be matched by crop index
  
  # World and FAO country names do not match; e.g. United States
  lapply(1:4, function(i){
    grogan_production[[i]] %>% 
      rename(Value_calc = production) %>% 
      left_join(fao[[i]],
                by=c("iso_a2"="Area Code (ISO2)")
      ) %>% 
      mutate(Diff = round((Value_calc - Value_fao)/Value_fao,2)) %>% 
      relocate(name, iso_a2, Diff)
  }) 
}

calc_future_yields <- function(yields_data, predictions){
  lapply(1:4, function(time_period){
    lapply(1:4, function(crop){
       yield <- raster::aggregate(yields_data[[crop]], 
                               c(0.5, 0.5)/res(yields_data), 
                               mean)
    
    pred_r <- rasterFromXYZ(predictions[[time_period]][[crop]],
                            crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    # future yield = (1 + yield change/100) * baseline yield
    # total production per pixel = yield in tons/hectare x number of hectares 
    (1+pred_r/100) * yield # * hectares * 1000
    
  })
    })
    
   
}

extract_country_production_predictions <- function(predictions, World){
  lapply(1:4, function(time_period){
    lapply(1:4, function(crop){
    # extend extent of World to be consistent with predictions (warning message was received)
    #st_bbox(World) <- st_bbox(c(xmin=-180, ymin=-90, xmax=180, ymax=90))
    
    production <- exactextractr::exact_extract(
      predictions[[time_period]][[crop]],
      World,
      fun='sum') 
    
    data.frame(
      name=World$name_long,
      iso_a2=World$iso_a2,
      production=production
    )
  })
  })
  
  
}

compare_baseline_future_production <- function(predictions, worldmap_clean, fao_production_2015_check) {
  lapply(1:4, function(time_period){
     lapply(1:4, function(crop){
    
    predictions[[time_period]][[crop]] %>% 
      left_join(dplyr::select(fao_production_2015_check[[crop]],
                              name,
                              iso_a2,
                              Value_calc,
                              Value_fao),
                by=c("name", "iso_a2")) %>% 
      rename(production_2015_gaez=Value_calc,
             production_2015_fao=Value_fao,
             production_future=production) %>% 
      mutate(pct_change=round((production_future-production_2015_gaez)/production_2015_gaez,2)) %>% 
      
      left_join(dplyr::select(
        as.data.frame(worldmap_clean),
        ISO_A2, ISO_A3), by=c("iso_a2"="ISO_A2"))
  })
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

calc_multiyear_crop_allocation <- function(data){
  x <- data %>% 
    group_by(Area, `Area Code (ISO2)`, Item, `Item Code (CPC)`, Element) %>% 
    summarise(Alloc_Total = sum(Value, na.rm=T))
  
  x %>% 
    left_join(data %>%
                filter(Element == "Domestic supply quantity") %>% 
                group_by(Area,Item) %>% 
                summarise(Total_Supply = sum(Value, na.rm=T)),
              by=c("Area","Item")) %>% 
    mutate(Prop = Alloc_Total/Total_Supply) 
  # note that only the crop uses sum to 1 = Domestic Supply Quantity:
  # Feed, Food, Seed, Losses, Other uses, Residuals, Seed 
  # and so only those as a proportion of domestic supply quantity should be considered
  # Domestic supply quantity = Production + Import Quantity - Export Quantity - Stock Variation
}

check_fao_trade_rice <- function(data){
  data %>% 
    filter(`Reporter Countries`=="Japan" & `Item Code (FAO)` %in% c(32, 28, 29, 30, 31, 27) & Element == "Export Quantity")
  # note that 30 is actually Rice, paddy (rice milled equivalent)
  # and 27 though labelled just 'Rice' is labelled 'Rice, paddy' in the official correspondance 
  # quantities for 30 are quite large, whereas quantities for 27 are very low
  # code 30 is missing from the official correspondance and default composition definition
  # hypothesise that 30 could be a total figure of all the other rices
  # (we've been using just 27 rice (actually rice paddy) so far! - introducing trade biases for sure)
  data %>% 
    filter(`Reporter Countries`=="Japan" & `Item Code (FAO)` %in% c(30) & Element == "Export Quantity" & Year=="2016") %>% 
    dplyr::select(`Partner Countries`, Value, `Item Code (FAO)`)
  # these are identical^
  data %>% 
    filter(`Reporter Countries`=="Japan" & `Item Code (FAO)` %in% c(32, 28, 29, 31, 27) & Element == "Export Quantity") %>% 
    group_by(`Partner Countries`, Year) %>%  
    summarise(Value=sum(Value)) %>%
    filter(Year==2016) %>% 
    print(n=Inf)
  
}

check_fbs_tm_data <- function(fao_export_data, fbs_data, concord, outfile){
  {
    # sum exports by reporting country, by item and year
    trade <- fao_export_data %>% left_join(concord, by=c("Item Code (FAO)")) %>% 
      group_by(`Reporter Countries`, Year, `Reporter Country Code (ISO2)`, `Item Code (CPC)`) %>% 

      summarise(Exports_trade=sum(Value,na.rm=T)) %>% 
      mutate(
        Exports_trade = Exports_trade/1000) # for comparison with alloc data
  
    
    alloc <- fbs_data %>% 
      dplyr::select(Area, `Area Code (ISO2)`, Year, Element, Item, Value, `Item Code (CPC)`) %>% 
      filter(Element == "Export Quantity") %>% 
      group_by(Area, `Area Code (ISO2)`, Item, `Item Code (CPC)`, Year) %>% 
      pivot_wider(., names_from = Element, values_from = Value) 
    
    x <- trade %>% 
      
      left_join(alloc, by = c("Reporter Country Code (ISO2)"="Area Code (ISO2)",
                              "Year",
                              "Item Code (CPC)")) %>% 
      # summarise across years
      group_by(`Reporter Countries`, `Item Code (CPC)`) %>% 
      summarise(Export_FBS=sum(`Export Quantity`, na.rm=T),
                Export_trade = sum(Exports_trade, na.rm=T)) %>% 
      mutate(dev=abs(Export_FBS-Export_trade))
    
    x %>% write_csv(outfile)
    
    x
    
    
    # these are remarkably well matched considering trade crop categories
    # do not include 'other products'
    # so on the whole slightly lower volumes than crop allocation data
    
  }
}

process_export_data <- function(fao_export_data, concord, fbs_data){

  export <- fao_export_data %>% 
    dplyr::select(`Reporter Countries`, `Partner Countries`, Item, Year, Value, `Reporter Country Code (ISO2)`, `Item Code (FAO)`) %>%
    mutate(Exports_trade = Value/1000) %>%
    dplyr::select(!c("Value")) %>% 
    left_join(concord, by = c("Item Code (FAO)")) %>% 
    group_by(`Reporter Countries`, `Reporter Country Code (ISO2)`, Year, `Item Code (CPC)`) %>% 
    summarise(Exports_trade = sum(Exports_trade, na.rm=T)) 
  
  
  fbs <-  fbs_data %>% dplyr::select(`Area Code (ISO2)`,
                                     Element,
                                     Item,
                                     `Item Code (CPC)`,
                                     Year,
                                     Value,  
                                     Prop)
  
  fbs %>% 
    left_join(export, 
      # # unlike previous fao_trade_alloc this joins by exporter
      by = c("Area Code (ISO2)"="Reporter Country Code (ISO2)",
             "Item Code (CPC)",
             "Year")) %>% 
    rename("Reporter Country Code (ISO2)"="Area Code (ISO2)") %>% 
    dplyr::select(!c("Prop")) %>% 
    filter(Element %in% c("Export Quantity", "Production")) %>% 
    pivot_wider(names_from = Element,
                values_from = Value)
}

calc_fao_export_share <- function(fao_trade_export){# remember that export quantity doesn't vary by partner country
  # only by reporter country (it is from fao crop allocation data)
  fao_trade_export %>% 
    group_by(`Reporter Countries`, `Reporter Country Code (ISO2)`, Item, `Item Code (CPC)`, Year) %>%  # summarise across partner countries
    summarise(Exports_total = sum(`Export Quantity`, na.rm=T), # mean before
              Production_total = sum(Production, na.rm=T), # mean before
              #  Exports_trade_total = sum(Exports_trade, na.rm=T), # as this is summing across exports to all countries for an exporting country 
              Exports.total.prop = Exports_total/Production_total) %>% 
    # average over years
    group_by(`Reporter Countries`, `Reporter Country Code (ISO2)`, Item, `Item Code (CPC)`) %>% 
    summarise(Exports_alloc_total = sum(Exports_total, na.rm=T), # export column from alloc dataset
              #  Exports_trade_total = sum(Exports_trade_total, na.rm=T), # export column from trade dataset
              Production_total = sum(Production_total, na.rm=T), # production column from alloc dataset
              Exports.alloc.total.prop = Exports_alloc_total/Production_total) 
}

rbind_country_production_predictions <- function(predictions){
  l <- lapply(1:4, function(time_period){
    rbindlist(predictions[[time_period]], idcol="crop")
    
  })
   
    rbindlist(l, idcol = "time") %>% 
    left_join(data.frame(
      crop = c(1:4),
      Item = c("Maize and products",
                      "Rice and products",
                      "Soyabeans",
                      "Wheat and products") # target
    ), by = "crop") %>% 
      left_join(data.frame(
        time=c(1:4),
        time_period=c("2021-2040","2041-2060","2061-2080","2081-2100")
      ), by = "time")
  
  
  }

rbind_country_baseline_future_production_by_crop <- function(country_baseline_future_production,
                                                             crops){
  
  l <- lapply(1:4, function(time_period){
    rbindlist(country_baseline_future_production[[time_period]], idcol="crop_no")
    
  })
  
  rbindlist(l, idcol = "time") %>% 
    left_join(data.frame(crop_no=c(1:4),
                         crop=crops), by="crop_no") %>% 
    left_join(data.frame(
      time=c(1:4),
      time_period=c("2021-2040","2041-2060","2061-2080","2081-2100")
    ), by = "time") %>% 
    dplyr::select(!c("crop_no", "time"))
}

aggregate_country_baseline_future_production <- function(country_baseline_future_production_df){
  country_baseline_future_production_df %>% 
    group_by(name,iso_a2,ISO_A3,time_period, crop) %>% 
    summarise(total_production_future=mean(production_future,na.rm=T),# actually unique
              total_production_2015_gaez=mean(production_2015_gaez,na.rm=T), # not unique across 'future years'
              total_production_2015_fao=mean(production_2015_fao,na.rm=T)) %>%  # not unique across 'future years'
    ungroup() %>% 
    group_by(name, iso_a2, ISO_A3, time_period) %>% 
    summarise(total_production_future=sum(total_production_future, na.rm=T),
              total_production_2015_gaez=sum(total_production_2015_gaez,na.rm=T),
              total_production_2015_fao=sum(total_production_2015_fao,na.rm=T))
}


calc_exports <- function(fao_export_share, 
                         future_production, 
                         crops, 
                         baseline_production){
  future_production %>% 
    left_join(fao_export_share, 
              by=c("iso_a2"="Reporter Country Code (ISO2)", "Item")) %>%
    rename(crop_no=crop,
           "Reporter Country Code (ISO2)"=iso_a2) %>% 
    left_join(data.frame(crop_no=c(1:4),crop=crops)) %>% 
    # add baseline exports as well to calculate baseline imports
    left_join(baseline_production %>% 
                filter(time_period=="2021-2040") %>% # just to avoid repeat rows 
                         dplyr::select( # using GAEZ production data
                            iso_a2,
                            crop,
                            production_2015_gaez,
                            production_2015_fao  # check export calc methodology using FAO production data
    ),
    by=c("Reporter Country Code (ISO2)" = "iso_a2", "crop")) %>% 
    # check export calc methodology using FAO production data
    
    # multiply export.prop by future production for total future exports
    mutate(exports_future = Exports.alloc.total.prop * production,
           exports_baseline = Exports.alloc.total.prop * production_2015_gaez,
           exports_fao = Exports.alloc.total.prop * production_2015_fao)
}

calc_export_partner_share <- function(fao_export_data, concord, est_exports){# start with trade matrix data
  x <- fao_export_data %>% 
    left_join(concord, by=c("Item Code (FAO)")) %>% 
    group_by(`Reporter Countries`, `Reporter Country Code (ISO2)`,`Partner Countries`, `Partner Country Code (ISO2)`, `Item Code (CPC)`, `Item (FBS)`, Year) %>% 
    summarise(Value = sum(Value,na.rm=T)) %>% 
    mutate(
      Exports_trade = Value/1000) %>% 
    dplyr::select(!c("Value")) %>% 
    ungroup()
  
  # calculate total exports by importer/exporter pair summed across years
  x %>% group_by(`Reporter Countries`, `Reporter Country Code (ISO2)`, 
                 `Partner Countries`, `Partner Country Code (ISO2)`,
                 `Item (FBS)`, `Item Code (CPC)`) %>% 
    summarise(sum_exports_partner = sum(Exports_trade, na.rm=T)) %>% 
    # calculate / left join total exports by exporter and summed across years
    left_join(x %>% group_by(`Item Code (CPC)`, `Item (FBS)`, `Reporter Countries`, `Reporter Country Code (ISO2)`) %>% 
                summarise(sum_exports_total = sum(Exports_trade, na.rm=T)) %>%  # sum across importers
                dplyr::select(`Item (FBS)`,
                              `Item Code (CPC)`,
                              `Reporter Countries`,
                              `Reporter Country Code (ISO2)`,
                              sum_exports_total),
              by = c("Reporter Countries","Item (FBS)", "Reporter Country Code (ISO2)", "Item Code (CPC)")) %>% 
    
    # calculate importer-share of total exports by importer/exporter pair (sum-years importer only /sum-years total)
    mutate(partner_export_share = sum_exports_partner/sum_exports_total) %>% 
    # calculate future exports by importer/exporter pair (importer-share * total future exports of each exporter)
    left_join(dplyr::select(est_exports,
                            `Reporter Countries`,
                            `Reporter Country Code (ISO2)`,
                            Item,
                            `Item Code (CPC)`,
                            exports_future,
                            exports_baseline,
                            exports_fao,
                            time_period), 
              by = c("Reporter Countries", "Reporter Country Code (ISO2)", "Item Code (CPC)")) %>% 
    mutate(partner_exports_future = exports_future * partner_export_share,
           partner_exports_baseline = exports_baseline * partner_export_share,
           partner_exports_fao = exports_fao * partner_export_share)   # expressed in tons
  
}

calc_imports <- function(est_trade_partners){
  est_trade_partners %>% 
    group_by(`Partner Countries`, `Partner Country Code (ISO2)`, `Item (FBS)`, `Item Code (CPC)`, time_period) %>% 
    # there are many Inf values for imports_baseline which are not present in the fao import data
    mutate(partner_exports_future=ifelse(is.infinite(partner_exports_future),0,partner_exports_future),
           partner_exports_baseline=ifelse(is.infinite(partner_exports_baseline),0,partner_exports_baseline),
           partner_exports_fao=ifelse(is.infinite(partner_exports_fao),0,partner_exports_fao)) %>% 
    summarise(imports_future = sum(partner_exports_future, na.rm=T),
              imports_baseline = sum(partner_exports_baseline, na.rm=T),
              imports_fao = sum(partner_exports_fao, na.rm=T)) 
}

process_fao_import_export_2015_data <- function(fbs_data){
  fbs_data %>%
    filter(Element %in% c("Import Quantity",
                          "Export Quantity")) %>%
    group_by(Area,`Area Code (ISO2)`, `Item Code (CPC)`,  Item, Element) %>% # summarise across years
    summarise(mean_value = mean(Value, na.rm=T)*1000) %>% # expressed in tons
    pivot_wider(., names_from=Element, values_from=mean_value) %>%
    rename(exports_2015 = `Export Quantity`,
           imports_2015 = `Import Quantity`) 
}

check_baseline_import_export <- function(data, est_imports, est_exports, outfile){
  data %>% 
    left_join(dplyr::select(est_imports, `Partner Country Code (ISO2)`, imports_baseline, imports_fao, `Item (FBS)`, `Item Code (CPC)`), by=c("Area Code (ISO2)"="Partner Country Code (ISO2)", "Item Code (CPC)")) %>% 
    left_join(dplyr::select(est_exports, `Reporter Country Code (ISO2)`, exports_baseline, exports_fao, `Item Code (CPC)`), by=c("Area Code (ISO2)"="Reporter Country Code (ISO2)", "Item Code (CPC)")) %>% 
  # actuals are imports_2015 and exports_2015, estimates are imports_baseline and exports_baseline
  # they should be near identical!
    dplyr::select(Area, `Area Code (ISO2)`, `Item Code (CPC)`, `Item (FBS)`, 
                  imports_baseline, imports_fao,
                  exports_baseline, exports_fao) %>% 
    write_csv(outfile)
}

calc_future_food_supply <- function(est_imports, 
                                    est_exports, 
                                    fao_crop_allocation_multiyear){
  est_imports %>% 
    left_join(dplyr::select(
      est_exports,
      `Reporter Country Code (ISO2)`,
       Item,
      `Item Code (CPC)`,
      time_period,
      production,
      exports_future
      
    ), by = c("Partner Country Code (ISO2)"="Reporter Country Code (ISO2)", "Item Code (CPC)", "time_period")) %>% 
    mutate(production=ifelse(
      is.na(production) | is.infinite(production), 0, production),
      imports_future=ifelse(
        is.na(imports_future) | is.infinite(imports_future), 0, imports_future),
      exports_future=ifelse(
        is.na(exports_future) | is.infinite(exports_future), 0, exports_future)) %>% 
    rowwise() %>% 
    mutate(total_supply = production + imports_future - exports_future) %>% 
    
    
    # apply crop allocation percentages
    left_join(dplyr::select(
      fao_crop_allocation_multiyear,
      `Area Code (ISO2)`,
      `Item Code (CPC)`,
      Item,
      Element, 
      Prop
    ), by = c("Partner Country Code (ISO2)"="Area Code (ISO2)", "Item Code (CPC)", "Item")) %>%  
    
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
    group_by(Area, `Area Code (ISO2)`) %>% 
    summarise(Total_Value = sum(Sum_adj, na.rm=T))
  # mutate proportion column
  df %>% 
    left_join(country_total_production, by = c("Area", "Area Code (ISO2)")) %>% 
    mutate(Prop = Sum_adj/Total_Value)
}

calc_total_future_calories <- function(fao_future_food_supply,
                                       fao_country_feed_calories,
                                       crop_calorie_conversion){
  fao_future_food_supply %>% 
    filter(Element %in% c("Feed", "Food")) %>% 
    left_join(fao_country_feed_calories, by=c("Partner Country Code (ISO2)"="Area Code (ISO2)")) %>% 
    left_join(crop_calorie_conversion, by=c("Item (FBS)")) %>% 
    # pivot_longer(., c("crop_conversion", "feed_conversion"), 
    #              names_to = "conversion_element", 
    #              values_to= "conversion_factor")
    rowwise() %>% 
    mutate(food_calories=total_supply*Prop*crop_conversion,
           feed_calories=total_supply*Prop*feed_conversion,
           total_calories=food_calories+feed_calories)
}

calc_future_food_gap <- function(future_total_calories,
                                 worldmap_clean,
                                 country_pop_mder){
  future_total_calories %>% 
    left_join(dplyr::select(
      as.data.frame(worldmap_clean),
      ISO_A2, ISO_A3), by=c("Partner Country Code (ISO2)"="ISO_A2")) %>% 
    left_join(dplyr::select(country_pop_mder,
                            Entity,
                            Code,
                            time_period,
                            mder_pop_annual_future), 
              by=c("ISO_A3"="Code", "time_period")) %>% 
    group_by(time_period) %>% 
    rename(calories_supply=total_calories,
           calories_demand=mder_pop_annual_future) %>% 
    mutate(
      calorie_gap_prop = calories_demand/calories_supply,
      calorie_gap_prop = round(calorie_gap_prop,2)) %>% 
    dplyr::select(!c("Entity")) %>% 
    rename(Country=`Partner Countries`) %>% 
    filter(ISO_A3!="Ashm")
}

calc_staple_share_calories <- function(fao_all_products_kcal,
                                       world_bank_country_list,
                                       grazing_income_group){
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
    dplyr::select(Area, `Area Code (ISO2)`, Item, `Item Code (CPC)`, Element, Year, Value_adj) %>% 
    group_by(Area, `Area Code (ISO2)`, Item, `Item Code (CPC)`) %>% 
    # sum across years
    summarise(Value_adj=sum(Value_adj,na.rm=T)) %>% 
    group_by(Area, `Area Code (ISO2)`) %>% 
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
              staple_item_share=sum_staple_items/sum_total_items)
  
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


calc_future_calorie_gap <- function(future_food_gap,
                                    country_pop_mder,
                                    fao_staple_share){
  future_food_gap %>% 
    left_join(dplyr::select(country_pop_mder, Code, mder_annual_2020, pop_future, time_period), 
              by = c("ISO_A3"="Code", "time_period")) %>% 
    left_join(dplyr::select(fao_staple_share, `Area Code (ISO2)`, staple_item_share),
              by=c("Partner Country Code (ISO2)" = "Area Code (ISO2)")) %>% # this line is new
    group_by(time_period) %>% 
    mutate(
      calories_supply_total = calories_supply / staple_item_share, # this line is new
      #calories_supply_total = calories_supply/0.7,
      calorie_gap_persons = (calories_demand - calories_supply_total)/mder_annual_2020,
      calorie_gap_rate = calorie_gap_persons/pop_future) %>% 
    filter(ISO_A3 != "Ashm")
  
}



calc_baseline_calories_by_crop <- function(country_baseline_future_production_df,
                                           fao_crop_allocation_multiyear,
                                           fao_country_feed_calories,
                                           crop_calorie_conversion,
                                           est_imports,
                                           est_exports,
                                           crops){
  country_baseline_future_production_df %>% 
    filter(time_period=="2021-2040") %>% 
    dplyr::select(!c("time_period")) %>%  # to prevent duplicate rows for baseline
    left_join(data.frame(crop=crops, Item=c("Maize and products",
                                                   "Rice and products",
                                                   "Soyabeans",
                                                   "Wheat and products"))) %>% 
    
    left_join(dplyr::select(fao_crop_allocation_multiyear, `Area Code (ISO2)`, Item, `Item Code (CPC)`, Element, Prop), 
              by=c("iso_a2"="Area Code (ISO2)", "Item")) %>% 
    filter(Element %in% c("Feed", "Food")) %>% 
    left_join(dplyr::select(fao_country_feed_calories, feed_conversion, `Area Code (ISO2)`), by=c("iso_a2"="Area Code (ISO2)")) %>% 
    left_join(crop_calorie_conversion, by=c("Item"="Item (FBS)")) %>% # need to left join import quantities in 2015 from FAO
    # left_join(fao_import_export_2015,
    #           by=c("iso_a2"="iso2", "alloc_crops"="Item")) %>% 
    left_join(est_imports %>% filter(time_period=="2021-2040") %>% dplyr::select(
                            `Partner Country Code (ISO2)`,
                            `Item Code (CPC)`,
                            imports_baseline),
              
              by=c("iso_a2"="Partner Country Code (ISO2)", "Item Code (CPC)")) %>% # we have lost 177-160 = 17 countries here?
    rename(imports_2015 = imports_baseline) %>% 
    left_join(est_exports %>% filter(time_period=="2021-2040") %>% dplyr::select(
                            `Reporter Country Code (ISO2)`,
                            `Item Code (CPC)`,
                            exports_baseline),
              by=c("iso_a2"="Reporter Country Code (ISO2)", "Item Code (CPC)")) %>% 
    rename(exports_2015 = exports_baseline) %>% 
    
    dplyr::select(!c("pct_change", "production_future")) %>% 
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
    group_by(Area, `Area Code (ISO2)`) %>% 
    summarise(Value=mean(Value,na.rm=T)) %>% 
    mutate(pop_2015=Value*1000) %>% 
    left_join(dplyr::select(
      as.data.frame(worldmap_clean),
      ISO_A2, ISO_A3), by=c("Area Code (ISO2)"="ISO_A2"))
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



calc_baseline_calorie_gap <- function(baseline_2015_calories_by_crop,
                                      country_pop_mder_2015,
                                      fao_staple_share){# only 145 countries
  baseline_2015_calories_by_crop %>% 
    group_by(name,ISO_A3,iso_a2) %>% 
    summarise(total_calories_gaez=sum(total_calories_gaez,na.rm=T)#,
              # total_calories_fao=sum(total_calories_fao,na.rm=T)
    ) %>% 
    left_join(dplyr::select(
      country_pop_mder_2015, 
      Code, mder_pop_annual_2015, mder_annual_2015, pop_2015), 
      by = c("ISO_A3"="Code")) %>% 
    rename(calories_demand=mder_pop_annual_2015) %>% 
    left_join(dplyr::select(fao_staple_share, `Area Code (ISO2)`, staple_item_share),
              by=c("iso_a2"="Area Code (ISO2)")) %>% 
    mutate( 
      #calories_supply_total_gaez = total_calories_gaez/0.7,
      #       calories_supply_total_fao = total_calories_fao/0.7,
      calories_supply_total_gaez = total_calories_gaez/staple_item_share, # /0.7
      # calories_supply_total_fao = total_calories_fao/staple_item_share, # /0.7
      calorie_gap_persons_gaez = (calories_demand - calories_supply_total_gaez)/mder_annual_2015,
      calorie_gap_rate_gaez = calorie_gap_persons_gaez/pop_2015#,
      # food_insecure_persons_fao = (calories_demand - calories_supply_total_fao)/mder_annual_2015,
      # pou_rate_fao = food_insecure_persons_fao/pop_2015
    ) %>% 
    filter(ISO_A3 != "Ashm")
}

check_est_fao_total_calories <- function(fao_all_products_kcal,
                                         world_bank_country_list,
                                         grazing_income_group,
                                         baseline_2015_calorie_gap){
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
  dplyr::select(Area, `Area Code (ISO2)`, Item, Element, Year, Value_adj) %>% 
  group_by(Area, `Area Code (ISO2)`, Item) %>% 
  # sum across years
  summarise(Value_adj=mean(Value_adj,na.rm=T)) %>% 
  group_by(Area, `Area Code (ISO2)`) %>% 
  summarise(sum_total_items=sum(Value_adj, na.rm=T))


data %>% 
  # sum_total_items
  left_join(dplyr::select(
    baseline_2015_calorie_gap,
    iso_a2,
    calories_supply_total_gaez),
    by=c("Area Code (ISO2)"="iso_a2")) %>% 
  mutate(fao_total_calories=sum_total_items*10^6) %>% 
  dplyr::select(Area, `Area Code (ISO2)`, fao_total_calories, calories_supply_total_gaez)

}

check_est_fao_calorie_gap <- function(comparison_2015_calories_supply,
                                      worldmap_clean,
                                      country_pop_mder_2015,
                                      outfile){
  data <- comparison_2015_calories_supply %>% 
  left_join(dplyr::select(
    as.data.frame(worldmap_clean),
    ISO_A2, ISO_A3), by=c("Area Code (ISO2)"="ISO_A2")) %>% 
  left_join(dplyr::select(
    country_pop_mder_2015, 
    Code, mder_pop_annual_2015, mder_annual_2015, pop_2015), 
    by = c("ISO_A3"="Code")) %>% 
  rename(calories_demand=mder_pop_annual_2015) %>% 
  mutate( 
    calorie_gap_persons_fao = (calories_demand - fao_total_calories)/mder_annual_2015,
    calorie_gap_persons_gaez = (calories_demand - calories_supply_total_gaez)/mder_annual_2015,
    calorie_gap_rate_fao = calorie_gap_persons_fao/pop_2015,
    calorie_gap_rate_gaez = calorie_gap_persons_gaez/pop_2015) %>% 
  filter(ISO_A3 != "Ashm") %>% 
  dplyr::select(Area, `Area Code (ISO2)`, ISO_A3, fao_total_calories, calories_supply_total_gaez, calories_demand, calorie_gap_rate_fao, calorie_gap_rate_gaez) 

data %>% write_csv(outfile)
data
}


calc_calorie_gap_change <- function(future_calorie_gap,
                                    baseline_2015_calorie_gap,
                                    outfile){
  future <- future_calorie_gap %>% 
  dplyr::select(c("Country", "Partner Country Code (ISO2)","ISO_A3","calories_demand","calories_supply_total",
                  "calorie_gap_rate", "time_period")) %>% 
   rename(calories_demand_future=calories_demand,
          calories_supply_total_future=calories_supply_total,
          calorie_gap_rate_future = calorie_gap_rate) %>% 
  # mutate(FI_status_2030 = ifelse(calorie_gap_rate_2030 < 0, 0, 1)) # 1=insecure
    mutate(FI_status_future = ifelse(calorie_gap_rate_future < 0, 0, 1))

baseline <- baseline_2015_calorie_gap %>% 
  dplyr::select(c("name", "ISO_A3", "calories_supply_total_gaez", "calories_demand", "calorie_gap_rate_gaez")) %>%
  rename(Country=name,
         calories_supply_total_2015 = calories_supply_total_gaez,
          calories_demand_2015 = calories_demand,
          calorie_gap_rate_2015 = calorie_gap_rate_gaez) %>% 
  mutate(FI_status_2015 = ifelse(calorie_gap_rate_2015 < 0, 0, 1)) # 1=insecure

data <- baseline %>% left_join(future,
                               by=c("ISO_A3")) %>% 
  mutate(FI_status_change = case_when(FI_status_2015==0 & FI_status_future==1 ~ "become insecure",
                                      FI_status_2015==0 & FI_status_future==0 ~ "remain secure",
                                      FI_status_2015==1 & FI_status_future==1 ~ "remain insecure",
                                      FI_status_2015==1 & FI_status_future==0 ~ "become secure"))

data %>% write_csv(outfile)

data


}

map_calorie_gap_persons <- function(data, World, outfile){
  
  data <- data %>% 
    dplyr::select(c("Country", "Partner Country Code (ISO2)","ISO_A3", "calorie_gap_persons_persons", "time_period")) %>% 
    mutate(persons_millions = calorie_gap_persons/10^6)
    
    dat <- World %>% left_join(data, by=c("iso_a2"="Partner Country Code (ISO2)"))
    
    plot <- tmap::tm_shape(dat) +
      tm_fill("persons_millions",
              palette = c(rev(hcl.colors(7,"RdYlGn"))),
              #palette=c("orange","lightgreen","red3","darkgreen"),
              title= 'Calorie gap in persons (millions)') +
      tmap::tm_shape(World) +
      tmap::tm_borders("grey", lwd =1) 
    
    tmap::tmap_save(plot, filename=outfile, height=4, width=10, asp=0)
    plot
    
}

plot_FI_status_change <- function(calorie_gap_change,
                                  World,
                                  outfile){
  data <- calorie_gap_change %>% 
    dplyr::select(`Partner Country Code (ISO2)`, ISO_A3, FI_status_change, time_period) 
  
  dat <- World %>% left_join(data, by=c("iso_a2"="Partner Country Code (ISO2)"))
  
  plot <- tmap::tm_shape(dat) +
    tm_fill("FI_status_change",
            palette = c("orange", "black","red3","darkgreen"),
            #palette=c("orange","lightgreen","red3","darkgreen"),
            title= 'Status change since 2015') +
    tm_facets(c("time_period"), drop.NA.facets=T) +
    tmap::tm_shape(World) +
    tmap::tm_borders("grey", lwd =1) 
  
  tmap::tmap_save(plot, filename=outfile, height=4, width=10, asp=0)
  plot
}






plot_rate_change <- function(calorie_gap_change,
                                        World,
                                        outfile){
  data <- calorie_gap_change %>% 
  # disregard food excess supply (net exporting countries) as the focus is on food security
  # the minimum pou rate is 0
  mutate(#pou_rate_2015=ifelse(pou_rate_2015<0, 0, pou_rate_2015),
         #pou_rate_2030=ifelse(pou_rate_2030<0, 0, pou_rate_2030),
    calorie_gap_rate_change=calorie_gap_rate_future-calorie_gap_rate_2015)
  # for uncapped, mutate (
    #pou_rate_change=(pou_rate_2030-pou_rate_2015)/pou_rate_2015)

dat <- World %>% left_join(data, by=c("iso_a2"="Partner Country Code (ISO2)"))

plot <- tmap::tm_shape(dat) +
  tm_fill("calorie_gap_rate_change", 
          palette=rev(hcl.colors(7,"RdYlGn")),
          #palette=c("yellowgreen","lightyellow","khaki1","orange","red3"), 
          #palette=c("yellowgreen", "lightyellow","khaki1","orange"),
          # note that some countries exceed this such as Ukraine
          #   palette=c("darkgreen", "yellowgreen","lightyellow","khaki1","orange","red3", "brown"), 
          #breaks=c(-0.4,-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5),
          midpoint=0,
          title= 'Change in calorie gap (persons/popn)') +
  tm_facets(c("time_period"), drop.NA.facets=T) +
  tmap::tm_shape(World) +
  tmap::tm_borders("grey", lwd =1) 

tmap::tmap_save(plot, filename=outfile, height=4, width=10, asp=0)
plot
# 
}





decompose_calorie_gap_change <- function(future_2030_calorie_gap,
                                         baseline_2015_calorie_gap,
                                         comparison_2015_calorie_gap){
  future <- future_calorie_gap %>% 
    dplyr::select(c("Country", "ISO_A3","calories_demand","calories_supply_total",
                    "mder_annual_2020","pop_future", "time_period")) %>% 
    rename(calories_demand_future=calories_demand,
           calories_supply_total_future=calories_supply_total) 
  
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
    mutate(change_demand_calories = calories_demand_future-calories_demand_2015,
           change_supply_calories = calories_supply_total_future-calories_supply_total_2015,
           calorie_gap_future = calories_demand_future-calories_supply_total_future, # gap = negative
           calorie_gap_2015 = calories_demand_2015-calories_supply_total_2015, # excess supply = positive
           calorie_gap_2015_fao = calories_demand_2015-fao_total_calories) 
  # how many and which countries to plot this for?
  # those who were secure and now become insecure?
  # all of those that are insecure?
  
}

plot_rate_change_become_insecure <- function(decomposed_change_in_calories,
                                                       outfile,
                                                       countries_becoming_insecure,
                                                       time,
                                                       label_future){
  data <- decomposed_change_in_calories %>% 
    dplyr::select(!c("name")) %>% 
    mutate(rate_gap_future=calorie_gap_future/mder_annual_future/pop_2021_2040,
           rate_gap_2015=calorie_gap_2015/mder_annual_2015/pop_2015,
           rate_gap_2015_fao=calorie_gap_2015_fao/mder_annual_2015/pop_2015,
           change_demand_rate=change_demand_calories/mder_annual_2020/pop_future, # instead of 2015/2015?
           change_supply_rate=change_supply_calories/mder_annual_2020/pop_future) %>% 
    filter(
      ISO_A3 %in% countries_becoming_insecure$ISO_A3) %>% # outlier that is skewing the bars 
    pivot_longer(.,
                 c("rate_gap_future",
                   "rate_gap_2015",
                   "rate_gap_2015_fao",
                   "change_demand_rate",
                   "change_supply_rate",),
                 names_to="measure",
                 values_to="rate") 
  
  data$measure <- factor(data$measure,
                         levels=c("rate_gap_2015_fao",
                                  "rate_gap_2015",
                                  "rate_gap_future",
                                  "change_demand_rate",
                                  "change_supply_rate"))
  
  lbls=c(
    "Demand - Supply FAO, 2015",
    "Demand - Supply, 2015",
    label,
    "Change in demand",
    "Change in supply"
  )
  
  plot <- ggplot(data %>% 
                   filter(time_period=time) %>% 
                   dplyr::select(c(ISO_A3, measure,rate)), 
                 aes(measure, rate,fill=measure))+
    geom_bar(stat="identity")+
    facet_wrap(~ISO_A3) +
    theme(axis.text.x = element_blank(),
          legend.position = "right")+
    scale_fill_discrete(
      limits=c("rate_gap_2015_fao",
               "rate_gap_2015",
               limit,
               "change_demand_rate",
               "change_supply_rate"),
      labels=str_wrap(lbls, 10),
      name="Measure"
    ) +
    scale_y_continuous(name="Persons / Population")
  
  plot
  ggsave(outfile)
  plot
}
# 
# plot_calorie_gap_become_insecure <- function(decomposed_change_in_calories,
#                                                         outfile,
#                                                         countries_becoming_insecure){
#   data <- decomposed_change_in_calories %>% 
#     dplyr::select(!c("name","calories_supply_total_2015","calories_demand_2015")) %>% 
#     filter(
#       ISO_A3 %in% countries_becoming_insecure$ISO_A3) %>% # outlier that is skewing the bars 
#     pivot_longer(.,
#                  c("calorie_gap_2030",
#                    "calorie_gap_2015",
#                    "change_demand_calories",
#                    "change_supply_calories",
#                    "calories_demand_2030",
#                    "calories_supply_total_2030"),
#                  names_to="measure",
#                  values_to="calories") %>% 
#     mutate(calories_billions=calories/10^9) %>% 
#     filter(!measure %in% c("calories_demand_2030",
#                            "calories_supply_total_2030"))
#   
#   
#   plot <- ggplot(data %>% 
#                    dplyr::select(c(ISO_A3, measure,calories_billions)), 
#                  aes(measure, calories_billions,fill=measure))+
#     geom_bar(stat="identity")+
#     facet_wrap(~ISO_A3) +
#     theme(axis.text.x = element_blank())+
#     scale_fill_discrete(
#       limits=c("calorie_gap_2015",
#                "calorie_gap_2030",
#                "change_demand_calories",
#                "change_supply_calories"),
#       labels=c(
#         "Demand - Supply, 2015",
#         "Demand - Supply, 2030",
#         "Change in demand (popn.)",
#         "Change in supply (prodn.)"
#       ),
#       name="Measure"
#     ) +
#     scale_y_continuous(name="Calories (billions)")
#   
#   plot
#   ggsave(outfile)
#   plot
# }
# 
# plot_persons_change_decomposed_become_insecure <- function(decomposed_change_in_calories,
#                                                            countries_becoming_insecure,
#                                                            outfile){
#   data <- decomposed_change_in_calories %>% 
#     dplyr::select(!c("name")) %>% 
#     mutate(rate_gap_2030=calorie_gap_2030/mder_annual_2020,
#            rate_gap_2015=calorie_gap_2015/mder_annual_2015,
#            change_demand_rate=change_demand_calories/mder_annual_2020, # instead of 2015/2015?
#            change_supply_rate=change_supply_calories/mder_annual_2020) %>% 
#     filter(
#       ISO_A3 %in% countries_becoming_insecure$ISO_A3) %>% # outlier that is skewing the bars 
#     pivot_longer(.,
#                  c("rate_gap_2030",
#                    "rate_gap_2015",
#                    "change_demand_rate",
#                    "change_supply_rate",),
#                  names_to="measure",
#                  values_to="persons") %>% 
#     mutate(persons_millions = persons/10^6)
#   
#   data$measure <- factor(data$measure,
#                          levels=c("rate_gap_2015",
#                                   "rate_gap_2030",
#                                   "change_demand_rate",
#                                   "change_supply_rate"))
#   
#   lbls=c(
#     "Demand - Supply, 2015",
#     "Demand - Supply, 2030",
#     "Change in demand",
#     "Change in supply"
#   )
#   
#   ggplot(data %>% 
#            dplyr::select(c(ISO_A3, measure,persons_millions)), 
#          aes(measure, persons_millions,fill=measure))+
#     geom_bar(stat="identity")+
#     facet_wrap(~ISO_A3) +
#     theme(axis.text.x = element_blank(),
#           legend.position = "right")+
#     scale_fill_discrete(
#       limits=c("rate_gap_2015",
#                "rate_gap_2030",
#                "change_demand_rate",
#                "change_supply_rate"),
#       labels=str_wrap(lbls, 10),
#       name="Measure"
#     ) +
#     scale_y_continuous(name="Persons (millions)")
#   
#   
#   ggsave(outfile)
#   
# }
# 
# plot_rate_change_decomposed_remain_insecure <- function(decomposed_change_in_calories,
#                                                        countries_remaining_insecure,
#                                                        outfile){
#   data <- decomposed_change_in_calories %>% 
#     dplyr::select(!c("name")) %>% 
#     mutate(rate_gap_2030=calorie_gap_2030/mder_annual_2020/pop_2021_2040,
#            rate_gap_2015=calorie_gap_2015/mder_annual_2015/pop_2015,
#            change_demand_rate=change_demand_calories/mder_annual_2020/pop_2021_2040, # instead of 2015/2015?
#            change_supply_rate=change_supply_calories/mder_annual_2020/pop_2021_2040) %>% 
#     filter(
#       ISO_A3 %in% countries_remaining_insecure$ISO_A3) %>% # outlier that is skewing the bars 
#     pivot_longer(.,
#                  c("rate_gap_2030",
#                    "rate_gap_2015",
#                    "change_demand_rate",
#                    "change_supply_rate",),
#                  names_to="measure",
#                  values_to="rate") 
#   
#   data$measure <- factor(data$measure,
#                          levels=c("rate_gap_2015",
#                                   "rate_gap_2030",
#                                   "change_demand_rate",
#                                   "change_supply_rate"))
#   
#   lbls=c(
#     "Demand - Supply, 2015",
#     "Demand - Supply, 2030",
#     "Change in demand",
#     "Change in supply"
#   )
#   
#   ggplot(data %>% 
#            dplyr::select(c(ISO_A3, measure,rate)), 
#          aes(measure, rate,fill=measure))+
#     geom_bar(stat="identity")+
#     facet_wrap(~ISO_A3) +
#     theme(axis.text.x = element_blank(),
#           legend.position = "right")+
#     scale_fill_discrete(
#       limits=c("rate_gap_2015",
#                "rate_gap_2030",
#                "change_demand_rate",
#                "change_supply_rate"),
#       labels=str_wrap(lbls, 10),
#       name="Measure"
#     ) +
#     scale_y_continuous(name="Persons / Population")
#   
#   
#   ggsave(outfile)
#   
# }