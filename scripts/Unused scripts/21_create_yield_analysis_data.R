# this script follows directly from script 19 and is followed by modelling in script 22


# read in GHDY country crop yield time series data ------------------------

crop_yields_lists <- lapply(1:4, function(j){
  lapply(1:36, function(i,j){
    readr::read_csv(here("processed", paste0("crop_yields_lists_", crops[[j]], "_", i, ".csv")))},j)
  })

# long dataframe by year 1:36

crop_yields_df <- lapply(1:4, function(i){
  rbindlist(crop_yields_lists[[i]], idcol = "year")})

crop_yields_dt <- rbindlist(crop_yields_df, idcol = "crop_pooled")

crop_numbers <- tribble(
  ~id_col , ~crop_pooled,
  "Maize", 1,
  "Rice", 2,
  "Soybean", 3,
  "Wheat", 4
)

crop_yields_dt <- crop_yields_dt %>%
  left_join(crop_numbers, by ='crop_pooled') %>% 
  mutate(Baseline.start = 1980+year) %>% 
  dplyr::select(!c("crop_pooled", "year")) %>% 
  rename(crop_pooled = "id_col") 
  

# merge with CGIAR data ---------------------------------------------------

# go back to start of script 08, run data cleaning section
# merge time series country crop yield data with CGIAR data and then re-impute 

# join by crop, baseline start year and country
cgiar_data <- AGIMPACTS_FINAL_INCOMPLETE %>% 
  left_join(crop_yields_dt, 
            by = c("crop_pooled", "Baseline.start", "CountryName"="name", "Country2_fact"="iso_a2")) %>% 
  rename(Baseline_yield = mean_yield)


# IMPUTATION --------------------------------------------------------------

# this is a repeat of the imputation, but with mean_yield time series where baseline year data is not available

f <- cgiar_data[,c("Baseline_yield", "Temp.Change", "Yield.Change", "Precipitation.change", "CO2.Change", "Baseline_tmp_weighted", "Reference_int", "f_CO2", "C3", "C4", "adapt_dummy", "crop_pooled", "Country2_int")]
pred_all <- make.predictorMatrix(f)
# pred["Baseline_tmp_weighted", "Country2"] <- -2
pred_all[, "Reference_int"] <- -2
pred_all["Reference_int",] <- 0

# multi-level predictive mean matching imputation
imp_pmm_ml_all <- mice(f, pred = pred_all, meth = "2l.pmm", print = FALSE, m = 5, maxit = 30, seed = 123)

plot(imp_pmm_ml_all, layout=c(2,6))
densityplot(imp_pmm_ml_all)

# random sampling imputation
imp_sample_all <- mice(f, pred = pred_all, meth = "sample", print = FALSE, m = 5, maxit = 30, seed = 123)

plot(imp_sample_all, layout=c(2,6))
densityplot(imp_sample_all)


# subset data in order to run crop-specific models
# first shape data long 
imp_long <- mice::complete(imp_pmm_ml_all, "long", include = T)

# do grouping as normal
imp_long_maize <- imp_long[which(imp_long$crop_pooled == "Maize"),]
imp_long_rice <- imp_long[which(imp_long$crop_pooled == "Rice"),]
imp_long_soy <- imp_long[which(imp_long$crop_pooled == "Soybean"),]
imp_long_wheat <- imp_long[which(imp_long$crop_pooled == "Wheat"),]

# turn back into mids objects to perform mice operations
imp_maize <- as.mids(imp_long_maize)
imp_rice <- as.mids(imp_long_rice)
imp_soy <- as.mids(imp_long_soy)
imp_wheat <- as.mids(imp_long_wheat)

# coalesce imputed and observed data --------------------------------------

# do have to coalesce imputed and observed data as all the variables we need for our final model are contained in imp_pmm_ml_all$imp$(EACH VARIABLE)
# for each imp_maize; imp_rice; imp_soy; imp_wheat, do separately:

# coalesce all variables of m datasets back into AGIMPACTS_FINAL_INCOMPLETE, and then create m datasets as elements of one list
# do this one m at a time?

crop.list.imp <- list(imp_maize, imp_rice, imp_soy, imp_wheat)

# make crop.list.imp number 'j' (crop) and temp.change 'i' (m imputed dataset)

impute_vars <- c("Baseline_yield", "Temp.Change", "Yield.Change", "Precipitation.change", "CO2.Change", "Baseline_tmp_weighted", "f_CO2")

select_imp_vars <- function(i, j){
  
  baseline_yield_imp <- cbind(baseline_yield = crop.list.imp[[j]]$imp$Baseline_yield[i],
                              row_index_baseline_yield = which(is.na(crop.list.imp[[j]]$data$Baseline_yield)))
  
  temp_imp <- cbind(Temp.Change = crop.list.imp[[j]]$imp$Temp.Change[i], 
                    row_index_temp = which(is.na(crop.list.imp[[j]]$data$Temp.Change)))
  
  yield_imp <-cbind(Yield.Change = crop.list.imp[[j]]$imp$Yield.Change[i], 
                    row_index_yield = which(is.na(crop.list.imp[[j]]$data$Yield.Change)))
  
  precip_imp <- cbind(Precipitation.change = crop.list.imp[[j]]$imp$Precipitation.change[i], 
                      row_index_precip = which(is.na(crop.list.imp[[j]]$data$Precipitation.change)))
  
  CO2_imp <- cbind(CO2.Change = crop.list.imp[[j]]$imp$CO2.Change[i], 
                   row_index_CO2 = which(is.na(crop.list.imp[[j]]$data$CO2.Change)))
  
  baseline_imp <- cbind(Baseline_tmp_weighted = crop.list.imp[[j]]$imp$Baseline_tmp_weighted[i], 
                        row_index_baseline = which(is.na(crop.list.imp[[j]]$data$Baseline_tmp_weighted)))
  
  fCO2_imp <- cbind(f_CO2 = crop.list.imp[[j]]$imp$f_CO2[i], 
                    row_index_fCO2 = which(is.na(crop.list.imp[[j]]$data$f_CO2)))
  
  combined <- crop.list.imp[[j]]$data %>% 
    mutate(row_index = row_number()) %>% 
    left_join(baseline_yield_imp, by = c("row_index" = "row_index_baseline_yield")) %>% 
    left_join(temp_imp, by = c("row_index" = "row_index_temp"))  %>% 
    left_join(yield_imp, by = c("row_index" = "row_index_yield")) %>% 
    left_join(precip_imp, by = c("row_index" = "row_index_precip")) %>% 
    left_join(CO2_imp, by = c("row_index" = "row_index_CO2")) %>% 
    left_join(baseline_imp, by = c("row_index" = "row_index_baseline")) %>% 
    left_join(fCO2_imp, by = c("row_index" = "row_index_fCO2")) 
  
  # rename the imputed variables that have been joined
  names(combined)[15:21] <- paste0(impute_vars, ".imp") 
  
  combined$baseline_yield <- coalesce(combined$Baseline_yield, combined$Baseline_yield.imp)
  combined$precip <- coalesce(combined$Precipitation.change, combined$Precipitation.change.imp)
  combined$yield <- coalesce(combined$Yield.Change, combined$Yield.Change.imp)
  combined$temp <- coalesce(combined$Temp.Change, combined$Temp.Change.imp)
  combined$CO2 <- coalesce(combined$CO2.Change, combined$CO2.Change.imp)
  combined$base <- coalesce(combined$Baseline_tmp_weighted, combined$Baseline_tmp_weighted.imp)
  combined$f_CO2 <- coalesce(combined$f_CO2, combined$f_CO2.imp)
  
  combined <- combined %>% 
    dplyr::select(c("baseline_yield", "temp", "yield", "precip", "CO2", "base", "f_CO2", "C3", "C4", "adapt_dummy", "crop_pooled", "Reference_int", "Country2_int")) 
  
  names(combined)[1:7] <- impute_vars
  
  combined
  
}

crop_imputed_datasets <- lapply(1:4, function(j){lapply(1:5, select_imp_vars, j)})


# create final datasets ---------------------------------------------------


crop_imputed_data_factors <- function(i,j){
  
  crop_imputed_datasets[[j]][[i]] %>% 
    mutate(Reference_fact = as.factor(Reference_int)) %>% 
    rename(Country2_fact = Country2_int)
  
}

crop_imputed_datasets_factors <- lapply(1:4, function(j){lapply(1:5, crop_imputed_data_factors, j)})

# restrict temperature change to 0,5

crop_imputed_data_restriction <- function(i,j){
  
  crop_imputed_datasets_factors[[j]][[i]] %>% # Reference and Country2 already factorised
    filter(Temp.Change >= 0 & Temp.Change <= 5) %>% 
    mutate(crop_factor = as.factor(crop_pooled))
  
}

crop_imputed_data_restricted <- lapply(1:4, function(j){
  lapply(1:5, crop_imputed_data_restriction, j)})


meta_m1_data_restricted <- function(i ,j){
  
  crop_imputed_data_restricted[[j]][[i]] %>% 
    mutate(es.id = row_number()) %>% 
    group_by(Reference_fact, Country2_fact)
}

meta_m1_data_restricted_crops <- lapply(1:4, function(j){lapply(1:5, meta_m1_data_restricted, j)})



DATA_YIELD <- lapply(1:4, function(j){
  lapply(1:5, function(i, j) {
    meta_m1_data_restricted_crops[[j]][[i]] %>% 
      #left_join(country_baseline_list_4[[j]], by = c("Country2_fact" = "ISO_A2")) %>% 
      mutate(Abs.Yield.Change = Yield.Change*Baseline_yield/100) %>%  # unit of measurement 
      mutate(Pct.Precipitation.Change = Precipitation.change/100) %>% 
      mutate(Yield.Level = Baseline_yield*(1+(Yield.Change/100))) %>%
      ungroup()
  }, j)
})

# write to csv
lapply(1:4, function(j){
  lapply(1:5, function(i,j){
    DATA_YIELD[[j]][[i]] %>% 
      readr::write_csv(here("processed", paste0("data_yield_", crops[[j]], "_", i, ".csv")))
  }
  , j)
})

