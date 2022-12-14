
# read in data that includes bs temp and precipitation data 

AGIMPACTS_bs_tp_yields <- readRDS(here("processed","AGIMPACTS_bs_tp_yields.Rdata"))


# IMPUTATION --------------------------------------------------------------

# add baseline_pre_weighted
# note integer vars are required (not factor) for imputation step

incomplete_vars <- AGIMPACTS_bs_tp_yields[,c("Temp.Change", "Yield.Change", "Precipitation.change", "CO2.Change", 
                                   "Baseline_tmp_weighted", "Baseline_pre_weighted", "Baseline_yield",
                                   "Reference_int", "f_CO2", "C3", "C4", 
                                   "adapt_dummy", "crop_pooled", "Country2_int")]
pred_all <- make.predictorMatrix(incomplete_vars)
# pred["Baseline_tmp_weighted", "Country2"] <- -2
pred_all[, "Reference_int"] <- -2
pred_all["Reference_int",] <- 0

# predictive mean matching multi-level imputation method
imp_pmm_ml_all <- mice(incomplete_vars, pred = pred_all, meth = "2l.pmm", print = FALSE, m = 5, maxit = 30, seed = 123)

# save convergence plot
png(file = file.path(here("results", "figures", "imp_convergence_plot.png")))
plot(imp_pmm_ml_all, layout=c(2,8))
dev.off()

# save density plot
png(file = file.path(here("results", "figures", "imp_density_plot.png")))
densityplot(imp_pmm_ml_all)
dev.off()

# sampling method 
imp_sample_all <- mice(incomplete_vars, pred = pred_all, meth = "sample", print = FALSE, m = 5, maxit = 30, seed = 123)

png(file = file.path(here("results", "figures", "imp_sample_convergence_plot.png")))
plot(imp_sample_all, layout=c(2,8))
dev.off()

# save density plot
png(file = file.path(here("results", "figures", "imp_sample_density_plot.png")))
densityplot(imp_sample_all)
dev.off()


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

impute_vars <- c("Temp.Change", "Yield.Change", "Precipitation.change", "CO2.Change", 
                 "Baseline_tmp_weighted", "Baseline_pre_weighted", "Baseline_yield",
                 "f_CO2")

select_imp_vars <- function(i, j){
  
  temp_imp <- cbind(Temp.Change = crop.list.imp[[j]]$imp$Temp.Change[i], 
                    row_index_temp = which(is.na(crop.list.imp[[j]]$data$Temp.Change)))
  
  yield_imp <-cbind(Yield.Change = crop.list.imp[[j]]$imp$Yield.Change[i], 
                    row_index_yield = which(is.na(crop.list.imp[[j]]$data$Yield.Change)))
  
  precip_imp <- cbind(Precipitation.change = crop.list.imp[[j]]$imp$Precipitation.change[i], 
                      row_index_precip = which(is.na(crop.list.imp[[j]]$data$Precipitation.change)))
  
  CO2_imp <- cbind(CO2.Change = crop.list.imp[[j]]$imp$CO2.Change[i], 
                   row_index_CO2 = which(is.na(crop.list.imp[[j]]$data$CO2.Change)))
  
  bstmp_imp <- cbind(Baseline_tmp_weighted = crop.list.imp[[j]]$imp$Baseline_tmp_weighted[i], 
                        row_index_bstmp = which(is.na(crop.list.imp[[j]]$data$Baseline_tmp_weighted)))
  
  bspre_imp <- cbind(Baseline_pre_weighted = crop.list.imp[[j]]$imp$Baseline_pre_weighted[i], 
                     row_index_bspre = which(is.na(crop.list.imp[[j]]$data$Baseline_pre_weighted)))
  
  bsyld_imp <- cbind(Baseline_yield = crop.list.imp[[j]]$imp$Baseline_yield[i], 
                     row_index_bsyld = which(is.na(crop.list.imp[[j]]$data$Baseline_yield)))
  
  fCO2_imp <- cbind(f_CO2 = crop.list.imp[[j]]$imp$f_CO2[i], 
                    row_index_fCO2 = which(is.na(crop.list.imp[[j]]$data$f_CO2)))
  
  combined <- crop.list.imp[[j]]$data %>% 
    mutate(row_index = row_number()) %>% 
    left_join(temp_imp, by = c("row_index" = "row_index_temp"))  %>% 
    left_join(yield_imp, by = c("row_index" = "row_index_yield")) %>% 
    left_join(precip_imp, by = c("row_index" = "row_index_precip")) %>% 
    left_join(CO2_imp, by = c("row_index" = "row_index_CO2")) %>% 
    left_join(bstmp_imp, by = c("row_index" = "row_index_bstmp")) %>% 
    left_join(bspre_imp, by = c("row_index" = "row_index_bspre")) %>% 
    left_join(bsyld_imp, by = c("row_index" = "row_index_bsyld")) %>% 
    left_join(fCO2_imp, by = c("row_index" = "row_index_fCO2")) 
  
  # rename the imputed variables that have been joined
  names(combined)[16:23] <- paste0(impute_vars, ".imp") 
  
  combined$precip <- coalesce(combined$Precipitation.change, combined$Precipitation.change.imp)
  combined$yield <- coalesce(combined$Yield.Change, combined$Yield.Change.imp)
  combined$temp <- coalesce(combined$Temp.Change, combined$Temp.Change.imp)
  combined$CO2 <- coalesce(combined$CO2.Change, combined$CO2.Change.imp)
  combined$bstmp <- coalesce(combined$Baseline_tmp_weighted, combined$Baseline_tmp_weighted.imp)
  combined$bspre <- coalesce(combined$Baseline_pre_weighted, combined$Baseline_pre_weighted.imp)
  combined$bsyld <- coalesce(combined$Baseline_yield, combined$Baseline_yield.imp)
  combined$f_CO2 <- coalesce(combined$f_CO2, combined$f_CO2.imp)
  
  combined <- combined %>% 
    dplyr::select(c("temp", "yield", "precip", "CO2", 
                    "bstmp", "bspre", "bsyld",
                    "f_CO2", "C3", "C4", "adapt_dummy", "crop_pooled", "Reference_int", "Country2_int")) 
  
  names(combined)[1:8] <- impute_vars
  
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

lapply(1:4, function(j){
  lapply(1:5, function(i,j){
    saveRDS(crop_imputed_datasets_factors[[j]][[i]], 
            here("processed", 
                 paste0("crop_data_unrestricted_", crops[[j]], "_", i, ".Rdata")))
  }, j)
})

# restrict temperature change to 0,5

crop_imputed_data_restriction <- function(i,j){
  
  crop_imputed_datasets_factors[[j]][[i]] %>% # Reference and Country2 already factorised
    filter(Temp.Change >= 0 & Temp.Change <= 5) %>% 
    mutate(crop_factor = as.factor(crop_pooled)) %>% 
    mutate(Abs.Yield.Change = Yield.Change*Baseline_yield/100) %>%  # unit of measurement 
    mutate(Pct.Precipitation.Change = Precipitation.change/100) %>% 
    mutate(Yield.Level = Baseline_yield*(1+(Yield.Change/100))) 
   
}

crop_imputed_data_restricted <- lapply(1:4, function(j){
  lapply(1:5, crop_imputed_data_restriction, j)})


# write imputed data as output files ----------------------------------------------

# write to Rdatafiles 

lapply(1:4, function(j){
  lapply(1:5, function(i,j){
    saveRDS(crop_imputed_data_restricted[[j]][[i]], 
            here("processed", 
                 paste0("crop_data_", crops[[j]], "_", i, ".Rdata")))
  }, j)
})

# readRDS(here("processed", "crop_data_Maize_1.Rdata"))

# export to csv - multiple files per crop and imputed dataset                
write_data_csv <- function(i,j){
  
  crop_imputed_data_restricted[[j]][[i]] %>% 
    readr::write_csv(here("processed", paste0("crop_data_", crops[[j]], "_", i, ".csv")))
}

lapply(1:4, function(j){
  lapply(1:5, write_data_csv, j)
})

