
# read in data ------------------------------------------------------------

crops <- c("Maize", "Rice", "Soybean", "Wheat")

crop_imputed_data_restricted <- lapply(1:4, function(j){
  lapply(1:5, function(i,j){
    readRDS(here("processed", paste0("crop_data_", crops[[j]], "_", i, ".Rdata")))
  }, j)
})

crop_imputed_data_restricted <- lapply(1:4, function(j){
  lapply(1:5, function(i,j){
   
    crop_imputed_data_restricted[[j]][[i]] %>% 
      mutate(Abs.Precipitation.Change = Baseline_pre_weighted * Pct.Precipitation.Change)
  }, j)
})

saveRDS(crop_imputed_data_restricted, here("processed",
                                           "crop_imputed_data_restricted.RData"))


crop_imputed_data_restricted <- readRDS(here("processed",
                                           "crop_imputed_data_restricted.RData"))

# model formulas ----------------------------------------------------------

model_formulas_abs <- c(Abs.Yield.Change ~ 0 +
  s(Temp.Change, k = 3) + 
  s(Abs.Precipitation.Change, k = 3) + # this has just been updated!!
  Temp.Change:Baseline_tmp_weighted +
  Abs.Precipitation.Change:Baseline_pre_weighted +
  Baseline_yield +
  Temp.Change:Abs.Precipitation.Change +
  f_CO2:C3 +
  f_CO2:C4 +
  adapt_dummy +
  Temp.Change:adapt_dummy +
  s(Reference_fact, bs='re') + # random intercept 
  s(Country2_fact, bs = 're') +
  s(Temp.Change, Reference_fact, bs = 're') + 
  s(Abs.Precipitation.Change, Reference_fact, bs = 're') + 
  s(f_CO2, Reference_fact, bs = 're'))


# model fit  --------------------------------------------------------------

fit_models_abs <- function(i,j,k){
  
  mgcv::gam(model_formulas_abs[[k]], # change between slopes and slopes_only
            method = 'REML', 
            family = 'gaussian',
            data = crop_imputed_data_restricted[[j]][[i]]) 
  
}

crop_models_abs <- lapply(1, function(k){ # formula = k - highest level in three-level list
  lapply(1:4, function(j, k){ # crop = j
    lapply(1:5, fit_models_abs, j, k) # m = i
  }, k)}) 

saveRDS(crop_models_abs, here("results",
                              "models",
                              "crop_models_abs.RData"))

crop_models_abs <- readRDS(here("results",
                              "models",
                              "crop_models_abs.RData"))
