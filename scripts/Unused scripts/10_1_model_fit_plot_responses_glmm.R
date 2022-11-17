# alternative to 10_1 in order to nest country RE inside study RE

# test gamm4 and lme4 -----------------------------------------------------

mod_test_1 <- lapply(1:4, function(j){ # crop = j
  lapply(1:5, function(i,j){
    gamm4::gamm4(Yield.Change ~ 0 +
                   s(Temp.Change, k = 3) + 
                   Temp.Change:Baseline_tmp_weighted +
                   Precipitation.change:Baseline_pre_weighted +
                   Temp.Change:Precipitation.change +
                   f_CO2:C3 +
                   f_CO2:C4 +
                   adapt_dummy +
                   Temp.Change:adapt_dummy,
                 data = crop_imputed_data_restricted[[j]][[i]],
                 random = ~ (1 | Reference_fact/Country2_fact))
  }, j)})  #  Error: grouping factors must have > 1 sampled level 

mod_test_1 <- lapply(1:4, function(j){ # crop = j
  lapply(1:5, function(i,j){
    lme4::lmer(Yield.Change ~ 0 +
                 poly(Temp.Change, 2) + 
                 Temp.Change:Baseline_tmp_weighted +
                 Precipitation.change:Baseline_pre_weighted +
                 Temp.Change:Precipitation.change +
                 f_CO2:C3 +
                 f_CO2:C4 +
                 adapt_dummy +
                 Temp.Change:adapt_dummy +
                 (1 | Reference_fact/Country2_fact),
               data = crop_imputed_data_restricted[[j]][[i]])
  }, j)}) # works as glmm model
# this might also be the time to explore boundary singular fit etc
# but first refit only glmms to see if predicted results are different


model_formulas <- c(
  # gamm relative
  Yield.Change ~ 0 +
    s(Temp.Change, k = 3) + 
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    s(Reference_fact, bs = 're') + 
    s(Country2_fact, bs = 're'),
  # gamm absolute
  Abs.Yield.Change ~ 0 +
    s(Temp.Change, k = 3) + 
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    Baseline_yield +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    s(Reference_fact, bs = 're') + 
    s(Country2_fact, bs = 're'),
  # glmm relative
  Yield.Change ~ 0 + 
    poly(Temp.Change,2) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    s(Reference_fact, bs = 're') + 
    s(Country2_fact, bs = 're'),
  # glmm absolute
  Abs.Yield.Change ~ 0 +
    poly(Temp.Change,2) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    Baseline_yield +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    s(Reference_fact, bs = 're') + 
    s(Country2_fact, bs = 're')
)

# note an option of interacting country and study RE blew out memory

# fit model ---------------------------------------------------------------

fit_models <- function(i,j,k){
  
  mgcv::gam(model_formulas[[k]], 
            method = 'REML', 
            family = 'gaussian',
            data = crop_imputed_data_restricted[[j]][[i]]) 
  
}

crop_models <- lapply(1:4, function(k){ # formula = k - highest level in three-level list
  lapply(1:4, function(j, k){ # crop = j
    lapply(1:5, fit_models, j, k) # m = i
  }, k)}) 

saveRDS(crop_models, here("results","models","crop_models.RData"))