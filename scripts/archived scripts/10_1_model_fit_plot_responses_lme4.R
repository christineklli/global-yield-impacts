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
  }, j)})  #  Error: grouping factors must have > 1 sampled level - could come back to this

# could be similar reason for error as for mgcv::gam() - too many parameters to be estimated relative to data due to smoothing functions
# since grouping factors seems to refer to Country2_fact
# c3, c4, adapt_dummy are factor variables with > 1 levels so unlikely to be source of error

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


# write out lme4 model specifications -------------------------------------------------


model_formulas_lmer <- c(
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
    (1 | Reference_fact/Country2_fact) +
    (1 | Country2_fact),
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
    (1 | Reference_fact/Country2_fact) +
    (1 | Country2_fact)
)


# fit model ---------------------------------------------------------------

# edit for lme4 functions

fit_models_lmer <- function(i,j,k){
  
  lme4::lmer(model_formulas_lmer[[k]], 
            data = crop_imputed_data_restricted[[j]][[i]]) 
  
}

crop_models_lmer <- lapply(1:2, function(k){ # formula = k - highest level in three-level list
  lapply(1:4, function(j, k){ # crop = j
    lapply(1:5, fit_models_lmer, j, k) # m = i
  }, k)}) 

saveRDS(crop_models_lmer, here("results","models","crop_models_lmer.RData"))

# go straight to script 15