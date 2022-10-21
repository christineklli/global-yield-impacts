# 
model_formulas_int_slopes <- c(
  # temp and precip
  Yield.Change ~ 0 +
    s(Temp.Change, k = 3) + 
    s(Precipitation.change, k = 3) + # NEW
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    s(Reference_fact, bs='re') + # random intercept
    s(Temp.Change, Reference_fact, bs = 're') + 
    s(Country2_fact, bs = 're'),
  # varying temp and+ precip slope separately - AFTER MODEL 5 THIS IS NEXT BEST MODEL
  Yield.Change ~ 0 +
    s(Temp.Change, k = 3) + 
    s(Precipitation.change, k = 3) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    s(Reference_fact, bs='re') + # random intercept
    s(Temp.Change, Reference_fact, bs = 're') + 
    s(Precipitation.change, Reference_fact, bs = 're') + # NEW
    s(Country2_fact, bs = 're'),
  # varying temp + precip slope + interaction of temp and precip
  Yield.Change ~ 0 +
    s(Temp.Change, k = 3) + 
    s(Precipitation.change, k = 3) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    s(Reference_fact, bs='re') + # random intercept
    s(Temp.Change, Reference_fact, bs = 're') + 
    s(Precipitation.change, Reference_fact, bs = 're') +
    s(Temp.Change, Precipitation.change, Reference_fact, bs = 're') + # NEW
    s(Country2_fact, bs = 're'),
  # varying interaction of temp and precip only
  Yield.Change ~ 0 +
    s(Temp.Change, k = 3) + 
    s(Precipitation.change, k = 3) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    s(Reference_fact, bs='re') + # random intercept
    s(Temp.Change, Precipitation.change, Reference_fact, bs = 're') + # NEW
    s(Country2_fact, bs = 're'),
  # try varying co2 slope as well - THIS IS THE NEW CHOSEN MODEL
  Yield.Change ~ 0 +
    s(Temp.Change, k = 3) + 
    s(Precipitation.change, k = 3) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    s(Reference_fact, bs='re') + # random intercept
    s(Temp.Change, Reference_fact, bs = 're') + 
    s(Precipitation.change, Reference_fact, bs = 're') + # NEW
    s(f_CO2, Reference_fact, bs = 're') + # NEW
    s(Country2_fact, bs = 're'),
  # try varying co2 slope as well as country slope? haven't run this yet
  Yield.Change ~ 0 +
    s(Temp.Change, k = 3) + 
    s(Precipitation.change, k = 3) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    s(Reference_fact, bs='re') + # random intercept
    s(Temp.Change, Reference_fact, bs = 're') + 
    s(Precipitation.change, Reference_fact, bs = 're') + # NEW
    s(f_CO2, Reference_fact, bs = 're') + # NEW
    s(Country2_fact, bs = 're') + 
    s(Temp.Change, Country2_fact, bs = 're')
  )


# fit model ---------------------------------------------------------------

fit_models_int_slopes <- function(i,j,k){
  
  mgcv::gam(model_formulas_int_slopes[[k]], # change between slopes and slopes_only
            method = 'REML', 
            family = 'gaussian',
            data = crop_imputed_data_restricted[[j]][[i]]) 
  
}


# compare with or without precip change as smooth term --------------------

crop_models_int_slopes <- lapply(1:4, function(k){ # formula = k - highest level in three-level list
  lapply(1:4, function(j, k){ # crop = j
    lapply(1:5, fit_models_int_slopes, j, k) # m = i
  }, k)}) 

saveRDS(crop_models_int_slopes, here("results", "models", "crop_models_int_slopes.RData"))


crop_models_ext <- lapply(5, function(k){ # formula = k - highest level in three-level list
  lapply(1:4, function(j, k){ # crop = j
    lapply(1:5, fit_models_int_slopes, j, k) # m = i
  }, k)}) 

saveRDS(crop_models_ext, here("results", "models", "crop_models_ext.RData"))


crop_models_countryslope <- lapply(6, function(k){ # formula = k - highest level in three-level list
  lapply(1:4, function(j, k){ # crop = j
    lapply(1:5, fit_models_int_slopes, j, k) # m = i
  }, k)}) 

# compare model 1 against crop_models_slopes
# only diff is with or without precip change as smooth term

compareML(crop_models_slopes[[1]][[1]][[5]], crop_models_int_slopes[[1]][[1]][[5]]) 

# decide whether to include precip change as smooth term into more complex models

# mixed bag - some of them have very small REML difference
# but some have VERY large AIC scores favouring including precip change as smooth term
# this is probably because of the large number of studies that include many precip change combinations
# decide to proceed with including precipitation change smooth term

# compare different specifications for fitting random slopes --------------
# note: as result of this process, model 2 is the chosen model!

# the last three models can all be compared with the first model which has temp random slope only
# function for each crop and imputed dataset to compare each model 2:4, systematically, against model 1

# compare model 2 against model 1
compare_1_2 <- lapply(1:4, function(j){
  lapply(1:5, function(m, j){
    compareML(crop_models_int_slopes[[1]][[j]][[m]], crop_models_int_slopes[[2]][[j]][[m]])
  }, j)
})

# for maize, model 2 is better than model 1 for 5/5 imputed datasets
# for rice, model 2 is better than model 1 for 4/5 imputed datasets
# for soy, model 2 is better than model 1 for 5/5 imputed datasets
# for wheat, model 2 is better than model 1 for 5/5 imputed datasets

# if not fitting any other model specs, this should be the new baseline
# model 3 is more complex than model 2 so should be compared to model 2

# compare model 3 against model 2
compare_2_3 <- lapply(1:4, function(j){
  lapply(1:5, function(m, j){
    compareML(crop_models_int_slopes[[2]][[j]][[m]], crop_models_int_slopes[[3]][[j]][[m]])
  }, j)
})

# for maize, model 3 is better than model 2 for 4/5 m
# for rice, model 3 is better than model 2 for 2/5 m
# for soy, model 3 is better than model 2 for 3/5 m
# for wheat, model 3 is better than model 2 for 3/5

# maybe not worth it considering the added complexity? quite small magnitude differences in AIC
gammit::extract_ranef(crop_models_int_slopes[[3]][[3]][[1]]) %>% print(n=Inf)
# the value in terms of differences between studies for temp/precip are quite small

# compare model 4 against model 1
compare_2_4 <- lapply(1:4, function(j){
  lapply(1:5, function(m, j){
    compareML(crop_models_int_slopes[[2]][[j]][[m]], crop_models_int_slopes[[4]][[j]][[m]])
  }, j)
})

# for maize, model 4 is better than model 2 for 0/5
# for rice, model 4 is better than model 2 for 0/5
# for soy, model 4 is better than model 2 for 0/5
# for wheat, model 4 is better than model 2 for 0/5

# so really we are comparing models 2 and 3
# but based on rationale of compare_2_3, I think we should stick to model 2
# model 2 is the chosen model!

# compare CO2 model against model 2
compare_2_5 <- lapply(1:4, function(j){
  lapply(1:5, function(m, j){
    compareML(crop_models_int_slopes[[2]][[j]][[m]], crop_models_ext[[1]][[j]][[m]])
  }, j)
})

# for maize, model 5 is better than model 2 for 3/5 with small AIC difs
# for rice, model 5 is better than model 2 for 5/5 with large AIC difs (probs more CO2 scenarios)
# for soy, model 5 is better than model 2 for 5/5 with large AIC diffs as well
# for wheat, model 5 is better than model 2 for 5/5 with moderately large AIC difs

# model 5 is now the new baseline / chosen model

# compare country random slope model against model 5

compare_5_6 <- lapply(1:4, function(j){
  lapply(1:5, function(m, j){
    compareML(crop_models_ext[[1]][[j]][[m]], crop_models_countryslope[[1]][[j]][[m]])
  }, j)
})

# for maize, model 6 is better than model 5 for 3/5 m
# for rice, model 6 is better than model 5 for 2-3/5 m
# for soy, model 6 is better than model 5 for 3/5 m
# for wheat, model 6 is better than model 5 for 2/5 m

# stick with model 5, more parsimonious

# examine study-specific random slope terms -------------------------------

# also check data are the same across the different models, considering how much level complexity there is in indexing
summary(crop_models_slopes[[1]][[3]][[1]])
coef(crop_models_slopes[[1]][[3]][[1]]) # s(Reference_fact, Temp.Change).1 is 8.484629e+00 
# this may include both the overall slope + the study-specific slope effect, but regardless it tells us that 
# no - give nthese are exactly the same as extract_ranef, it's reporting the difference from overall mean slope

# the first study and  second study is contributing the large positive slope
# however the level names are missing - what is the 'first study'?

# USE different function:
# look at random effects specific estimates (equivalent of ranef in lmer)
# this should represent the difference from overall mean slope?
gammit::extract_ranef(crop_models_int_slopes[[2]][[3]][[1]]) %>% print(n=Inf)

gammit::extract_ranef(crop_models_int_slopes[[3]][[3]][[1]]) %>% print(n=Inf)


gammit::extract_ranef(crop_models_ext[[1]][[3]][[5]]) %>% print(n=Inf)
# random coefs look REALLY different and country intercepts do too now as well
# is it worth having country random slopes?
summary(crop_models_ext[[1]][[3]][[1]])


gammit::extract_ranef(crop_models_countryslope[[1]][[3]][[1]]) %>% print(n=Inf)


# understanding what is driving soybean results ---------------------------

# actually reference levels should be same across all datasets regardless of crop split?
AGIMPACTS_bs_tp_yields %>% 
  filter(Reference_int==8) %>% #dplyr::select(Reference)# 189 data points from Brassard (2008)
  ggplot(aes(Temp.Change, Yield.Change, col = Precipitation.change)) + 
  geom_point() +
  facet_grid(cols = vars(crop_pooled)) # soybean definitely looks positive -ranef is 16.0!
# however there is still a positive relationship
# and the magnitude of yield increases are huge, i.e. above 60%
# look into this paper/data
# other factors affecting the slope other than dP - maybe dCO2, site?
AGIMPACTS_bs_tp_yields %>% 
  filter(Reference_int==8 & crop_pooled == "Soybean") %>% 
  dplyr::select(CO2.Projected,
                CO2.Baseline,
                Temp.Change,
                Precipitation.change,
                Yield.Change,
                Adaptation,
                Climate.scenario,
                f_CO2) %>% 
  print(n=Inf)

# data points are unique to temp/precip/co2 combinations
# may need random CO2 slope too
# however, the yield change estimates are ridiculously high - check paper

AGIMPACTS_bs_tp_yields %>% 
  filter(Reference_int==8) %>% #dplyr::select(Reference)# 189 data points from Brassard (2008)
  ggplot(aes(Temp.Change, Precipitation.change)) + 
  geom_point() +
  facet_grid(cols = vars(crop_pooled))

# it's bcause higher temperatures are correlated with higher precipitation change 
# therefore higher yield change
# need an interaction random slope effect between temp change and precipitation change!!

AGIMPACTS_bs_tp_yields %>% 
  filter(Reference_int==5) %>%  #dplyr::select(Reference)# 460 data points from Alexandrov et al 
  ggplot(aes(Temp.Change, Yield.Change)) + 
  geom_point() +
  facet_grid(cols = vars(Precipitation.change)) # only soybean results in Austria! 
# this is because there are so many different precipitation change scenarios!
# however if we look at extract_ranef for group 5, the temp.change x precipitationc.hange slope is pretty small
# so should be ok not to include that interaction term random slope

