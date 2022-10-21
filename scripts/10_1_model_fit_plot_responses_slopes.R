# 
model_formulas_slopes <- c(
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
    s(Reference_fact, bs='re') + # random intercept
    s(Temp.Change, Reference_fact, bs = 're') + # random slope - order of vars shouldnt matter
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
    s(Reference_fact, bs='re') +
    s(Temp.Change, Reference_fact, bs = 're') + 
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

# temp change re slope only - take out fixed effect smooth term on temp change
# note we decide not to go this route
model_formulas_slopes_only <- c(
  # gamm relative
  Yield.Change ~ 0 +
    #s(Temp.Change, k = 3) + 
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    s(Reference_fact, bs='re') + # random intercept
    s(Temp.Change, Reference_fact, bs = 're') + # random slope - order of vars shouldnt matter
    s(Country2_fact, bs = 're'),
  # gamm absolute
  Abs.Yield.Change ~ 0 +
    #s(Temp.Change, k = 3) + 
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    Baseline_yield +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    s(Reference_fact, bs='re') +
    s(Temp.Change, Reference_fact, bs = 're') + 
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


# fit model ---------------------------------------------------------------

fit_models_slopes <- function(i,j,k){
  
  mgcv::gam(model_formulas_slopes[[k]], # change between slopes and slopes_only
            method = 'REML', 
            family = 'gaussian',
            data = crop_imputed_data_restricted[[j]][[i]]) 
  
}

# only fit gam models
crop_models_slopes<- lapply(1:2, function(k){ # formula = k - highest level in three-level list
  lapply(1:4, function(j, k){ # crop = j
    lapply(1:5, fit_models_slopes, j, k) # m = i
  }, k)}) 

saveRDS(crop_models_slopes, here("results","models","crop_models_slopes.RData"))
#saveRDS(crop_models_slopes_only, here("results","models","crop_models_slopes_only.RData"))

crop_models_slopes <- readRDS(here("results","models","crop_models_slopes.RData"))

crop_models <- readRDS(here("results","models","crop_models.RData"))


# compare random intercepts only and random slopes models -----------------

# visualise() and model.comparison() from flexplot don't work 
# https://www.youtube.com/watch?v=Lz-IBUxQG5A&list=PL8F480DgtpW9_IT7xN1XeRF_dglZmK0nM&index=9
# use other visualisation tools for gam

gratia::draw(crop_models_slopes[[1]][[1]][[1]]) # does't work either, object 'value' not found

model.comparison(crop_models_slopes[[1]][[1]][[1]], crop_models[[1]][[1]][[1]])

# use itsadug package for GAMM models
inspect_random(crop_models_slopes[[1]][[1]][[1]]) # this doesn't
compareML(crop_models_slopes[[1]][[1]][[2]], crop_models[[1]][[1]][[2]]) # this works
# full model is statistically significant, lower AIC also favours full model

variance_comp(crop_models_slopes[[1]][[4]][[1]])



# examine study-specific random slope terms -------------------------------

# also check data are the same across the different models, considering how much level complexity there is in indexing
summary(crop_models_slopes[[1]][[3]][[1]])
coef(crop_models_slopes[[1]][[3]][[1]]) # s(Reference_fact, Temp.Change).1 is 8.484629e+00 
# this may include both the overall slope + the study-specific slope effect, but regardless it tells us that 
# no - give nthese are exactly the same as extract_ranef, it's reporting the difference from overall mean slope
# https://m-clark.github.io/gammit/reference/extract_ranef.html
# the first study and  second study is contributing the large positive slope
# however the level names are missing - what is the 'first study'?

# USE different function:
# look at random effects specific estimates (equivalent of ranef in lmer)
# this should represent the difference from overall mean slope?
gammit::extract_ranef(crop_models_slopes[[1]][[3]][[1]]) %>% print(n=Inf)
# this is exactly the same as coef but with se, lower 2.5 and upper 97.5 pct
# and most importantly with labels for group
# BUT reference intercepts are missing?
# mgcv strips the level names for 're' smooth terms, so this attempts to get them back. 
#This may not work under every circumstance, but the attempt is made to extract the names 
# of random effect groups based on how they are ordered in the data (which is how the model matrix would be constructed), 
# and in the case of random slopes, detect that second variable in the 're' specification would be the grouping variable. 
# This will not work for continuous x continuous smooths of type 're', 
# but I can't think of a reason why you'd use that given your other options with mgcv.

# first and second study (group 5 and group 8) give very large positive slopes
# which studies are these?

crop_imputed_data_restricted[[3]][[1]] # soybean
crop_imputed_data_restricted[[3]][[1]] %>% 
  filter(Reference_int ==8 | Reference_int==5)

# actually reference levels should be same across all datasets regardless of crop split?
AGIMPACTS_bs_tp_yields %>% 
  filter(Reference_int==8) %>% #dplyr::select(Reference)# 189 data points from Brassard (2008)
  ggplot(aes(Temp.Change, Yield.Change, col = Precipitation.change)) + 
  geom_point() +
  facet_grid(cols = vars(crop_pooled)) # soybean definitely looks positive -ranef is 16.0!
# however there is still a positive relationship
# and the magnitude of yield increases are huge, i.e. above 60%
# look into this paper/data

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


# plot the study-specific curves


# decide not to go with crop_models_slopes for theoretical reasons --------


# also compare crop_models_slopes and crop_models_slopes_only
# the latter takes out the first smooth term on temp change
# not sure about implications for response functions
compareML(crop_models_slopes[[1]][[2]][[1]], crop_models_slopes_only[[1]][[2]][[1]]) 
# very small difference for maize, prefer slopes_only
# for rice, prefer slopes
# for soy, very small difference, prefer slopes
# for wheat, very small diff, prefer slopes_only
# try for other m datasets, slopes seems to be better for most - stick with slopes


variance_comp(crop_models_slopes_only[[1]][[1]][[1]])
inspect_random(crop_models_slopes_only[[1]][[1]][[1]]) 

# https://stats.stackexchange.com/questions/532209/do-i-have-to-drop-a-random-slope-if-i-drop-the-fixed-effect-in-mixed-models-for
# theory rationale for not dropping main fixed effects
