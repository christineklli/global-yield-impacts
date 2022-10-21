

# READ IN DATA IF NOT FOLLOWING DIRECTLY FROM 10 ------------------------------------------------------------

crops <- c("Maize", "Rice", "Soybean", "Wheat")

crop_imputed_data_restricted <- lapply(1:4, function(j){
  lapply(1:5, function(i,j){
    readRDS(here("processed", paste0("crop_data_", crops[[j]], "_", i, ".Rdata")))
  }, j)
})


# FIT GAMM --------------------------------------------------------------

# write function to fit models based on list of formulas - just have a single model fitting function for each crop

gamm_formulas <- c(# Bs precip + linear temp-precip interaction
                   Yield.Change ~ 
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
                  # Bs precip + smoothed temp-precip interaction
                  Yield.Change ~ 
                    s(Temp.Change, k = 3) + 
                    Temp.Change:Baseline_tmp_weighted +
                    Precipitation.change:Baseline_pre_weighted +
                    te(Temp.Change, Precipitation.change) +
                    f_CO2:C3 +
                    f_CO2:C4 +
                    adapt_dummy +
                    Temp.Change:adapt_dummy +
                    s(Reference_fact, bs = 're') + 
                    s(Country2_fact, bs = 're'),
                  # try fitting glmm
                  Yield.Change ~ # adding 0+ seems to make no diff
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
                  # Absolutes
                  Abs.Yield.Change ~ 
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

fit_gamm <- function(i,j,k){
  
  mgcv::gam(gamm_formulas[[k]], 
                      method = 'REML', 
                      family = 'gaussian',
                      data = crop_imputed_data_restricted[[j]][[i]]) 
  
}

crop_gamm <- lapply(1:5, function(k){ # formula = k - highest level in three-level list
  lapply(1:4, function(j, k){ # crop = j
    lapply(1:5, fit_gamm, j, k) # m = i
  }, k)}) 

lapply(1:2, function(k){ # formula = k - highest level in three-level list
  lapply(1:4, function(j, k){ # crop = j
    lapply(1:5, function(i,j,k){
      saveRDS(crop_gamm[[k]][[j]][[i]], 
              here("results", "models", paste("crop_gamm_", k, crops[[j]], i, ".RData", sep="_")))
    }
           , j, k) # m = i
  }, k)}) 

# try fitting glmm
trial_gamm <- lapply(4, function(k){ # formula = k - highest level in three-level list
  lapply(1, function(j, k){ # crop = j
    lapply(1, fit_gamm, j, k) # m = i
  }, k)}) 


# this will end up saving models in a 'superlist' with 3 levels - [[gamm formula]][[crop]][[imputed dataset]]
# we will run all of these 4*5*No.formulas models, but only save the final model?
# even then, keep this function so that we can test and store results of future model specifications easily




# Fit GLMM ----------------------------------------------------------------

### Advantage of fitting GLMM is to force response through origin

# packages - glmm or brms or lme4 or mgcv
# if using mgcv then can fit the glmm as part of the list of model formulas above
# however, need to make sure intercepts are zero
# mgcv can't fit correlated random effects model
# https://stats.stackexchange.com/questions/522682/r-fit-linear-mixed-effects-model-with-gam-mgcv
# https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/
# are countries and studies correlated? some countries within a study may be correlated..
# is that the same thing? some countries across studies may also be correlated?
# study and country is correlated...
# https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf


maize_glmm_formulas <- c(# most basic glmm 
  Yield.Change ~ 
    poly(Temp.Change,2) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
   # f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    (1 | Reference_fact) + 
    (1 | Country2_fact),
  # most basic glmm with zero intercept
  Yield.Change ~ 0 + # this doesn't seem to be working to predict yield.change at 0 when everything at 0
    poly(Temp.Change,2) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
   # f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    (1 | Reference_fact) + 
    (1 | Country2_fact),
# country RE nested within study RE with zero intercept
  Yield.Change ~ 0 + 
    poly(Temp.Change,2) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
   # f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    (1 | Reference_fact/Country2_fact),
# country RE nested within study RE with zero intercept - Abs yield change ~ Baseline_yield
  Abs.Yield.Change ~ 0 + 
    poly(Temp.Change,2) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    Baseline_yield +
   # f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    (1 | Reference_fact/Country2_fact)
) # fixed effect model matrix is rank deficient so dropping 1 columns / coefficients - the C3 dummy


fit_maize_glmm <- function(i,j,k){
  
  lme4::lmer(maize_glmm_formulas[[k]], 
            data = crop_imputed_data_restricted[[j]][[i]]) 
  
}

maize_glmm <- lapply(1:4, function(k){ # formula = k - highest level in three-level list
  lapply(1, function(j, k){ # crop = j
    lapply(1:5, fit_maize_glmm, j, k) # m = i
  }, k)}) 


# testing maize models results in non-zero origin, which is not supposed to happen!

# other crops -------------------------------------------------------------

# two issues: singular model + origin does not go through zero

glmm_formulas <- c(# most basic glmm 
  Yield.Change ~ 
    poly(Temp.Change,2) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    #f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    (1 | Reference_fact) + 
    (1 | Country2_fact),
  # most basic glmm with zero intercept
  Yield.Change ~ -1 + 
    poly(Temp.Change,2) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    #f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    (1 | Reference_fact) + 
    (1 | Country2_fact),
  # country RE nested within study RE with zero intercept
  Yield.Change ~ -1 + 
    poly(Temp.Change,2) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    #f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    (1 | Reference_fact/Country2_fact),
  # country RE nested within study RE with zero intercept - Abs yield change ~ Baseline_yield
  Abs.Yield.Change ~ -1 + 
   poly(Temp.Change,2) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    Baseline_yield +
    f_CO2:C3 +
    #f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    (1 | Reference_fact/Country2_fact),
  # country RE nested within study RE with zero intercept - Abs yield change ~ Baseline_yield
  Abs.Yield.Change ~ -1 + 
    poly(Temp.Change,2) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    Baseline_yield +
    f_CO2:C3 +
    #f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    (1 | Reference_fact) +
    (1 | Country2_fact)
) 


fit_glmm <- function(i,j,k){
  
  lme4::lmer(glmm_formulas[[k]], 
             data = crop_imputed_data_restricted[[j]][[i]]) 
  
}

other_glmm <- lapply(1:5, function(k){ # formula = k - highest level in three-level list
  lapply(2:4, function(j, k){ # crop = j
    lapply(1:5, fit_glmm, j, k) # m = i
  }, k)}) 

# boundary (singular) fit: see ?isSingular
# this may be because random effectts are very small
# overfitted mixed model can result in singular fits
# https://stats.stackexchange.com/questions/323273/what-to-do-with-random-effects-correlation-that-equals-1-or-1
# this does not apply to maize and wheat, only rice and soy
# which suggests that overfitting is an issue for the two smaller datasets, rice and soy
# however, rice is the second largest dataset?

# SOY formulas m2, m4 and m5  show same patterns
isSingular(other_glmm[[1]][[2]][[5]]) # true

isSingular(other_glmm[[2]][[2]][[5]]) # true

isSingular(other_glmm[[3]][[2]][[5]]) # true

isSingular(other_glmm[[4]][[2]][[5]]) # false - Abs Yield nested RE

isSingular(other_glmm[[5]][[2]][[5]]) # false - Abs Yield non-nested RE 

gratia::variance_comp(trial_gamm[[1]][[1]][[1]])

# compare gamm and glmm on the same models- note they are the same despite different order of formula and crop
# however the variance and stdev with gam model is less close to zero? however still pretty small
gratia::variance_comp(crop_gamm[[4]][[2]][[2]])
summary(other_glmm[[2]][[1]][[2]])$varcor

gratia::variance_comp(crop_gamm[[5]][[2]][[2]])
summary(other_glmm[[5]][[1]][[2]])$varcor

summary(other_glmm[[1]][[2]][[5]])$varcor # there is no variation at all in the country-specific intercept?
summary(other_glmm[[4]][[2]][[5]])$varcor # nesting improves the variancei n the country intercept
summary(other_glmm[[4]][[1]][[2]])$varcor # but not for rice m2
# https://www.r-bloggers.com/2021/02/using-random-effects-in-gams-with-mgcv/

# RICE formulas m2
isSingular(other_glmm[[1]][[1]][[2]]) # true

isSingular(other_glmm[[2]][[1]][[2]]) # true

isSingular(other_glmm[[3]][[1]][[2]]) # true

isSingular(other_glmm[[4]][[1]][[2]]) # true - Abs Yield nested RE

isSingular(other_glmm[[5]][[1]][[2]]) # true - Abs Yield non-nested RE 

# time to go Bayesian?
# use a partially Bayesian method that produces maximum a posteriori (MAP) estimates using regularizing priors to force the estimated random-effects variance-covariance matrices away from singularity (Chung et al 2013, blme package)
# use a fully Bayesian method that both regularizes the model via informative priors and gives estimates and credible intervals for all parameters that average over the uncertainty in the random effects parameters (Gelman and Hill 2006, McElreath 2015; MCMCglmm, rstanarm and brms packages)

fixef(maize_glmm[[1]][[1]][[1]])
fixef(maize_glmm[[2]][[1]][[1]])
ranef(maize_glmm[[1]][[1]][[1]]) # note can make a dot/whisker plot of intercept estimates from this output

# compile list of all crop glmms ------------------------------------------

# need to compile on the middle element, which is annoying


lmerTest::anova(crop_glmm[[4]][[1]][[1]]) # test

lapply(1:4, function(k){ # formula = k - highest level in three-level list - we probably want model 3 or 4
  lapply(1:4, function(j, k){ # crop = j
    lapply(1:5, function(i,j,k){
      saveRDS(crop_glmm[[k]][[j]][[i]], 
              here("results", "models", paste("crop_glmm_", k, crops[[j]], i, ".RData", sep="_")))
    }
    , j, k) # m = i
  }, k)}) 

# now to script 11 to cross validate and choose final model candidate(s)
# or to script 12 to plot diagnostics and print estimates and plot predicted response functions
# main thing is to compare, for the same m1 and crop, different model specifications

# example showing that constraining gam model intercept to 0 results in predictions far away
x<-seq(0,10,length=100)
y<-x^2+rnorm(100)
m1<-mgcv::gam(y~s(x,k=10,bs='cs'))
m2<-mgcv::gam(y~s(x,k=10,bs='cs')-1)
x1<-seq(0,10,0.1)
y1<-predict(m1,newdata=list(x=x1))
y2<-predict(m2,newdata=list(x=x1))
plot(x,y,ylim=c(0,100))
lines(x1,y1,lwd=4,col='red')
lines(x1,y2,lwd=4,col='blue')
