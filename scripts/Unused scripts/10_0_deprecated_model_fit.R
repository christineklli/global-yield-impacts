

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
                    s(Country2_fact, bs = 're'))

# note an option of interacting country and study RE blew out memory

fit_gamm <- function(i,j,k){
  
  mgcv::gam(gamm_formulas[[k]], 
                      method = 'REML', 
                      family = 'gaussian',
                      data = crop_imputed_data_restricted[[j]][[i]]) 
  
}

crop_gamm <- lapply(1:2, function(k){ # formula = k - highest level in three-level list
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

glmm_formulas <- c(# most basic glmm 
  Yield.Change ~ 
    poly(Temp.Change,2) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    (1 | Reference_fact) + 
    (1 | Country2_fact),
  # most basic glmm with zero intercept
  Yield.Change ~ 0 + 
    poly(Temp.Change,2) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
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
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    (1 | Reference_fact/Country2_fact),
# correlated random intercept and slope, with zero intercept
  Yield.Change ~ 0 + 
    poly(Temp.Change,2) +
    Temp.Change:Baseline_tmp_weighted +
    Precipitation.change:Baseline_pre_weighted +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    (1 | Reference_fact) +
    (Temp.Change | Country2_fact),
# country RE nested within study RE with zero intercept - Abs yield change ~ Baseline_yield
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
    (1 | Reference_fact/Country2_fact)
) # fixed effect model matrix is rank deficient so dropping 1 columns / coefficients - the C3 dummy


fit_glmm <- function(i,j,k){
  
  lme4::lmer(glmm_formulas[[k]], 
            data = crop_imputed_data_restricted[[j]][[i]]) 
  
}

crop_glmm <- lapply(1:5, function(k){ # formula = k - highest level in three-level list
  lapply(1:4, function(j, k){ # crop = j
    lapply(1:5, fit_glmm, j, k) # m = i
  }, k)}) 


lmerTest::anova(crop_glmm[[4]][[1]][[1]]) # test

lapply(1:5, function(k){ # formula = k - highest level in three-level list - we probably want model 3 or 4
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
