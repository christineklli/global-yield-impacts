

# define model formulas ---------------------------------------------------

model_spec <- c(
    Yield.Change ~ 0 +
      s(Temp.Change, k = 3) + 
      Temp.Change:Baseline_tmp +
      Precipitation.change:Baseline_pre +
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
      Temp.Change:Baseline_tmp +
      Precipitation.change:Baseline_pre +
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
      Temp.Change:Baseline_tmp +
      Precipitation.change:Baseline_pre +
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
      Temp.Change:Baseline_tmp +
      Precipitation.change:Baseline_pre +
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

# fit ALL models

fit_models <- function(data, spec){
  
  lapply(1:4, function(k){ # formula = k - highest level in three-level list 1:4
    lapply(1:4, function(j, k){ # crop = j 1:4
      lapply(1:4, function(i,j,k){ # 1:4
             mgcv::gam(spec[[k]], 
                       method = 'REML', 
                       family = 'gaussian',
                       data = data[[j]][[i]]) 
      }
             , j, k) # m = i
    }, k)}) 
}


# plot multiply model response functions ----------------------------------


