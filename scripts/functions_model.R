

# define model formulas ---------------------------------------------------

model_spec <- c(
  # gamm relative
  Yield.Change ~ 0 +
    s(Temp.Change) + 
    s(Precipitation.change) +
    Temp.Change:Baseline_tmp +
    Precipitation.change:Baseline_pre +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    s(Reference_fact, bs='re') + # random intercept
    s(Temp.Change, Reference_fact, bs = 're') + # random slope - order of vars shouldnt matter
    s(Precipitation.change, Reference_fact, bs = 're') +
    s(f_CO2, Reference_fact, bs = 're') +
    s(Country2_fact, bs = 're'),
  
  # GAMM RI
  Yield.Change ~ 0 +
    s(Temp.Change) + 
    s(Precipitation.change) +
    Temp.Change:Baseline_tmp +
    Precipitation.change:Baseline_pre +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    s(Reference_fact, bs='re') + # random intercept
    #s(Temp.Change, Reference_fact, bs = 're') + # random slope - order of vars shouldnt matter
    #s(Precipitation.change, Reference_fact, bs = 're') +
    #s(f_CO2, Reference_fact, bs = 're') +
    s(Country2_fact, bs = 're'),
  
  # glmm relative
  Yield.Change ~ 0 + 
    poly(Temp.Change,2) +
    poly(Precipitation.change, 2) +
    Temp.Change:Baseline_tmp +
    Precipitation.change:Baseline_pre +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    s(Reference_fact, bs = 're') + 
    s(Temp.Change, Reference_fact, bs = 're') + 
    s(Precipitation.change, Reference_fact, bs = 're') +
    s(f_CO2, Reference_fact, bs = 're') +
    s(Country2_fact, bs = 're'),
  
  # GLMM RI
  Yield.Change ~ 0 + 
    poly(Temp.Change,2) +
    poly(Precipitation.change, 2) +
    Temp.Change:Baseline_tmp +
    Precipitation.change:Baseline_pre +
    Temp.Change:Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy +
    s(Reference_fact, bs = 're') + 
    #s(Temp.Change, Reference_fact, bs = 're') + 
    #s(Precipitation.change, Reference_fact, bs = 're') +
    #s(f_CO2, Reference_fact, bs = 're') +
    s(Country2_fact, bs = 're'),
  
  # LM
  Yield.Change ~ 0 +
    Temp.Change +
    I(Temp.Change)^2 +
    Temp.Change:Baseline_tmp +
    Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy
)


lm_model_spec <- c(
  Yield.Change ~ 0 +
    Temp.Change +
    I(Temp.Change)^2 +
    Temp.Change:Baseline_tmp +
    Precipitation.change +
    f_CO2:C3 +
    f_CO2:C4 +
    adapt_dummy +
    Temp.Change:adapt_dummy
)


# fit model ---------------------------------------------------------------

# fit GAMM AND GLMM relative & absolute RS models

fit_models <- function(data, spec){
  
  lapply(1:5, function(k){ # formula = k - highest level in three-level list 1:4
    lapply(1:4, function(j, k){ # crop = j 1:4
      lapply(1:5, function(i,j,k){ # 1:4
        mgcv::gam(spec[[k]], 
                  method = 'REML', 
                  family = 'gaussian',
                  data = data[[j]][[i]]) 
      }
      , j, k) # m = i
    }, k)}) 
}

# fit lm model
fit_lm <- function(data, spec){
  
  lapply(1, function(k){ # formula = k - highest level in three-level list 1:4
    lapply(1:4, function(j, k){ # crop = j 1:4
      lapply(1:5, function(i,j,k){ # 1:4
        mgcv::gam(spec[[k]], 
                  method = 'REML', 
                  family = 'gaussian',
                  data = data[[j]][[i]]) 
      }
      , j, k) # m = i
    }, k)}) 
}


# plot multiply model response functions ----------------------------------

# use imp terminology and workflow exactly same as in Solomon Kurz

multiply_imp_for_plot_gam_RS <- function(data){
  
  # hopefully the fact that we had to force Reference_int instead of
  # Reference_fact does not change the model estimates?
  # we couldn't use Reference_fact in imputation process...
  # anyway this is only the case for pooled response functions
  # not for the predictions and plotted predictions themselves
  
  # break out imputed data into 4 crop data sets- right now all are pooled
  
  unnested_list <- lapply(1:4, function(i){
    mice:::with.mids(data[[i]], mgcv::gam(Yield.Change ~ 0 +
                                            s(Temp.Change) + 
                                            s(Precipitation.change) +
                                            Temp.Change:Baseline_tmp +
                                            Precipitation.change:Baseline_pre +
                                            Temp.Change:Precipitation.change +
                                            f_CO2:C3 +
                                            f_CO2:C4 +
                                            adapt_dummy +
                                            Temp.Change:adapt_dummy +
                                            s(Reference_int, bs='re') + # random intercept
                                            s(Temp.Change, Reference_int, bs = 're') + # random slope - order of vars shouldnt matter
                                            s(Precipitation.change, Reference_int, bs = 're') +
                                            s(f_CO2, Reference_int, bs = 're') +
                                            s(Country2_fact, bs = 're'),
                                          method='REML',
                                          family='gaussian'))
  })
  
  
  
  # need to nest this list per every 5xm elements
  list(unnested_list[[1]],
       unnested_list[[2]],
       unnested_list[[3]],
       unnested_list[[4]])  
  
}

# GAM RI


multiply_imp_for_plot_gam_RI <- function(data){
  
  # hopefully the fact that we had to force Reference_int instead of
  # Reference_fact does not change the model estimates?
  # we couldn't use Reference_fact in imputation process...
  # anyway this is only the case for pooled response functions
  # not for the predictions and plotted predictions themselves
  
  # break out imputed data into 4 crop data sets- right now all are pooled
  
  unnested_list <- lapply(1:4, function(i){
    mice:::with.mids(data[[i]], mgcv::gam(Yield.Change ~ 0 +
                                            s(Temp.Change) + 
                                            s(Precipitation.change) +
                                            Temp.Change:Baseline_tmp +
                                            Precipitation.change:Baseline_pre +
                                            Temp.Change:Precipitation.change +
                                            f_CO2:C3 +
                                            f_CO2:C4 +
                                            adapt_dummy +
                                            Temp.Change:adapt_dummy +
                                            s(Reference_int, bs='re') + # random intercept
                                            #s(Temp.Change, Reference_fact, bs = 're') + # random slope - order of vars shouldnt matter
                                            #s(Precipitation.change, Reference_fact, bs = 're') +
                                            #s(f_CO2, Reference_fact, bs = 're') +
                                            s(Country2_fact, bs = 're'),
                                          method='REML',
                                          family='gaussian'))
  })
  
  
  
  # need to nest this list per every 5xm elements
  list(unnested_list[[1]],
       unnested_list[[2]],
       unnested_list[[3]],
       unnested_list[[4]])  
  
}

# GLM RS

multiply_imp_for_plot_glm_RS <- function(data){
  
  # hopefully the fact that we had to force Reference_int instead of
  # Reference_fact does not change the model estimates?
  # we couldn't use Reference_fact in imputation process...
  # anyway this is only the case for pooled response functions
  # not for the predictions and plotted predictions themselves
  
  # break out imputed data into 4 crop data sets- right now all are pooled
  
  unnested_list <- lapply(1:4, function(i){
    mice:::with.mids(data[[i]], mgcv::gam(Yield.Change ~ 0 + 
                                            poly(Temp.Change,2) +
                                            poly(Precipitation.change, 2) +
                                            Temp.Change:Baseline_tmp +
                                            Precipitation.change:Baseline_pre +
                                            Temp.Change:Precipitation.change +
                                            f_CO2:C3 +
                                            f_CO2:C4 +
                                            adapt_dummy +
                                            Temp.Change:adapt_dummy +
                                            s(Reference_int, bs = 're') + 
                                            s(Temp.Change, Reference_int, bs = 're') + 
                                            s(Precipitation.change, Reference_int, bs = 're') +
                                            s(f_CO2, Reference_int, bs = 're') +
                                            s(Country2_fact, bs = 're'),
                                          method='REML',
                                          family='gaussian'))
  })
  
  
  
  # need to nest this list per every 5xm elements
  list(unnested_list[[1]],
       unnested_list[[2]],
       unnested_list[[3]],
       unnested_list[[4]])  
  
}


# GLM RI


multiply_imp_for_plot_glm_RI <- function(data){
  
  # hopefully the fact that we had to force Reference_int instead of
  # Reference_fact does not change the model estimates?
  # we couldn't use Reference_fact in imputation process...
  # anyway this is only the case for pooled response functions
  # not for the predictions and plotted predictions themselves
  
  # break out imputed data into 4 crop data sets- right now all are pooled
  
  unnested_list <- lapply(1:4, function(i){
    mice:::with.mids(data[[i]], mgcv::gam(Yield.Change ~ 0 + 
                                            poly(Temp.Change,2) +
                                            poly(Precipitation.change, 2) +
                                            Temp.Change:Baseline_tmp +
                                            Precipitation.change:Baseline_pre +
                                            Temp.Change:Precipitation.change +
                                            f_CO2:C3 +
                                            f_CO2:C4 +
                                            adapt_dummy +
                                            Temp.Change:adapt_dummy +
                                            s(Reference_int, bs = 're') + 
                                            #s(Temp.Change, Reference_fact, bs = 're') + 
                                            #s(Precipitation.change, Reference_fact, bs = 're') +
                                            #s(f_CO2, Reference_fact, bs = 're') +
                                            s(Country2_fact, bs = 're'),
                                          method='REML',
                                          family='gaussian'))
  })
  
  
  
  # need to nest this list per every 5xm elements
  list(unnested_list[[1]],
       unnested_list[[2]],
       unnested_list[[3]],
       unnested_list[[4]])  
  
}

# LM


multiply_imp_for_plot_lm <- function(data){
  
  # hopefully the fact that we had to force Reference_int instead of
  # Reference_fact does not change the model estimates?
  # we couldn't use Reference_fact in imputation process...
  # anyway this is only the case for pooled response functions
  # not for the predictions and plotted predictions themselves
  
  # break out imputed data into 4 crop data sets- right now all are pooled
  
  unnested_list <- lapply(1:4, function(i){
    mice:::with.mids(data[[i]], mgcv::gam(Yield.Change ~ 0 +
                                            Temp.Change +
                                            I(Temp.Change)^2 +
                                            Temp.Change:Baseline_tmp +
                                            Precipitation.change +
                                            f_CO2:C3 +
                                            f_CO2:C4 +
                                            adapt_dummy +
                                            Temp.Change:adapt_dummy,
                                          method='REML',
                                          family='gaussian'))
    
  })
  
  # need to nest this list per every 5xm elements
  list(unnested_list[[1]],
       unnested_list[[2]],
       unnested_list[[3]],
       unnested_list[[4]])  
  
}


list_multiply_fit <- function(list1,list2,list3,list4,list5){
  
  list(list1,
       list2,
       list3,
       list4,
       list5)
}

# we don't actually use this
new_data <- function(data){
  
  nd <- expand.grid(
    Precipitation.change = data$Precipitation.change,
    CO2.Change = data$CO2.Change)
  # too memory intensive to have unique combinations of all vars
  # besides we can set predictions conditional on constant terms
  nd %>% 
    mutate(Temp.Change = seq(0,5,nrow(nd)),
           Baseline_tmp = median(data$Baseline_tmp),
           Baseline_pre = median(data$Baseline_pre),
           adapt_dummy = 1, # or 0
           C3 = 1,
           C4 = 0, # NOTE WE CAN ONLY PLOT NON-MAIZE FOR NOW
           f_CO2 = CO2.Change/(CO2.Change + 100)
           #Reference_int = data$Reference_int,
           #Country2_fact = data$Country2_fact
    )
  
}


#fit_lines <- function(fit, data){

# since we do not provide the newdata argument, 
# the model is fit on original data
# so we can append this to the fitted values
#  tibble(.imp=1:5) %>% 
#    mutate(p=purrr::map(.imp, ~ predict(fit$analyses[[.]],
#newdata = data,  
#newdata.guaranteed = TRUE,
#                                 se.fit = TRUE) %>% 
#                   data.frame())) %>% 
#    unnest(p) %>% 
# add in the nd predictor data
#    bind_cols(
#      bind_rows(replicate(5, data, simplify = FALSE))
#    )
#}

fit_lines <- function(fit, data){
  
  lapply(1:5, function(k){
    
    #v = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
    
    lapply(1:4, function(i, k){ # crop subscripts
      
      tibble(.imp=1:5) %>% # m subscripts
        mutate(p=purrr::map(.imp, ~ 
                              ggpredict(fit[[k]][[i]]$analyses[[.]],
                                        terms = c("Temp.Change [0:5 by=0.25]"#,
                                                  #"Baseline_tmp [5:25, by= 5]"
                                        ),
                                        typical = "median", # bs temp and bs precip
                                        condition = c(Precipitation.change =0, # change this
                                                      adapt_dummy = 0, f_CO2 = 0, 
                                                      Reference_fact = 0, Country2_fact = 0),
                                        type = "random")  %>% 
                              data.frame())) %>% 
        unnest(p) #%>% 
      # add in the nd predictor data
      #bind_cols(
      #  bind_rows(replicate(5, data, simplify = FALSE))
      #)
      
    }, k)
    
  })
}





# however, not sure how to capture partial dependence/conditional terms
# working this out

#group_fitted_lines <- function(fitted_lines){

#  m <- 5

#  fitted_lines %>% 
#    group_by(Temp.Change, Precipitation.change, f_CO2) %>% 
# filter for certain conditional terms i.e. baseline temp and baseline precip?
#    filter(Baseline_tmp == median(Baseline_tmp),
#           Baseline_pre == median(Baseline_pre)) %>% 
# there's also C3, C4 etc?
#  summarise(fit_bar = mean(fit),
#            v_w     = mean(se.fit^2),
#            v_b     = sum((fit - fit_bar)^2) / (m - 1),
#            v_p     = v_w + v_b * (1 + (1 / m)),
#            se_p    = sqrt(v_p)) %>% 
# use the _p suffix to indicate these are pooled
#  mutate(lwr_p = fit_bar - se_p * 1.96,
#         upr_p = fit_bar + se_p * 1.96) 

#}

group_fitted_lines <- function(fitted_lines){
  
  lapply(1:5, function(k){
    
    lapply(1:4, function(i, k){
      
      
      m <- 5
      
      # pooled predictions could be really choppy because of how 
      # continuous the distribution of x is (i.e. 1381 rows of x from -4 to 5)
      # overfitting?
      # or that predictions are really different across m
      
      fitted_lines[[k]][[i]] %>% 
        group_by(x#, group
        ) %>% # x = temp.change, group = precipitation.change
        summarise(fit_bar = mean(predicted),
                  v_w     = mean(std.error^2),
                  v_b     = sum((predicted - fit_bar)^2) / (m - 1),
                  v_p     = v_w + v_b * (1 + (1 / m)),
                  se_p    = sqrt(v_p)) %>% 
        # use the _p suffix to indicate these are pooled
        mutate(lwr_p = fit_bar - se_p * 1.96,
               upr_p = fit_bar + se_p * 1.96) 
      
    }, k)
    
  })
  
}

# unlist grouped_lines list of dfs to make faceted plotting easier

rbind_grouped_lines <- function(grouped_lines){
  
  lapply(1:5, function(k){
    rbindlist(grouped_lines[[k]], idcol="crop") %>% 
      mutate(crop=as.factor(crop))
  })
  
  
  
}

# plot pooled lines
plot_pooled_lines <- function(predictions, data){
  
  lapply(1:5, function(k){
    
    AGIMPACTS_bs_restricted <- data %>% 
      filter(Temp.Change >=0 & Temp.Change <=5) %>% 
      rename(x=Temp.Change,
             fit_bar=Yield.Change)  %>% 
      mutate(crop=case_when(crop_pooled=="Maize" ~ "1",
                            crop_pooled=="Rice" ~ "2",
                            crop_pooled=="Soybean" ~ "3",
                            crop_pooled=="Wheat" ~ "4"))
    
    # crop facet labels
    crop_labs <- c("Maize", "Rice", "Soy", "Wheat")
    names(crop_labs) <- c("1","2","3","4")
    # precipitation change facet labels
    #group_labs <- c("-100%", "0%", "100%")
    #names(group_labs) <- c("-100", "0", "100")
    
    
    #predictions %>% 
    # filter(x>=0 & x<=5) %>% 
    ggplot() +
      geom_line(data = predictions[[k]][x>=0 & x<=5,],
                aes(x=x, y = fit_bar), 
                col="black",
                size=1) +
      geom_ribbon(data = predictions[[k]][x>=0 & x<=5,],
                  aes(x=x, ymin = lwr_p, ymax = upr_p,
                      fill = crop),
                  alpha = 1/2) +
      theme_bw() +
      theme(legend.position="none") +
      labs(x="Temperature Change (°C)",
           y="Fitted Yield Change (%)") +
      # add the observed data for good measure
      # note this doesn't differentiate by precipitation change or any other covariates
      geom_point(data = AGIMPACTS_bs_restricted,
                 aes(x = x, y = fit_bar,
                     col = crop),
                 alpha=1/2
      ) +
      facet_wrap(facets=vars(crop),
                 labeller=labeller(crop=crop_labs))
    #facet_grid(cols=vars(crop),
    #           rows=vars(group),
    #           labeller = labeller(crop=crop_labs#,
    #                               #group=group_labs
    #                               )) 
    
  })
  
  
}

# save plots

save_plots_reponse_functions <- function(plots, path, spec_no){
  
  # save plots as png
  #png(paste(path,j,l, ".png", sep="_"))
  
  ggsave(filename=path,
         plot=plots[[spec_no]],
         width=7, height=6)
  
  
  #dev.off()
  #paste(path,j,l, ".png", sep="_")
  
}

# plot all fitted response functions on same plot

plot_all_response_functions <- function(predictions, path){
  
  # make sure this is discrete scale
  predictions$model_spec <- as.factor(predictions$model_spec)
  # crop facet labels
  crop_labs <- c("Maize", "Rice", "Soy", "Wheat")
  names(crop_labs) <- c("1","2","3","4")
  #model_spec_labs <- c("gam_RS", "gam_RI", "glm_RS", "glm_RI", "lm")
  #names(model_spec_labs) <- c("1","2","3","4","5")
  
  plot <- ggplot() +
    geom_line(data = predictions[x>=0 & x<=5,],
              aes(x=x, y = fit_bar, colour = model_spec), 
              size=1) +
    geom_ribbon(data = predictions[x>=0 & x<=5,],
                aes(x=x, ymin = lwr_p, ymax = upr_p,
                    fill = model_spec),
                alpha = 0.1) +
    theme_bw() +
    #theme(legend.position="none") +
    labs(x="Temperature Change (°C)",
         y="Fitted Yield Change (%)") +
    scale_colour_discrete(
      name = "Model",
      breaks=c("1","2","3","4","5"),
      labels=c("GAM RS", "GAM RI", "GLM RS", "GLM RI", "LM")
    ) +
    scale_fill_discrete(
      name = "Model",
      breaks=c("1","2","3","4","5"),
      labels=c("GAM RS", "GAM RI", "GLM RS", "GLM RI", "LM")
    ) +
    facet_wrap(facets=vars(crop),
               labeller=labeller(crop=crop_labs))
  
  ggplot2::ggsave(filename=path,
                  plot=plot,
                  width=7, height=6)
  
  plot
  
  
  
}

plot_all_response_functions_with_data <- function(predictions, data, path){
  
  # make sure this is discrete scale
  predictions$model_spec <- as.factor(predictions$model_spec)
  # crop facet labels
  crop_labs <- c("Maize", "Rice", "Soy", "Wheat")
  names(crop_labs) <- c("1","2","3","4")

  AGIMPACTS_bs_restricted <- data %>% 
    filter(Temp.Change >=0 & Temp.Change <=5) %>% 
    rename(x=Temp.Change,
           fit_bar=Yield.Change)  %>% 
    mutate(crop=case_when(crop_pooled=="Maize" ~ "1",
                          crop_pooled=="Rice" ~ "2",
                          crop_pooled=="Soybean" ~ "3",
                          crop_pooled=="Wheat" ~ "4"))
  
  
  plot <- ggplot() +
    geom_line(data = predictions[x>=0 & x<=5,],
              aes(x=x, y = fit_bar, colour = model_spec), 
              size=1) +
    geom_ribbon(data = predictions[x>=0 & x<=5,],
                aes(x=x, ymin = lwr_p, ymax = upr_p,
                    fill = model_spec),
                alpha = 0.1) +
    theme_bw() +
    #theme(legend.position="none") +
    labs(x="Temperature Change (°C)",
         y="Fitted Yield Change (%)") +
    scale_colour_discrete(
      name = "Model",
      breaks=c("1","2","3","4","5"),
      labels=c("GAM RS", "GAM RI", "GLM RS", "GLM RI", "LM")
    ) +
    scale_fill_discrete(
      name = "Model",
      breaks=c("1","2","3","4","5"),
      labels=c("GAM RS", "GAM RI", "GLM RS", "GLM RI", "LM")
    ) +
  #add the observed data for good measure
  #note this doesn't differentiate by precipitation change or any other covariates
  geom_point(data = AGIMPACTS_bs_restricted,
             aes(x = x, y = fit_bar),
             alpha=0.2, size=0.5, col="black"
  ) +
  facet_wrap(facets=vars(crop),
             labeller=labeller(crop=crop_labs))
  
  ggplot2::ggsave(filename=path,
                  plot=plot,
                  width=7, height=6)
  
  plot
  
  
}
