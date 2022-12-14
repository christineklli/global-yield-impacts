#### fit gam and glm  ####
# fit with mgcv::gam() instead of lme4::lmer()
# note that this means we cannot fit correlated random intercept and slopes, or nested RE intercepts
# however, given the boundary singularity fit flagged for some of the models on rice and soy data
# there is likely not enough random effects variance to do anything complex with the country RE anyway
# so we are going to fit the simplest mixed effect models - just random country and study intercepts

# at a later point, consider modelling Bayesian to resolve the singularity fit - brms() and rstanarm()

# for now, proceed with random intercept country and study model, on four models with forced zero intercept
# varying response: absolute yield change and relative yield change
# model: glmm and gamm

# plot response functions

# prepare prediction data by downloading gridded CMIP6 RCP8.5 data to 2100 from KMNI
# fit one model at a time, pool over models (median, 25%, 75% pct), pool over m1-m5 (median, 25%, 75%)
# for the two absolute yield change models, convert back to % yield change using rasters of baseline yield in 2015-2020
# note we only have raster yield data up to 2016 at latest

# if storage or time issues, then triage by running only on m1 data, if still not enough, then run only gamm or glmm on m1 data
# however, set up code infrastructure to implement all m1-m5 and all 4 model specifications (i.e. in lists)
# so that this can be done when SSD is upgraded (or with external SSD or VM) fairly easily


# READ IN DATA IF NOT FOLLOWING DIRECTLY FROM 10 ------------------------------------------------------------

crops <- c("Maize", "Rice", "Soybean", "Wheat")

crop_imputed_data_restricted <- lapply(1:4, function(j){
  lapply(1:5, function(i,j){
    readRDS(here("processed", paste0("crop_data_", crops[[j]], "_", i, ".Rdata")))
  }, j)
})

AGIMPACTS_bs_tp_yields <- readRDS(here("processed","AGIMPACTS_bs_tp_yields.Rdata"))

# split unimputed dataset into 4 datasets by crops[[j]]

AGIMPACTS_crops <- AGIMPACTS_bs_tp_yields %>% 
  mutate(Abs.Yield.Change = Yield.Change*Baseline_yield/100) %>%  # unit of measurement 
  mutate(Pct.Precipitation.Change = Precipitation.change/100) %>% 
  mutate(Yield.Level = Baseline_yield*(1+(Yield.Change/100))) %>%
  group_split(crop_pooled)

# FIT GAMM --------------------------------------------------------------

# write function to fit models based on list of formulas - just have a single model fitting function for each crop
# note that we have not done cross-validation on te() interaction variables for temp:precip, just kept to linear interaction

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
        
# not necessary to do this, can just save entire crop_models as RData
lapply(1:4, function(k){ # formula = k - highest level in three-level list
  lapply(1:4, function(j, k){ # crop = j
    lapply(1:5, function(i,j,k){
      saveRDS(crop_models[[k]][[j]][[i]], 
              here("results", "models", paste("crop_model", k, crops[[j]], i, ".RData", sep="_")))
    }
    , j, k) # m = i
  }, k)}) 


# plot response functions -------------------------------------------------

# we need to cut the response functions multiple ways
# first group of crop-facet plots: do what was done before, represent m1-m5 for each crop on a 2x2 grid at precip.change = median
# second group of crop-facet plots: choose m1, and represent temp.change by groups of precip.change for each crop on a 2x2 grid 
# in both plots, bs temp and bs precipitation are set at median, then temp 5 (precip=median), 15 (precip=median) and 25 (precip=median) degrees  
# so for each of 4 bs 'scenarios', we would like to generate 2 different plots representing m1-m5 @ median precip change and precip interactions
# so we should end up with 8 crop-facet plots


# at median values of precipitation change (dpmd)------------------------------------------

# planned steps:
# 1. use ggpredict for predicted response in abs/relative yield change for m1-m5 at precip.change = median (dpmd), bs temp = median, bs pre = median
# 2. plot
# 3. can repeat for different values of bs temp [5,15,25]

# 1. predict responses
predict_responses_dpmd <- function(i,j,k){
  
  ggpredict(crop_models[[k]][[j]][[i]],
                                terms = c("Temp.Change"),
                                typical = "median", # bs temp and bs precip and precipitation change
                                condition = c(adapt_dummy = 0, f_CO2 = 0, 
                                              Reference_fact = 0, Country2_fact = 0),
                                type = "random") 
  # intercept will be determined by coef(adapt_dummy0) and population level mean random intercepts
  }

predicted_responses_dpmd <- lapply(1:4, function(k){ # formula = k - highest level in three-level list
  lapply(1:4, function(j, k){ # crop = j
    lapply(1:5, predict_responses_dpmd, j, k) # m = i
  }, k)}) 

# 2. plot

# each [[k]] represents different model & must be plotted separately/sequentially
# also plot against imputed data (plotting against unimputed data results in no data for soybean, see explanation below)

response <- c("Yield.Change", "Abs.Yield.Change", "Yield.Change", "Abs.Yield.Change")


plot_predicted_responses_dpmd <- function(j, k){
  
  ggplot() + 
    geom_line(data = predicted_responses_dpmd[[k]][[j]][[1]], linetype = "solid", aes(x, predicted), color = "black", size=0.7) + # adjusted damage function
    geom_line(data = predicted_responses_dpmd[[k]][[j]][[2]], linetype = "solid", aes(x, predicted), color = "#355EF2", size=0.7) + # blue
    geom_line(data = predicted_responses_dpmd[[k]][[j]][[3]], linetype = "solid", aes(x, predicted), color = "#CFB617", size=0.7) + # yellow
    geom_line(data = predicted_responses_dpmd[[k]][[j]][[4]], linetype = "solid", aes(x, predicted), color = "#CF1738", size=0.7) + # red
    geom_line(data = predicted_responses_dpmd[[k]][[j]][[5]], linetype = "solid", aes(x, predicted), color = "#5CC74E", size=0.7) + # green
    geom_ribbon(data = predicted_responses_dpmd[[k]][[j]][[1]], aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#464A4D", alpha=0.1) + # adjusted damage function
    geom_ribbon(data = predicted_responses_dpmd[[k]][[j]][[2]], aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#7BB9E8", alpha=0.1) +
    geom_ribbon(data = predicted_responses_dpmd[[k]][[j]][[3]], aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#E8E084", alpha=0.1) +
    geom_ribbon(data = predicted_responses_dpmd[[k]][[j]][[4]], aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#E87B7B", alpha=0.1) +
    geom_ribbon(data = predicted_responses_dpmd[[k]][[j]][[5]], aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#90C78D", alpha=0.1) +
   #geom_point(data = crop_imputed_data_restricted[[j]][[1]], aes(x=Temp.Change, y=eval(rlang::parse_expr(response[[k]]))), size = 0.5, alpha = 0.3) +
    scale_x_continuous(limits = c(0, 5)) +
    # scale_y_continuous(limits = c(-50, 50)) +
    theme_test() +
    geom_hline(yintercept = 0) +
    xlab("Temperature Change (degrees C)") +
    ylab("Yield Change") +
    theme(legend.position = "none",
          text=element_text(size=15),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(crops[[j]])
  
}

# we can save this dynamically with and without data  to reduce code duplication/length
# with and without line 184 and line 223 commented out 

plot_responses_dpmd <- lapply(1:4, function(k){ # for each model
  lapply(1:4, plot_predicted_responses_dpmd, k)}) # for each crop

model_response_plots_dpmd <- lapply(1:4, function(k){
 
   grid.arrange(plot_responses_dpmd[[k]][[1]],
               plot_responses_dpmd[[k]][[2]],
               plot_responses_dpmd[[k]][[3]],
               plot_responses_dpmd[[k]][[4]],
               ncol = 2, 
               nrow = 2)
  
})

lapply(1:4, function(k){
  
  g <- arrangeGrob(plot_responses_dpmd[[k]][[1]],
               plot_responses_dpmd[[k]][[2]],
               plot_responses_dpmd[[k]][[3]],
               plot_responses_dpmd[[k]][[4]],
               ncol = 2, 
               nrow = 2)
  #ggsave(here("results", "figures", "response functions", paste("precip_change_median_data", k, ".png", sep="_")), g) # with m1 data only 
  ggsave(here("results", "figures", "response functions", paste("precip_change_median_nodata", k, ".png", sep="_")), g)
  
})

# e.g. model_response_plots[[1]] is model 1, model_response_plots[[2]] is model 2,.. in model_formulas list
# unsurprisingly, model_response_plots[[1]] looks most similar to previous main model response functions bc model spec is closest
# except that here, precipitation change is set at median values rather than zero
# soybean data is missing for Abs.Yield.Change because in the unimputed data, soybean baseline periods are all pre-1981
# but our yield data only begins at 1981
# however, imputed datasets then impute this...
# therefore when plotting with data, instead of plotting raw unimputed AGIMPACTS_crops data, we plot m1 imputed dataset 

# at varying values of precipitation change (dpvar) for m1----------------------------------

# use ggpredict for predicted response in abs/relative yield change for m1 by precip.change[-100,0,100], bs temp = median, bs pre = median
# plot
# # can repeat for different values of bs temp [5,15,25]


predict_responses_dpvar <- function(i,j,k){
  
  ggpredict(crop_models[[k]][[j]][[i]],
            terms = c("Temp.Change", "Precipitation.change [-100,0,100]"),
            typical = "median", # bs temp and bs precip and precipitation change
            condition = c(adapt_dummy = 0, f_CO2 = 0, 
                          Reference_fact = 0, Country2_fact = 0),
            type = "random") 
  # intercept will be determined by coef(adapt_dummy0) and population level mean random intercepts
}

predicted_responses_dpvar <- lapply(1:4, function(k){ # formula = k - highest level in three-level list
  lapply(1:4, function(j, k){ # crop = j
    lapply(1:5, predict_responses_dpvar, j, k) # m = i
  }, k)}) 


# 2. plot

# each [[k]] represents different model & must be plotted separately/sequentially
# also plot against imputed data (plotting against unimputed data results in no data for soybean, see explanation below)

response <- c("Yield.Change", "Abs.Yield.Change", "Yield.Change", "Abs.Yield.Change")


plot_predicted_responses_dpvar <- function(j, k){
  
  ggplot() + 
    geom_line(data = predicted_responses_dpvar[[k]][[j]][[1]], linetype = "solid", aes(x, predicted, color=group), size=0.7) + # color by precip change value
    geom_ribbon(data = predicted_responses_dpvar[[k]][[j]][[1]], aes(x=x,ymin=conf.low, ymax=conf.high, fill=group), alpha=0.1) + # fill by precip change value
    scale_x_continuous(limits = c(0, 5)) +
    # scale_y_continuous(limits = c(-50, 50)) +
    theme_test() +
    geom_hline(yintercept = 0) +
    xlab("Temperature Change (degrees C)") +
    ylab("Yield Change") +
    labs(colour="Precipitation change (%)") + # resave images
    guides(fill = "none") +
    theme(legend.position = "none",
          text=element_text(size=15),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(crops[[j]])
  
}

plot_responses_dpvar <- lapply(1:4, function(k){ # for each model
  lapply(1:4, plot_predicted_responses_dpvar, k)}) # for each crop

model_response_plots_dpvar <- lapply(1:4, function(k){
  
  lemon::grid_arrange_shared_legend(plot_responses_dpvar[[k]][[1]],
                                     plot_responses_dpvar[[k]][[2]],
                                     plot_responses_dpvar[[k]][[3]],
                                     plot_responses_dpvar[[k]][[4]],
                                     ncol = 2, 
                                     nrow = 2,
                                     position="top")
  
})

# save


lapply(1:4, function(k){
  
  g <- lemon::grid_arrange_shared_legend(plot_responses_dpvar[[k]][[1]],
                                    plot_responses_dpvar[[k]][[2]],
                                    plot_responses_dpvar[[k]][[3]],
                                    plot_responses_dpvar[[k]][[4]],
                                    ncol = 2, 
                                    nrow = 2,
                                    position="top")
  
  ggsave(here("results", "figures", "response functions", paste("precip_change_varying", "m1", k, ".png", sep="_")), g)
  
})


# with the exception of rice, all other 3 crops show that temperature and precipitation interact negatively
# i.e. -100 dP results in larger temperature yield decrease than 100 dP
# but do so in different ways - for maize this looks like a slope interaction; for rice an intercept interaction
# for soybean and wheat both intercept and slope  


# try later - code to make universal axis titles --------------------------


# https://stackoverflow.com/questions/11076567/plot-a-legend-and-well-spaced-universal-y-axis-and-main-titles-in-grid-arrange
model_response_plots_dpvar <- lapply(1:4, function(k){
  
legend = gtable::gtable_filter(ggplotGrob(plot_responses_dpvar[[1]][[1]]), "guide-box") 
 #grid.draw(legend)    # Make sure the legend has been extracted

# Arrange the elements to be plotted. 
# The inner arrangeGrob() function arranges the four plots, the main title, 
#   and the global y-axis title.
# The outer grid.arrange() function arranges and draws the arrangeGrob object and the legend.
  grid.arrange(arrangeGrob(plot_responses_dpvar[[k]][[1]] + theme(legend.position="none"), 
                         plot_responses_dpvar[[k]][[2]] + theme(legend.position="none"),
                         plot_responses_dpvar[[k]][[3]] + theme(legend.position="none"),
                         plot_responses_dpvar[[k]][[4]] + theme(legend.position="none"), 
                         nrow = 2, ncol = 2,
                         top = textGrob("Main Title", vjust = 1, gp = gpar(fontface = "bold", cex = 1.5)),
                         left = textGrob("Yield Change", rot = 90, vjust = 1)), 
             legend, 
             widths=unit.c(unit(1, "npc") - legend$width, legend$width)
        )
})
