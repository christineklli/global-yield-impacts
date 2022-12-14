# this script is deprecated by script 10_1_modelfit which has code for plotting response functions

# read in models ----------------------------------------------------------

crops <- c("Maize", "Rice", "Soybean", "Wheat")

crop_gamm <- lapply(1:2, function(k){ # formula = k - model 1 is linear T*P interaction, model 2 is nonlinear int.
  lapply(1:4, function(j, k){ # crop = j
    lapply(1:5, function(i,j,k){
      readRDS(here("results", "models", paste("crop_gamm_", k, crops[[j]], i, ".RData", sep="_")))
    }
    , j, k) # m = i
  }, k)}) 

crop_glmm <- lapply(1:4, function(k){ # formula = k - model 1 is linear T*P interaction, model 2 is nonlinear int.
  lapply(1:4, function(j, k){ # crop = j
    lapply(1:5, function(i,j,k){
      readRDS(here("results", "models", paste("crop_glmm_", k, crops[[j]], i, ".RData", sep="_")))
    }
    , j, k) # m = i
  }, k)}) 


# PRINT MODEL RESULTS -----------------------------------------------------

# crop_gamm 

summary_fit_gamm <- lapply(1:2, function(k){ # formula = k - model 1 is linear T*P interaction, model 2 is nonlinear int.
  lapply(1:4, function(j, k){ # crop = j
    lapply(1:5, function(i,j,k){
      
      summary(crop_gamm[[k]][[j]][[i]])
    }
    , j, k) # m = i
  }, k)}) 

sink(here("results", "text files", "summary_fit_gamm.txt"))
summary_fit_gamm
sink()

# crop_glmm

summary_fit_glmm <- lapply(1:4, function(k){ # formula = k - model 3 and 4 most interesting
  lapply(1:4, function(j, k){ # crop = j
    lapply(1:5, function(i,j,k){
      summary(crop_glmm[[k]][[j]][[i]])
    }
    , j, k) # m = i
  }, k)}) 

sink(here("results", "text files", "summary_fit_glmm.txt"))
summary_fit_glmm
sink()


# PLOT DIAGNOSTICS  -------------------------------------------------------

# redo these in a function to reduce repeated code 
# also note these are for gam, not for glm?

# MAIZE
gratia::appraise(crop_gamm[[1]][[1]][[1]])

ggsave(here("results", "figures", "maize_diagnostics_reweighted.png"))

plot.gam(crop_gamm[[1]][[1]], pages=1)

ggsave(here("results", "figures", "maize_plot_reweighted.png"))

gratia::appraise(fit_weighted_rice_restricted[[1]][[1]])

ggsave(here("results", "figures", "rice_diagnostics_reweighted.png"))

plot.gam(fit_weighted_rice_restricted[[1]][[1]], pages=1)

ggsave(here("results", "figures", "rice_plot_reweighted.png"))

gratia::appraise(fit_weighted_soy_restricted[[1]][[1]])

ggsave(here("results", "figures", "soy_diagnostics_reweighted.png"))

plot.gam(fit_weighted_soy_restricted[[1]][[1]], pages=1)

ggsave(here("results", "figures", "soy_plot_reweighted.png"))

gratia::appraise(fit_weighted_wheat_restricted[[1]][[1]])

ggsave(here("results", "figures", "wheat_diagnostics_reweighted.png"))

plot.gam(fit_weighted_wheat_restricted[[1]][[1]], pages=1)

ggsave(here("results", "figures", "wheat_plot_reweighted.png"))


# PLOT RESPONSE FUNCTIONS -------------------------------------------------

# must decide - do we do this at different values of precipitation change and baseline temperature? 
# and f_Co2???
# how to represent all these partial effects?
# m are significant enough that we should plot all m's as well

# we need to cut the response functions multiple ways
# first group of crop-facet plots: do what was done before, represent m1-m5 for each crop on a 2x2 grid at precip.change = median
# second group of crop-facet plots: choose m1, and represent temp.change by groups of precip.change for each crop on a 2x2 grid 
# in both plots, bs temp and bs precipitation are set at median, then temp 5 (precip=median), 15 (precip=median) and 25 (precip=median) degrees  
# so for each of 4 bs 'scenarios', we would like to generate 2 different plots representing m1-m5 @ median precip change and precip interactions
# so we should end up with 8 crop-facet plots

# try this other way - expand data frame and predict 
# https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/

tidyr::expand(crop_imputed_data_restricted[[1]][[1]][[1]], Temp.Change = unique(Temp.Change)) # doesn't seem to work on double/numeric classes


# At median baseline temperature - without observed data ----------------

# glmm isn't resulting in the calculation of CIs

predicted_yield <-  ggpredict(trial_gamm[[1]][[1]][[1]],
                              # terms = c("Temp.Change", "Baseline_tmp_weighted [median]"), 
                              terms = c("Temp.Change", "Precipitation.change [-100,0,100]"),
                              typical = "median", # THIS IS WHAT SETS BASELINE TEMP TO MEDIAN
                              condition = c(adapt_dummy = 0,  f_CO2 = 0, # is it reasonable to set f_CO2 to 0 for plotting the response curves?
                                            Reference_fact = 0, Country2_fact = 0),
                              type = "random") # random effect variances no difference


predicted_yield <-  ggpredict(trial_gamm[[1]][[1]][[1]],
                              # terms = c("Temp.Change", "Baseline_tmp_weighted [median]"), 
                              terms = c("Temp.Change", "Precipitation.change [-100,0,100]"),
                              typical = "median", # THIS IS WHAT SETS BASELINE TEMP TO MEDIAN
                              condition = c(adapt_dummy = 0,  f_CO2 = 0),
                              type = "random") # this does evaluate everything at population level (Ref_fact = 0, country = 0)

# note that before, it was necessary to adjust by coef(adapt_dummy)

predicted_yield$predicted <- predicted_yield$predicted  - (- coef(trial_gamm[[1]][[1]][[1]])[1] + coef(trial_gamm[[1]][[1]][[1]])[12] + coef(trial_gamm[[1]][[1]][[1]])[53])

predicted_yield

# there are three diff values for precip.change, how to reflect these in ggplot?
# but also do not want to represent all values of precip.change in the same plot - but instead in different plots?
# or should we actually pick m1 and represent the different slopes at different interactions of precipitation etc.?

ggplot() + 
  geom_line(data = predicted_yield, linetype = "solid", aes(x, predicted, color = group)) + 
  scale_x_continuous(limits = c(0, 5)) +
  # scale_y_continuous(limits = c(-50, 50)) +
  theme_test() +
  geom_hline(yintercept = 0) +
  xlab("Temperature Change (degrees C)") +
  ylab("Yield Change (%)") +
  theme(legend.position = "none",
        text=element_text(size=15),
        plot.title = element_text(hjust = 0.5)) 


ggpredict_crops_weighted_restricted_median <- function(i,j){
  
  predicted_yield <-  ggpredict(fit_weighted_restricted_list[[j]][[1]][[i]],
                                # terms = c("Temp.Change", "Baseline_tmp_weighted [median]"), 
                                terms = c("Temp.Change"),
                                typical = "median", # THIS IS WHAT SETS BASELINE TEMP TO MEDIAN
                                condition = c(adapt_dummy = 0, Precipitation.change = 0, f_CO2 = 0, 
                                              Reference_fact = 0, Country2_fact = 0),
                                type = "random") # random effect variances no difference
  
  
}

# https://search.r-project.org/CRAN/refmans/ggeffects/html/ggpredict.html

ggpredicted_crops_weighted_restricted_median <- lapply(1:4, function(j){lapply(1:5, ggpredict_crops_weighted_restricted_test, j)})

# plot(ggpredict_crops_15[[1]][[1]]) + plot(ggpredict_crops_15[[1]][[2]])
plot_response_crops_weighted_restricted_median <- function(i){
  
  ggplot() + 
    geom_line(data = ggpredicted_crops_weighted_restricted_median[[i]][[1]], linetype = "solid", aes(x, predicted), color = "black", size=0.7) + # adjusted damage function
    geom_line(data = ggpredicted_crops_weighted_restricted_median[[i]][[2]], linetype = "solid", aes(x, predicted), color = "#355EF2", size=0.7) + # blue
    geom_line(data = ggpredicted_crops_weighted_restricted_median[[i]][[3]], linetype = "solid", aes(x, predicted), color = "#CFB617", size=0.7) + # yellow
    geom_line(data = ggpredicted_crops_weighted_restricted_median[[i]][[4]], linetype = "solid", aes(x, predicted), color = "#CF1738", size=0.7) + # red
    geom_line(data = ggpredicted_crops_weighted_restricted_median[[i]][[5]], linetype = "solid", aes(x, predicted), color = "#5CC74E", size=0.7) + # green
    geom_ribbon(data = ggpredicted_crops_weighted_restricted_median[[i]][[1]], aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#464A4D", alpha=0.1) + # adjusted damage function
    geom_ribbon(data = ggpredicted_crops_weighted_restricted_median[[i]][[2]], aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#7BB9E8", alpha=0.1) +
    geom_ribbon(data = ggpredicted_crops_weighted_restricted_median[[i]][[3]], aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#E8E084", alpha=0.1) +
    geom_ribbon(data = ggpredicted_crops_weighted_restricted_median[[i]][[4]], aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#E87B7B", alpha=0.1) +
    geom_ribbon(data = ggpredicted_crops_weighted_restricted_median[[i]][[5]], aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#90C78D", alpha=0.1) +
    
    scale_x_continuous(limits = c(0, 5)) +
    # scale_y_continuous(limits = c(-50, 50)) +
    theme_test() +
    geom_hline(yintercept = 0) +
    xlab("Temperature Change (degrees C)") +
    ylab("Yield Change (%)") +
    theme(legend.position = "none",
          text=element_text(size=15),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(crops[[i]])
  
}

# NOTE - TO INCLUDE WEIGHTS VISUALLY, MUST USE A DATASET THAT HAS THE SE IN IT;
# AGIMPACTS_FINAL_INCOMPLETE DOES NOT
# because SE is calculated on the imputed datasets!

plots_response_crops_weighted_restricted_median <- lapply(1:4, plot_response_crops_weighted_restricted_median)

grid.arrange(plots_response_crops_weighted_restricted_median[[1]],
             plots_response_crops_weighted_restricted_median[[2]],
             plots_response_crops_weighted_restricted_median[[3]],
             plots_response_crops_weighted_restricted_median[[4]],
             ncol = 2, 
             nrow = 2)

ggsave(here("results", "figures", "response-functions-reweighted-median-no-data.png"))

# At dynamically set baseline temperatures - without observed data --------


ggpredict_crops_weighted_restricted <- function(i,j){
  
  predicted_yield <-  ggpredict(fit_weighted_restricted_list[[j]][[1]][[i]],
                                # terms = c("Temp.Change", "Baseline_tmp_weighted [median]"), 
                                terms = c("Temp.Change"),
                                # typical = "median", # THIS IS WHAT SETS BASELINE TEMP TO MEDIAN
                                condition = c(Baseline_tmp_weighted = 25, adapt_dummy = 0, Precipitation.change = 0, f_CO2 = 0, 
                                              Reference_fact = 0, Country2_fact = 0),
                                type = "random") # random effect variances no difference
  
  
}

# https://search.r-project.org/CRAN/refmans/ggeffects/html/ggpredict.html

ggpredicted_crops_weighted_restricted <- lapply(1:4, function(j){lapply(1:5, ggpredict_crops_weighted_restricted, j)})

# plot(ggpredict_crops_15[[1]][[1]]) + plot(ggpredict_crops_15[[1]][[2]])
plot_response_crops_weighted_restricted <- function(i){
  
  ggplot() + 
    geom_line(data = ggpredicted_crops_weighted_restricted[[i]][[1]], linetype = "solid", aes(x, predicted), color = "black", size=0.7) + # adjusted damage function
    geom_line(data = ggpredicted_crops_weighted_restricted[[i]][[2]], linetype = "solid", aes(x, predicted), color = "#355EF2", size=0.7) + # blue
    geom_line(data = ggpredicted_crops_weighted_restricted[[i]][[3]], linetype = "solid", aes(x, predicted), color = "#CFB617", size=0.7) + # yellow
    geom_line(data = ggpredicted_crops_weighted_restricted[[i]][[4]], linetype = "solid", aes(x, predicted), color = "#CF1738", size=0.7) + # red
    geom_line(data = ggpredicted_crops_weighted_restricted[[i]][[5]], linetype = "solid", aes(x, predicted), color = "#5CC74E", size=0.7) + # green
    geom_ribbon(data = ggpredicted_crops_weighted_restricted[[i]][[1]], aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#464A4D", alpha=0.1) + # adjusted damage function
    geom_ribbon(data = ggpredicted_crops_weighted_restricted[[i]][[2]], aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#7BB9E8", alpha=0.1) +
    geom_ribbon(data = ggpredicted_crops_weighted_restricted[[i]][[3]], aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#E8E084", alpha=0.1) +
    geom_ribbon(data = ggpredicted_crops_weighted_restricted[[i]][[4]], aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#E87B7B", alpha=0.1) +
    geom_ribbon(data = ggpredicted_crops_weighted_restricted[[i]][[5]], aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#90C78D", alpha=0.1) +
    #  geom_point(data = split_crops_data[[i]], aes(x=Temp.Change, y=Yield.Change), size = 0.5, alpha = 0.3) +
    scale_x_continuous(limits = c(0, 5)) +
    # scale_y_continuous(limits = c(-50, 50)) +
    theme_test() +
    geom_hline(yintercept = 0) +
    xlab("Temperature Change (degrees C)") +
    ylab("Yield Change (%)") +
    theme(legend.position = "none",
          text=element_text(size=15),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(crops[[i]])
  
}

# NOTE - TO INCLUDE WEIGHTS VISUALLY, MUST USE A DATASET THAT HAS THE SE IN IT;
# AGIMPACTS_FINAL_INCOMPLETE DOES NOT
# because SE is calculated on the imputed datasets!

plots_response_crops_weighted_restricted <- lapply(1:4, plot_response_crops_weighted_restricted)

grid.arrange(plots_response_crops_weighted_restricted[[1]],
             plots_response_crops_weighted_restricted[[2]],
             plots_response_crops_weighted_restricted[[3]],
             plots_response_crops_weighted_restricted[[4]],
             ncol = 2, 
             nrow = 2)

ggsave(here("results", "figures", "response-functions-reweighted-5.png"))
ggsave(here("results", "figures", "response-functions-reweighted-15.png"))
ggsave(here("results", "figures", "response-functions-reweighted-25.png"))

# At median baseline temperature - with observed data ---------------------

# READ IN AGIMPACTS_FINAL_INCOMPLETE FROM SCRIPT 08

# create crop-specific datasets for AGIMPACTS_FINAL_INCOMPLETE
split_crops <- function(i){
  
  AGIMPACTS_FINAL_INCOMPLETE %>% 
    filter(crop_pooled == crops[i])
  
}

split_crops_data <- lapply(1:4, split_crops)

plot_observed_predicted_restricted <- function(i){
  
  ggplot() + 
    geom_line(data = ggpredicted_crops_weighted_restricted_median[[i]][[1]], linetype = "solid", aes(x, predicted), color = "black", size=0.7) + # adjusted damage function
    geom_line(data = ggpredicted_crops_weighted_restricted_median[[i]][[2]], linetype = "solid", aes(x, predicted), color = "#355EF2", size=0.7) + # blue
    geom_line(data = ggpredicted_crops_weighted_restricted_median[[i]][[3]], linetype = "solid", aes(x, predicted), color = "#CFB617", size=0.7) + # yellow
    geom_line(data = ggpredicted_crops_weighted_restricted_median[[i]][[4]], linetype = "solid", aes(x, predicted), color = "#CF1738", size=0.7) + # red
    geom_line(data = ggpredicted_crops_weighted_restricted_median[[i]][[5]], linetype = "solid", aes(x, predicted), color = "#5CC74E", size=0.7) + # green
    geom_ribbon(data = ggpredicted_crops_weighted_restricted_median[[i]][[1]], aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#464A4D", alpha=0.1) + # adjusted damage function
    geom_ribbon(data = ggpredicted_crops_weighted_restricted_median[[i]][[2]], aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#7BB9E8", alpha=0.1) +
    geom_ribbon(data = ggpredicted_crops_weighted_restricted_median[[i]][[3]], aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#E8E084", alpha=0.1) +
    geom_ribbon(data = ggpredicted_crops_weighted_restricted_median[[i]][[4]], aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#E87B7B", alpha=0.1) +
    geom_ribbon(data = ggpredicted_crops_weighted_restricted_median[[i]][[5]], aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#90C78D", alpha=0.1) +
    geom_point(data = split_crops_data[[i]], aes(x=Temp.Change, y=Yield.Change), size = 0.5, alpha = 0.3) + # OBSERVED DATA ONLY - NO IMPUTATIONS
    scale_x_continuous(limits = c(0, 5)) +
    scale_y_continuous(limits = c(-50, 50)) +
    theme_test() +
    geom_hline(yintercept = 0) +
    xlab("Temperature Change (degrees C)") +
    ylab("Yield Change (%)") +
    theme(legend.position = "none",
          text=element_text(size=15),
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(crops[[i]])
  
}

plots_response_observed_weighted_restricted <- lapply(1:4, plot_observed_predicted_restricted)

grid.arrange(plots_response_observed_weighted_restricted[[1]],
             plots_response_observed_weighted_restricted[[2]],
             plots_response_observed_weighted_restricted[[3]],
             plots_response_observed_weighted_restricted[[4]],
             ncol = 2, 
             nrow = 2)

ggsave(here("results", "figures", "response-functions-reweighted-median-with-data.png"))



