
# make sure run 13_cmip5_prediction_data first ----------------------------

# stratified bootstrapping ------------------

# note the wrong labelling - anything with 'cluster' is actually stratified, 
# whereas the labels in next section with no cluster are actually cluster bootstrapped

# now try stratified sampling - THIS MEANS WE ARE RESAMPLING WITHIN EACH STUDY - BUT THE SAME 40 STUDIES ARE REPRESENTED IN EVERY BOOTSTRAP

bs_stratified <- bootstraps(AGIMPACTS_TRUNCATED_DATA, times = 750, strata = Reference) 

# look at samples to check that every study is represented, but different point estimates from each study

unique(as_tibble(bs_stratified$splits[[1]])[11]) %>% print(n=Inf) # cycle through [[1:750]] to confirm; [[11]] is the Ref column

bootstrap_regression_stratified_predict <- function(i){
  
  split_tibble <- as_tibble(bs_stratified$splits[[i]]) # 3633 point estimates for each bootstrap
  
  damages <- lm(Yield.Change ~ 0 + Temp.Change:crop_pooled + I(Temp.Change^2):crop_pooled +
                  Temp.Change:crop_pooled:Baseline_tmp_weighted + 
                  I(Temp.Change^2):crop_pooled:Baseline_tmp_weighted +
                  # crop_pooled:Baseline_tmp_weighted + ### NOTE THIS CHANGE ###
                  f_CO2:C3 + f_CO2:C4  + 
                  Precipitation.change +
                  Temp.Change:adapt_dummy +
                  adapt_dummy,
                data = split_tibble)
  
  ggpredict(damages, 
            terms = c("Temp.Change", "Baseline_tmp_weighted [15:25 by = 5]", "crop_pooled"), 
            condition = c(adapt_dummy = 0, Precipitation.change = 0, f_CO2 = 0),
            ci.lvl = NA)  
  
  
}

coefs_bootstrap_regression_stratified_predict <- lapply(1:750, bootstrap_regression_stratified_predict)

coefs_bootstrap_regression_stratified_predict_df <- do.call("rbind", coefs_bootstrap_regression_stratified_predict) %>% as.data.frame()

dim(coefs_bootstrap_regression_stratified_predict_df) # 1600200       4

# estimate summary confidence intervals for bootstrapped coefficients
# is it not ambiguous which predicted value to take the 0.025/0.975 over?
bootstrap_predicted_values <- coefs_bootstrap_regression_stratified_predict_df %>% 
  group_by(x, group, facet) %>% 
  summarise(pct_025 = quantile(predicted, 0.025),
            pct_500 = quantile(predicted, 0.500),
            pct_975 = quantile(predicted, 0.975))

bootstrap_predicted_values <- bootstrap_predicted_values %>% 
  filter(x >= 0 & x <= 5)

# skip this
ggplot() + 
  geom_line(data = predicted_crops_quantile_copy, 
            aes(x, predicted, colour = facet, alpha = group)) +
  geom_line(data = bootstrap_predicted_values, linetype = "dotted", 
            aes(x, pct_025, colour = facet, alpha = group)) +
  geom_line(data = bootstrap_predicted_values, linetype = "dotted", 
            aes(x, pct_975, colour = facet, alpha = group)) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-50, 50)) +
  facet_wrap(~factor(facet, levels = c("Maize", "Rice", "Wheat", "Soybean"))) +
  scale_alpha_discrete(range = c(0.2, 1)) + 
  scale_color_manual(values = c("Maize" = "#489A07",
                                "Rice" = "#2370BB",
                                "Wheat" = "#F5BA37",
                                "Soybean" = "red")) +
  theme_test() +
  geom_hline(yintercept = 0) +
  xlab("Temperature Change (degrees Celsius)") +
  ylab("Yield Change (%)") +
  theme(legend.position = "none")


# # individual plots - with central median bootstrap estimates

maize_response_fun_strat <- ggplot() + 
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values[bootstrap_predicted_values$facet == "Maize",], 
            col = "#489A07", size = 1,
            aes(x, pct_500, alpha = group)) + # adjusted damage function
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values[bootstrap_predicted_values$facet == "Maize",], 
            col = "#489A07",linetype = "dotted", size = 0.8,
            aes(x, pct_025, alpha = group)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values[bootstrap_predicted_values$facet == "Maize",], 
            col = "#489A07", linetype = "dotted", size = 0.8,
            aes(x, pct_975, alpha = group)) +
  scale_alpha_discrete(range = c(0.4, 1)) + 
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-75, 15)) +
  theme_test() +
  geom_hline(yintercept = 0) +
  xlab("Temperature change (degrees C)") +
  ylab("Yield change (%)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Maize")

rice_response_fun_strat <- ggplot() + 
  geom_line(stat = "smooth",
            data = bootstrap_predicted_values[bootstrap_predicted_values$facet == "Rice",], 
            col = "#2370BB", size = 1,
            aes(x, pct_500, alpha = group)) + # adjusted damage function
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values[bootstrap_predicted_values$facet == "Rice",], 
            col = "#2370BB",linetype = "dotted", size = 0.8,
            aes(x, pct_025, alpha = group)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values[bootstrap_predicted_values$facet == "Rice",], 
            col = "#2370BB", linetype = "dotted", size = 0.8,
            aes(x, pct_975, alpha = group)) +
  scale_alpha_discrete(range = c(0.4, 1)) + 
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-75, 15)) +
  theme_test() +
  geom_hline(yintercept = 0) +
  xlab("Temperature change (degrees C)") +
  ylab("Yield change (%)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Rice")

wheat_response_fun_strat <- ggplot() + 
  geom_line(stat = "smooth",
            data = bootstrap_predicted_values[bootstrap_predicted_values$facet == "Wheat",], 
            col = "#F8BC2C", size = 1,
            aes(x, pct_500, alpha = group)) + # adjusted damage function
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values[bootstrap_predicted_values$facet == "Wheat",], 
            col = "#F8BC2C",linetype = "dotted", size = 0.8,
            aes(x, pct_025, alpha = group)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values[bootstrap_predicted_values$facet == "Wheat",], 
            col = "#F8BC2C", linetype = "dotted", size = 0.8,
            aes(x, pct_975, alpha = group)) +
  scale_alpha_discrete(range = c(0.4, 1)) + 
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-75, 15)) +
  theme_test() +
  geom_hline(yintercept = 0) +
  xlab("Temperature change (degrees C)") +
  ylab("Yield change (%)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Wheat")

soy_response_fun_strat <- ggplot() + 
  geom_line(stat = "smooth",
            data = bootstrap_predicted_values[bootstrap_predicted_values$facet == "Soybean",], 
            col = "red", size = 1,
            aes(x, pct_500, alpha = group)) + # adjusted damage function
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values[bootstrap_predicted_values$facet == "Soybean",], 
            col = "red",linetype = "dotted", size = 0.8,
            aes(x, pct_025, alpha = group)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values[bootstrap_predicted_values$facet == "Soybean",], 
            col = "red", linetype = "dotted", size = 0.8,
            aes(x, pct_975, alpha = group)) +
  scale_alpha_discrete(range = c(0.4, 1)) + 
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-300, 50)) +
  theme_test() +
  geom_hline(yintercept = 0) +
  xlab("Temperature change (degrees C)") +
  ylab("Yield change (%)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Soy")

grid.arrange(maize_response_fun_strat,
             rice_response_fun_strat,
             wheat_response_fun_strat,
             soy_response_fun_strat,
             ncol = 2, 
             nrow = 2)

# block/cluster bootstrapping (clusters are sampled) ------------------

# greater variation in these bootstraps, which is why 0.025 and 0.975 for each prediction at x is not continuous
# in general, likely not sampling from the same bootstrap as we move down x
# D <- AGIMPACTS_TRUNCATED_DATA %>% nest(-Reference)

E <- AGIMPACTS_FINAL_INCOMPLETE %>% 
  group_by(Reference) %>% 
  nest()

library(rsample)
bs <- bootstraps(E, times = 750) 

bootstrap_regression_predict <- function(i){
  
  split_tibble <- as_tibble(bs$splits[[i]]) %>% unnest(cols = c(data))  # different data set length each bootstrap
  
  damages <- lm(Yield.Change ~ 0 + Temp.Change:crop_pooled + I(Temp.Change^2):crop_pooled +
                  Temp.Change:crop_pooled:Baseline_tmp_weighted + 
                  I(Temp.Change^2):crop_pooled:Baseline_tmp_weighted +
                  # crop_pooled:Baseline_tmp_weighted + ### NOTE THIS CHANGE ###
                  f_CO2:C3 + f_CO2:C4  + 
                  Precipitation.change +
                  Temp.Change:adapt_dummy +
                  adapt_dummy,
                data = split_tibble)
  
  predicted_yield <- ggpredict(damages, 
                               terms = c("Temp.Change", "Baseline_tmp_weighted [15:25 by = 5]", "crop_pooled"), 
                               condition = c(adapt_dummy = 0, Precipitation.change = 0, f_CO2 = 0, C3 = 0, C4 = 0), # THIS
                               ci.lvl = NA)  
  
  predicted_yield$predicted <- predicted_yield$predicted - coef(damages)[2] # adjust damage functions to go through the origin
  
  predicted_yield
  # six warnings: prediction from a rank-deficient fit may be misleading
}

# 
#coefs_bootstrap_regression_predict <- lapply(1:2000, bootstrap_regression_predict)
coefs_bootstrap_regression_predict_adj <- lapply(1:750, bootstrap_regression_predict)

#coefs_bootstrap_regression_predict_df <- do.call("rbind", coefs_bootstrap_regression_predict) %>% as.data.frame()
coefs_bootstrap_regression_predict_adj_df <- do.call("rbind", coefs_bootstrap_regression_predict_adj) %>% as.data.frame()

# estimate summary confidence intervals for bootstrapped coefficients
# is it not ambiguous which predicted value to take the 0.025/0.975 over?
dim(coefs_bootstrap_regression_predict_df) # 2348244 - now 3099468
# if we had more predicted values, do we have smoother 2.5th and 97.5th values selected from a larger set of bootstrapped predictions?
coefs_bootstrap_regression_predict_df %>% 
  group_by(x, facet, group) %>% 
  summarise(n=n()) # groups of 951 values # even less than 1500 bs end up being generated? # 1278 with 2000 bs

coefs_bootstrap_regression_cluster_predict_df %>% 
  group_by(x, facet, group) %>% 
  summarise(n=n()) # groups of 1494 values # this must mean that more of the 1500 bootstrap samples get generated

# the results are coming from the fact that a different bootstrap is getting selected each time for the 0.025 and 0.975

bootstrap_predicted_values_a <- coefs_bootstrap_regression_predict_adj_df %>% 
  group_by(facet, group, x) %>% 
  summarise(pct_025 = quantile(predicted, 0.025),
            pct_975 = quantile(predicted, 0.975),
            pct_500 = quantile(predicted, 0.5))

bootstrap_predicted_values_a <- bootstrap_predicted_values_a %>% 
  filter(x >= 0 & x <= 5)

# do this differently, filter first and then group

bootstrap_predicted_values_b <- coefs_bootstrap_regression_predict_adj_df %>% 
  filter(x >= 0 & x <= 5)

bootstrap_predicted_values_b <- bootstrap_predicted_values_b %>% 
  group_by(x, facet, group) %>% 
  summarise(pct_025 = quantile(predicted, 0.025),
            pct_975 = quantile(predicted, 0.975),
            pct_500 = quantile(predicted, 0.5))

# bootstrap_predicted_values_a %>% filter(facet == "Rice" & group == 15) %>% print(n=Inf)
ggplot() + 
  geom_line(stat = "smooth", data = bootstrap_predicted_values_a, linetype = "solid", 
            aes(x, pct_500, colour = facet, alpha = group)) + # adjusted damage function
  geom_line(stat = "smooth", data = bootstrap_predicted_values_a, linetype = "dotted", 
            aes(x, pct_025, colour = facet, alpha = group)) +
  geom_line(stat = "smooth", data = bootstrap_predicted_values_a, linetype = "dotted", 
            aes(x, pct_975, colour = facet, alpha = group)) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-50, 50)) +
  facet_wrap(~factor(facet, levels = c("Maize", "Rice", "Wheat", "Soybean"))) +
  scale_alpha_discrete(range = c(0.4, 1)) + 
  scale_color_manual(values = c("Maize" = "#489A07",
                                "Rice" = "#2370BB",
                                "Wheat" = "#F5BA37",
                                "Soybean" = "red")) +
  theme_test() +
  geom_hline(yintercept = 0) +
  xlab("Temperature Change (degrees Celsius)") +
  ylab("Yield Change (%)") +
  theme(legend.position = "none")

# with new format - 4 x 3 and with geom_ribbon
# group columns, facet rows

# save.image("agimpacts-250122.RData")

ggplot() + 
  geom_line(stat = "smooth", method="loess",data = bootstrap_predicted_values_b, linetype = "solid", 
            aes(x, pct_500), colour = "black") + 
  geom_line(stat = "smooth", method="loess", data = bootstrap_predicted_values_b, linetype = "solid", 
            aes(x, pct_025, colour = facet)) +
  geom_line(stat = "smooth", method="loess", data = bootstrap_predicted_values_b, linetype = "solid", 
            aes(x, pct_975, colour = facet)) +
  #geom_ribbon( data = bootstrap_predicted_values_a, 
  #            aes(x, ymin = pct_025, ymax = pct_975), fill="grey", alpha=0.4) +
  #stat_smooth(
  #  geom = "ribbon", 
  #  data = bootstrap_predicted_values_a, 
  #  aes(x, pct_500, ymin = pct_025, ymax = pct_975), 
  #           fill = "grey", alpha=0.4) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-50, 50)) +
  facet_grid(rows = vars(facet), cols = vars(group)) +
  scale_color_manual(values = c("Maize" = "#489A07",
                                "Rice" = "#2370BB",
                                "Wheat" = "#F5BA37",
                                "Soybean" = "red")) +
  theme_test() +
  geom_hline(yintercept = 0) +
  xlab("Temperature Change (degrees Celsius)") +
  ylab("Yield Change (%)") +
  theme(legend.position = "none")

# VALUES JUMP AROUND - NOT SMOOTH - still jump around even with bs = 2000

ggplot() + 
  geom_line(data = predicted_crops_quantile_copy, 
            aes(x, predicted - coef(damages)[2], colour = facet, alpha = group)) + # adjusted damage function
  geom_line(stat = "smooth", data = bootstrap_predicted_values_a, linetype = "dotted", 
            aes(x, pct_025, colour = facet, alpha = group)) +
  geom_line(stat = "smooth", data = bootstrap_predicted_values_a, linetype = "dotted", 
            aes(x, pct_975, colour = facet, alpha = group)) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-50, 50)) +
  facet_wrap(~factor(facet, levels = c("Maize", "Rice", "Wheat", "Soybean"))) +
  scale_alpha_discrete(range = c(0.4, 1)) + 
  scale_color_manual(values = c("Maize" = "#489A07",
                                "Rice" = "#2370BB",
                                "Wheat" = "#F5BA37",
                                "Soybean" = "red")) +
  theme_test() +
  geom_hline(yintercept = 0) +
  xlab("Temperature Change (degrees Celsius)") +
  ylab("Yield Change (%)") +
  theme(legend.position = "none")

# individual plots- just for maize, with single model estimates

maize_response_fun <- ggplot() + 
  geom_line(data = predicted_crops_quantile_copy[predicted_crops_quantile$facet == "Maize",], 
            col = "#489A07", size = 1,
            aes(x, predicted - coef(damages)[2], alpha = group)) + # adjusted damage function
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_a[bootstrap_predicted_values_a$facet == "Maize",], 
            col = "#489A07",linetype = "dotted", size = 0.8,
            aes(x, pct_025, alpha = group)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_a[bootstrap_predicted_values_a$facet == "Maize",], 
            col = "#489A07", linetype = "dotted", size = 0.8,
            aes(x, pct_975, alpha = group)) +
  scale_alpha_discrete(range = c(0.4, 1)) + 
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-75, 15)) +
  theme_test() +
  geom_hline(yintercept = 0) +
  xlab("Temperature change (degrees C)") +
  ylab("Yield change (%)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Maize")

# individual plots - with central median bootstrap estimates

maize_response_fun <- ggplot() + 
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_a[bootstrap_predicted_values_a$facet == "Maize",], 
            col = "#489A07", size = 1,
            aes(x, pct_500, alpha = group)) + # adjusted damage function
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_a[bootstrap_predicted_values_a$facet == "Maize",], 
            col = "#489A07",linetype = "dotted", size = 0.8,
            aes(x, pct_025, alpha = group)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_a[bootstrap_predicted_values_a$facet == "Maize",], 
            col = "#489A07", linetype = "dotted", size = 0.8,
            aes(x, pct_975, alpha = group)) +
  scale_alpha_discrete(range = c(0.4, 1)) + 
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-75, 15)) +
  theme_test() +
  geom_hline(yintercept = 0) +
  xlab("Temperature change (degrees C)") +
  ylab("Yield change (%)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Maize")

rice_response_fun <- ggplot() + 
  geom_line(stat = "smooth",
            data = bootstrap_predicted_values_a[bootstrap_predicted_values_a$facet == "Rice",], 
            col = "#2370BB", size = 1,
            aes(x, pct_500, alpha = group)) + # adjusted damage function
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_a[bootstrap_predicted_values_a$facet == "Rice",], 
            col = "#2370BB",linetype = "dotted", size = 0.8,
            aes(x, pct_025, alpha = group)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_a[bootstrap_predicted_values_a$facet == "Rice",], 
            col = "#2370BB", linetype = "dotted", size = 0.8,
            aes(x, pct_975, alpha = group)) +
  scale_alpha_discrete(range = c(0.4, 1)) + 
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-75, 15)) +
  theme_test() +
  geom_hline(yintercept = 0) +
  xlab("Temperature change (degrees C)") +
  ylab("Yield change (%)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Rice")

wheat_response_fun <- ggplot() + 
  geom_line(stat = "smooth",
            data = bootstrap_predicted_values_a[bootstrap_predicted_values_a$facet == "Wheat",], 
            col = "#F8BC2C", size = 1,
            aes(x, pct_500, alpha = group)) + # adjusted damage function
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_a[bootstrap_predicted_values_a$facet == "Wheat",], 
            col = "#F8BC2C",linetype = "dotted", size = 0.8,
            aes(x, pct_025, alpha = group)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_a[bootstrap_predicted_values_a$facet == "Wheat",], 
            col = "#F8BC2C", linetype = "dotted", size = 0.8,
            aes(x, pct_975, alpha = group)) +
  scale_alpha_discrete(range = c(0.4, 1)) + 
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-75, 15)) +
  theme_test() +
  geom_hline(yintercept = 0) +
  xlab("Temperature change (degrees C)") +
  ylab("Yield change (%)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Wheat")

soy_response_fun <- ggplot() + 
  geom_line(stat = "smooth",
            data = bootstrap_predicted_values_a[bootstrap_predicted_values_a$facet == "Soybean",], 
            col = "red", size = 1,
            aes(x, pct_500, alpha = group)) + # adjusted damage function
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_a[bootstrap_predicted_values_a$facet == "Soybean",], 
            col = "red",linetype = "dotted", size = 0.8,
            aes(x, pct_025, alpha = group)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_a[bootstrap_predicted_values_a$facet == "Soybean",], 
            col = "red", linetype = "dotted", size = 0.8,
            aes(x, pct_975, alpha = group)) +
  scale_alpha_discrete(range = c(0.4, 1)) + 
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-75, 15)) + # (-300, 50)
  theme_test() +
  geom_hline(yintercept = 0) +
  xlab("Temperature change (degrees C)") +
  ylab("Yield change (%)") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Soy")

grid.arrange(maize_response_fun,
             rice_response_fun,
             wheat_response_fun,
             soy_response_fun,
             ncol = 2, 
             nrow = 2)


# block bootstraps at median baseline temperature ----------------------------------------------------
# for reference function at median baseline temperature

F <- AGIMPACTS_TRUNCATED_ADAPT %>% 
  group_by(Reference) %>% 
  nest()

library(rsample)
bs <- bootstraps(F, times = 750) 

bootstrap_regression_predict_main_median <- function(i){
  
  split_tibble <- as_tibble(bs$splits[[i]]) %>% unnest(cols = c(data))  # different data set length each bootstrap
  
  damages <- lm(Yield.Change ~ 0 + Temp.Change:crop_pooled + I(Temp.Change^2):crop_pooled +
                  Temp.Change:crop_pooled:Baseline_tmp_weighted + 
                  I(Temp.Change^2):crop_pooled:Baseline_tmp_weighted +
                  # crop_pooled:Baseline_tmp_weighted + ### NOTE THIS CHANGE ###
                  f_CO2:C3 + f_CO2:C4  + 
                  Precipitation.change +
                  Temp.Change:adapt_dummy +
                  adapt_dummy,
                data = split_tibble)
  
  predicted_yield <- ggpredict(damages, 
                               terms = c("Temp.Change", "Baseline_tmp_weighted [median]", "crop_pooled"), # HERE
                               condition = c(adapt_dummy = 0, Precipitation.change = 0, f_CO2 = 0, C3 = 0, C4 = 0), # THIS
                               ci.lvl = NA)  
  
  predicted_yield$predicted <- predicted_yield$predicted - coef(damages)[2] # adjust damage functions to go through the origin
  
  predicted_yield
  # six warnings: prediction from a rank-deficient fit may be misleading
}

# 
#coefs_bootstrap_regression_predict <- lapply(1:2000, bootstrap_regression_predict)
coefs_bootstrap_regression_predict_adj_main_median <- lapply(1:750, bootstrap_regression_predict_main_median)

#coefs_bootstrap_regression_predict_df <- do.call("rbind", coefs_bootstrap_regression_predict) %>% as.data.frame()
coefs_bootstrap_regression_predict_adj_df_main_median <- do.call("rbind", coefs_bootstrap_regression_predict_adj_main_median) %>% as.data.frame()


bootstrap_predicted_values_main_median <- coefs_bootstrap_regression_predict_adj_df_main_median %>% 
  group_by(facet, group, x) %>% 
  summarise(pct_025 = quantile(predicted, 0.025),
            pct_975 = quantile(predicted, 0.975),
            pct_500 = quantile(predicted, 0.5))

bootstrap_predicted_values_main_median <- bootstrap_predicted_values_main_median %>% 
  filter(x >= 0 & x <= 5)

# cubic term --------------------------------------------------------------

all_crops_model_no_fe_cubic <- function(DATA){
  
  
  damages_cubic <<- lm(Yield.Change ~ 0 + Temp.Change:crop_pooled + I(Temp.Change^2):crop_pooled + I(Temp.Change^3):crop_pooled +
                         Temp.Change:crop_pooled:Baseline_tmp_weighted + 
                         I(Temp.Change^2):crop_pooled:Baseline_tmp_weighted +
                         f_CO2:C3 + f_CO2:C4  + 
                         Precipitation.change +
                         Temp.Change:adapt_dummy +
                         adapt_dummy,
                       data = DATA)
  
  
  # ggeffect() wheat
  
  # effects_crops <<- ggeffect(damages_cubic, 
  #                           terms = c("Temp.Change", "Baseline_tmp_weighted [quart]", "crop_pooled"), 
  #                           condition = c(Precipitation.change = 0, adapt_dummy = 0, f_CO2 = 0))
  
  predicted_crops_cubic <<- ggpredict(damages_cubic, 
                                      terms = c("Temp.Change", "Baseline_tmp_weighted [median]", "crop_pooled"), 
                                      condition = c(Precipitation.change = 0, adapt_dummy = 0, f_CO2 = 0))
  
}


all_crops_model_no_fe_cubic(AGIMPACTS_TRUNCATED_ADAPT)

plot(predicted_crops_cubic)


bootstrap_regression_predict_cubic <- function(i){
  
  split_tibble <- as_tibble(bs$splits[[i]]) %>% unnest(cols = c(data))  # different data set length each bootstrap
  
  damages_cubic <- lm(Yield.Change ~ 0 + Temp.Change:crop_pooled + I(Temp.Change^2):crop_pooled + I(Temp.Change^3):crop_pooled +
                        Temp.Change:crop_pooled:Baseline_tmp_weighted + 
                        I(Temp.Change^2):crop_pooled:Baseline_tmp_weighted +
                        f_CO2:C3 + f_CO2:C4  + 
                        Precipitation.change +
                        Temp.Change:adapt_dummy +
                        adapt_dummy,
                      data = split_tibble)
  
  predicted_yield <- ggpredict(damages_cubic, 
                               terms = c("Temp.Change", "Baseline_tmp_weighted [median]", "crop_pooled"), 
                               condition = c(adapt_dummy = 0, Precipitation.change = 0, f_CO2 = 0, C3 = 0, C4 = 0), # THIS
                               ci.lvl = NA)  
  
  predicted_yield$predicted <- predicted_yield$predicted - coef(damages_cubic)[2] # adjust damage functions to go through the origin
  
  predicted_yield
  # six warnings: prediction from a rank-deficient fit may be misleading
}

# 
#coefs_bootstrap_regression_predict <- lapply(1:2000, bootstrap_regression_predict)
coefs_bootstrap_regression_predict_adj_cubic <- lapply(1:750, bootstrap_regression_predict_cubic)

#coefs_bootstrap_regression_predict_df <- do.call("rbind", coefs_bootstrap_regression_predict) %>% as.data.frame()
coefs_bootstrap_regression_predict_adj_df_cubic <- do.call("rbind", coefs_bootstrap_regression_predict_adj_cubic) %>% as.data.frame()

# estimate summary confidence intervals for bootstrapped coefficients
# is it not ambiguous which predicted value to take the 0.025/0.975 over?
dim(coefs_bootstrap_regression_predict_adj_df_cubic) # 389224
# if we had more predicted values, do we have smoother 2.5th and 97.5th values selected from a larger set of bootstrapped predictions?

# the results are coming from the fact that a different bootstrap is getting selected each time for the 0.025 and 0.975

bootstrap_predicted_values_cubic <- coefs_bootstrap_regression_predict_adj_df_cubic %>% 
  group_by(facet, group, x) %>% # take out the 'group'
  summarise(pct_025 = quantile(predicted, 0.025),
            pct_975 = quantile(predicted, 0.975),
            pct_500 = quantile(predicted, 0.5))

bootstrap_predicted_values_cubic <- bootstrap_predicted_values_cubic %>% 
  filter(x >= 0 & x <= 5)

# individual plots - with central median bootstrap estimates
# cubic plots -------------------------------------------------------------

ggplot() + 
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_cubic, 
            linetype = "dashed", size = 1,
            aes(x, pct_500, col = facet)) + # adjusted damage function
  #geom_line(stat = "smooth", 
  #          data = bootstrap_predicted_values_cubic, 
  #          linetype = "dotted", size = 0.8,
  #          aes(x, pct_025, col = facet)) +
  #geom_line(stat = "smooth", 
  #          data = bootstrap_predicted_values_cubic, 
  #          linetype = "dotted", size = 0.8,
  #          aes(x, pct_975, col = facet)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_main_median, 
            size = 0.8,  # col = "grey"
            aes(x, pct_500, col = facet)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_main_median, 
            linetype = "dotted", size = 0.8,  # col = "grey"
            aes(x, pct_025, col = facet)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_main_median, 
            linetype = "dotted", size = 0.8,  # col = "grey"
            aes(x, pct_975, col = facet)) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-50, 50)) +
  facet_wrap(~factor(facet, levels = c("Maize", "Rice", "Wheat", "Soybean"))) +
  scale_color_manual(values = c("Maize" = "#489A07",
                                "Rice" = "#2370BB",
                                "Wheat" = "#F5BA37",
                                "Soybean" = "red")) +
  theme_test() +
  geom_hline(yintercept = 0) +
  xlab("Temperature Change (degrees Celsius)") +
  ylab("Yield Change (%)") +
  theme(legend.position = "none") 


# individual adaptation terms ---------------------------------------------

bootstrap_regression_predict_adapt <- function(i){
  
  split_tibble <- as_tibble(bs$splits[[i]]) %>% unnest(cols = c(data))  # different data set length each bootstrap
  
  damages <- lm(Yield.Change ~ 0 + Temp.Change:crop_pooled + I(Temp.Change^2):crop_pooled +
                  Temp.Change:crop_pooled:Baseline_tmp_weighted + 
                  I(Temp.Change^2):crop_pooled:Baseline_tmp_weighted +
                  # crop_pooled:Baseline_tmp_weighted + ### NOTE THIS CHANGE ###
                  f_CO2:C3 + f_CO2:C4  + 
                  Precipitation.change +
                  Temp.Change:Adaptation +
                  Adaptation,
                data = split_tibble)
  
  predicted_yield <- ggpredict(damages, 
                               terms = c("Temp.Change", "Baseline_tmp_weighted [20]", "crop_pooled"), # HERE
                               condition = c(Adaptation = "No", Precipitation.change = 0, f_CO2 = 0, C3 = 0, C4 = 0), # THIS
                               ci.lvl = NA)  
  
  # predicted_yield$predicted <- predicted_yield$predicted - coef(damages)[7] # adjust damage functions to go through the origin
  
  predicted_yield
  # six warnings: prediction from a rank-deficient fit may be misleading
}

# 
#coefs_bootstrap_regression_predict <- lapply(1:2000, bootstrap_regression_predict)
coefs_bootstrap_regression_predict_adj_adapt <- lapply(1:750, bootstrap_regression_predict_adapt)

#coefs_bootstrap_regression_predict_df <- do.call("rbind", coefs_bootstrap_regression_predict) %>% as.data.frame()
coefs_bootstrap_regression_predict_adj_df_adapt <- do.call("rbind", coefs_bootstrap_regression_predict_adj_adapt) %>% as.data.frame()


bootstrap_predicted_values_adapt <- coefs_bootstrap_regression_predict_adj_df_adapt %>% 
  group_by(facet, group, x) %>% # the same whether with or without group, when value is set to 20 degrees
  summarise(pct_025 = quantile(predicted, 0.025),
            pct_975 = quantile(predicted, 0.975),
            pct_500 = quantile(predicted, 0.5))

bootstrap_predicted_values_adapt <- bootstrap_predicted_values_adapt %>% 
  filter(x >= 0 & x <= 5)

ggplot() + 
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_adapt, 
            linetype = "dashed", size = 1,
            aes(x, pct_500, col = facet)) + # adjusted damage function
  # geom_line(stat = "smooth", 
  #           data = bootstrap_predicted_values_adapt, 
  #           linetype = "dotted", size = 0.8,
  #           aes(x, pct_025, col = facet)) +
  # geom_line(stat = "smooth", 
  #          data = bootstrap_predicted_values_adapt, 
  #          linetype = "dotted", size = 0.8,
  #          aes(x, pct_975, col = facet)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_main_median, 
            size = 0.8,  # normal
            aes(x, pct_500, col = facet)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_main_median, 
            linetype = "dotted", size = 0.8, # normal
            aes(x, pct_025, col = facet)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_main_median, 
            linetype = "dotted", size = 0.8, # normal
            aes(x, pct_975, col = facet)) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-50, 50)) +
  facet_wrap(~factor(facet, levels = c("Maize", "Rice", "Wheat", "Soybean"))) +
  scale_color_manual(values = c("Maize" = "#489A07",
                                "Rice" = "#2370BB",
                                "Wheat" = "#F5BA37",
                                "Soybean" = "red")) +
  theme_test() +
  geom_hline(yintercept = 0) +
  xlab("Temperature Change (degrees Celsius)") +
  ylab("Yield Change (%)") +
  theme(legend.position = "none") 


# segmenting post-2005 ----------------------------------------------------

G <- AGIMPACTS_TRUNCATED_2005 %>% 
  group_by(Reference) %>% 
  nest()

library(rsample)
bs_2005 <- bootstraps(G, times = 750) 


bootstrap_regression_predict_2005 <- function(i){
  
  split_tibble <- as_tibble(bs_2005$splits[[i]]) %>% unnest(cols = c(data))  # different data set length each bootstrap
  
  damages <- lm(Yield.Change ~ 0 + Temp.Change:crop_pooled + I(Temp.Change^2):crop_pooled +
                  Temp.Change:crop_pooled:Baseline_tmp_weighted + 
                  I(Temp.Change^2):crop_pooled:Baseline_tmp_weighted +
                  # crop_pooled:Baseline_tmp_weighted + ### NOTE THIS CHANGE ###
                  f_CO2:C3 + f_CO2:C4  + 
                  Precipitation.change +
                  Temp.Change:adapt_dummy +
                  adapt_dummy,
                data = split_tibble)
  
  predicted_yield <- ggpredict(damages, 
                               terms = c("Temp.Change", "Baseline_tmp_weighted [median]", "crop_pooled"), # HERE
                               condition = c(adapt_dummy = 0, Precipitation.change = 0, f_CO2 = 0, C3 = 0, C4 = 0), # THIS
                               ci.lvl = NA)  
  
  predicted_yield$predicted <- predicted_yield$predicted - coef(damages)[2] # adjust damage functions to go through the origin
  
  predicted_yield
  # six warnings: prediction from a rank-deficient fit may be misleading
}

# 
#coefs_bootstrap_regression_predict <- lapply(1:2000, bootstrap_regression_predict)
coefs_bootstrap_regression_predict_adj_2005 <- lapply(1:750, bootstrap_regression_predict_2005)

#coefs_bootstrap_regression_predict_df <- do.call("rbind", coefs_bootstrap_regression_predict) %>% as.data.frame()
coefs_bootstrap_regression_predict_adj_df_2005 <- do.call("rbind", coefs_bootstrap_regression_predict_adj_2005) %>% as.data.frame()


bootstrap_predicted_values_2005 <- coefs_bootstrap_regression_predict_adj_df_2005 %>% 
  group_by(facet, group, x) %>% 
  summarise(pct_025 = quantile(predicted, 0.025),
            pct_975 = quantile(predicted, 0.975),
            pct_500 = quantile(predicted, 0.5))

bootstrap_predicted_values_2005 <- bootstrap_predicted_values_2005 %>% 
  filter(x >= 0 & x <= 5)

ggplot() + 
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_2005, 
            linetype = "dashed", size = 1,
            aes(x, pct_500, col = facet)) + # adjusted damage function
  # geom_line(stat = "smooth", 
  #           data = bootstrap_predicted_values_2005, 
  #           linetype = "dotted", size = 0.8,
  #           aes(x, pct_025, col = facet)) +
  #  geom_line(stat = "smooth", 
  #          data = bootstrap_predicted_values_2005, 
  #           linetype = "dotted", size = 0.8,
  #          aes(x, pct_975, col = facet)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_main_median, 
            size = 0.8, # normal
            aes(x, pct_500, col = facet)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_main_median, 
            linetype = "dotted", size = 0.8, # normal
            aes(x, pct_025, col = facet)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_main_median, 
            linetype = "dotted", size = 0.8, # normal
            aes(x, pct_975, col = facet)) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-50, 50)) +
  facet_wrap(~factor(facet, levels = c("Maize", "Rice", "Wheat", "Soybean"))) +
  scale_color_manual(values = c("Maize" = "#489A07",
                                "Rice" = "#2370BB",
                                "Wheat" = "#F5BA37",
                                "Soybean" = "red")) +
  theme_test() +
  geom_hline(yintercept = 0) +
  xlab("Temperature Change (degrees Celsius)") +
  ylab("Yield Change (%)") +
  theme(legend.position = "none") 



# segmenting excluding korea ----------------------------------------------


H <- AGIMPACTS_TRUNCATED_NO_KOREA %>% 
  group_by(Reference) %>% 
  nest()

library(rsample)
bs_NO_KR <- bootstraps(H, times = 750) 


bootstrap_regression_predict_NO_KR <- function(i){
  
  split_tibble <- as_tibble(bs_NO_KR$splits[[i]]) %>% unnest(cols = c(data))  # different data set length each bootstrap
  
  damages <- lm(Yield.Change ~ 0 + Temp.Change:crop_pooled + I(Temp.Change^2):crop_pooled +
                  Temp.Change:crop_pooled:Baseline_tmp_weighted + 
                  I(Temp.Change^2):crop_pooled:Baseline_tmp_weighted +
                  # crop_pooled:Baseline_tmp_weighted + ### NOTE THIS CHANGE ###
                  f_CO2:C3 + f_CO2:C4  + 
                  Precipitation.change +
                  Temp.Change:adapt_dummy +
                  adapt_dummy,
                data = split_tibble)
  
  predicted_yield <- ggpredict(damages, 
                               terms = c("Temp.Change", "Baseline_tmp_weighted [median]", "crop_pooled"), # HERE
                               condition = c(adapt_dummy = 0, Precipitation.change = 0, f_CO2 = 0, C3 = 0, C4 = 0), # THIS
                               ci.lvl = NA)  
  
  predicted_yield$predicted <- predicted_yield$predicted - coef(damages)[2] # adjust damage functions to go through the origin
  
  predicted_yield
  # six warnings: prediction from a rank-deficient fit may be misleading
}

# 
#coefs_bootstrap_regression_predict <- lapply(1:2000, bootstrap_regression_predict)
coefs_bootstrap_regression_predict_adj_NO_KR <- lapply(1:750, bootstrap_regression_predict_NO_KR)

#coefs_bootstrap_regression_predict_df <- do.call("rbind", coefs_bootstrap_regression_predict) %>% as.data.frame()
coefs_bootstrap_regression_predict_adj_df_NO_KR <- do.call("rbind", coefs_bootstrap_regression_predict_adj_NO_KR) %>% as.data.frame()


bootstrap_predicted_values_NO_KR <- coefs_bootstrap_regression_predict_adj_df_NO_KR %>% 
  group_by(facet, group, x) %>% # remember that each bootstrap has a different median baseline temp, so cannot really group by 'group' i.e. temp?
  summarise(pct_025 = quantile(predicted, 0.025),
            pct_975 = quantile(predicted, 0.975),
            pct_500 = quantile(predicted, 0.5))

bootstrap_predicted_values_NO_KR <- bootstrap_predicted_values_NO_KR %>% 
  filter(x >= 0 & x <= 5)

ggplot() + 
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_NO_KR, 
            linetype = "dashed", size = 1,
            aes(x, pct_500, col = facet)) + # adjusted damage function
  #geom_line(stat = "smooth", 
  #          data = bootstrap_predicted_values_NO_KR, 
  #          linetype = "dotted", size = 0.8,
  #          aes(x, pct_025, col = facet)) +
  #geom_line(stat = "smooth", 
  #          data = bootstrap_predicted_values_NO_KR, 
  #         linetype = "dotted", size = 0.8,
  #         aes(x, pct_975, col = facet)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_main_median, 
            size = 0.8, # normal
            aes(x, pct_500, col = facet)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_main_median, 
            linetype = "dotted", size = 0.8, # normal
            aes(x, pct_025, col = facet)) +
  geom_line(stat = "smooth", 
            data = bootstrap_predicted_values_main_median, 
            linetype = "dotted", size = 0.8,  # normal
            aes(x, pct_975, col = facet)) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-50, 50)) +
  facet_wrap(~factor(facet, levels = c("Maize", "Rice", "Wheat", "Soybean"))) +
  scale_color_manual(values = c("Maize" = "#489A07",
                                "Rice" = "#2370BB",
                                "Wheat" = "#F5BA37",
                                "Soybean" = "red")) +
  theme_test() +
  geom_hline(yintercept = 0) +
  xlab("Temperature Change (degrees Celsius)") +
  ylab("Yield Change (%)") +
  theme(legend.position = "none") 

