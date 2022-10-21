

# Read in data ------------------------------------------------------------

AGIMPACTS_MAIN_COMPLETED_WEIGHTED <- readr::read_csv(here("processed", "agimpacts_main_completed_weighted_v2.csv"))

# Data cleaning -----------------------------------------------------------


# mutate new column in AGIMPACTS_MAIN_COMPLETED_WEIGHTED for the four crop values

AGIMPACTS_WEIGHTED_POOLED <- AGIMPACTS_MAIN_COMPLETED_WEIGHTED %>% 
  mutate(crop_pooled = case_when(Crop %in% c("Wheat", "Wheat (Spring)", "Wheat (Durum)", "Wheat (Winter)", "Wheat (Rainfed)", "Wheat (Irrigated)") ~ "Wheat",
                                 Crop %in% c("Rice", "Rice (Irrigated)", "Rice (Rainfed)") ~ "Rice",
                                 Crop == "Maize" ~ "Maize",
                                 Crop == "Soybean" ~ "Soybean"))            

# split out adaptation by type rather than dummy, but first recode 
AGIMPACTS_WEIGHTED_POOLED$Adaptation[AGIMPACTS_WEIGHTED_POOLED$Adaptation %in% c("No", NA)] <- "No"

AGIMPACTS_WEIGHTED_POOLED <- AGIMPACTS_WEIGHTED_POOLED %>% 
  mutate(adapt_dummy = as.factor(if_else(Adaptation %in% c("No", "NA"), 0, 1)))

# remove data point that is missing baseline_tmp_weighted

sum(is.na(AGIMPACTS_WEIGHTED_POOLED$Baseline_tmp_weighted))

AGIMPACTS_WEIGHTED_POOLED <- AGIMPACTS_WEIGHTED_POOLED %>% 
  filter(!is.na(Baseline_tmp_weighted))

nrow(AGIMPACTS_WEIGHTED_POOLED) # 4138

# remove outliers for precipitation? removes up to half the data points - may not be necessary either

AGIMPACTS_WEIGHTED_POOLED %>% 
  filter(Precipitation.change >= 0 & Precipitation.change <= 100)


crop_specific_models_no_fe <- function(DATA){
  
  wheat <<- DATA %>% 
    filter(crop_pooled == "Wheat")
  
  wheat_damages <<- lm(Yield.Change ~ Temp.Change + I(Temp.Change^2) +
                         Temp.Change:Baseline_tmp_weighted + I(Temp.Change^2):Baseline_tmp_weighted +
                         f_CO2:C3  + 
                         Precipitation.change +
                         Temp.Change:adapt_dummy +
                         adapt_dummy - 1,
                       data = wheat)
  
  summary(wheat_damages) 
  
  # rice
  
  rice <<- DATA %>% 
    filter(crop_pooled == "Rice")
  
  rice_damages <<- lm(Yield.Change ~ Temp.Change + I(Temp.Change^2) +
                        Temp.Change:Baseline_tmp_weighted + I(Temp.Change^2):Baseline_tmp_weighted +
                        f_CO2:C3  + 
                        Precipitation.change +
                        Temp.Change:adapt_dummy +
                        adapt_dummy - 1,
                      data = rice)
  
  summary(rice_damages) 
  
  # soybean
  
  soybean <<- DATA %>% 
    filter(crop_pooled == "Soybean")
  
  soybean_damages <<- lm(Yield.Change ~ Temp.Change + I(Temp.Change^2) +
                           Temp.Change:Baseline_tmp_weighted + I(Temp.Change^2):Baseline_tmp_weighted +
                           f_CO2:C3  + 
                           Precipitation.change +
                           Temp.Change:adapt_dummy +
                           adapt_dummy - 1,
                         data = soybean)
  
  summary(soybean_damages) 
  
  # maize
  
  maize <<- DATA %>% 
    filter(crop_pooled == "Maize")
  
  maize_damages <<- lm(Yield.Change ~ Temp.Change + I(Temp.Change^2) +
                         Temp.Change:Baseline_tmp_weighted + I(Temp.Change^2):Baseline_tmp_weighted +
                         f_CO2:C4  + 
                         Precipitation.change +
                         Temp.Change:adapt_dummy +
                         adapt_dummy - 1,
                       data = maize)
  
  summary(maize_damages) 
  
  # ggpredict() wheat
  
  predicted_wheat <<- ggpredict(wheat_damages, 
                                terms = c("Temp.Change", "Baseline_tmp_weighted [quart]"), 
                                condition = c(adapt_dummy = 0, f_CO2 = 0))
  
  
  
  # ggeffect() wheat
  
  effects_wheat <<- ggeffect(wheat_damages, 
                             terms = c("Temp.Change", "Baseline_tmp_weighted [quart]"), 
                             condition = c(adapt_dummy = 0, f_CO2 = 0))
  
  
  
  # try brms::marginal_effects()
  
  # ggpredict() rice
  
  predicted_rice <<- ggpredict(rice_damages, 
                               terms = c("Temp.Change", "Baseline_tmp_weighted [quart]"), 
                               condition = c(adapt_dummy = 0, f_CO2 = 0))
  
  
  
  # ggeffect() rice
  
  effects_rice <<- ggeffect(rice_damages, 
                            terms = c("Temp.Change", "Baseline_tmp_weighted [quart]"), 
                            condition = c(adapt_dummy = 0, f_CO2 = 0))
  
  
  
  # ggpredict() soy
  
  predicted_soybean <<- ggpredict(soybean_damages, 
                                  terms = c("Temp.Change", "Baseline_tmp_weighted [quart]"), 
                                  condition = c(adapt_dummy = 0, f_CO2 = 0))
  
  
  # ggeffect() soy
  
  effects_soybean <<- ggeffect(soybean_damages, 
                               terms = c("Temp.Change", "Baseline_tmp_weighted [quart]"), 
                               condition = c(adapt_dummy = 0, f_CO2 = 0))
  
  
  
  # ggpredict() maize
  
  predicted_maize <<- ggpredict(maize_damages, 
                                terms = c("Temp.Change", "Baseline_tmp_weighted [quart]"), 
                                condition = c(adapt_dummy = 0, f_CO2 = 0))
  
  
  # ggeffect() maize
  
  effects_maize <<- ggeffect(maize_damages, 
                             terms = c("Temp.Change", "Baseline_tmp_weighted [quart]"), 
                             condition = c(adapt_dummy = 0, f_CO2 = 0))
  
  
  
}

crop_specific_models_no_adapt <- function(DATA){
  
  wheat <<- DATA %>% 
    filter(crop_pooled == "Wheat" & adapt_dummy == 0)
  
  wheat_damages <<- lm(Yield.Change ~ Temp.Change + I(Temp.Change^2) +
                         Temp.Change:Baseline_tmp_weighted + I(Temp.Change^2):Baseline_tmp_weighted +
                         f_CO2:C3  + 
                         Precipitation.change +
                         + Country2 - 1,
                       data = wheat)
  
  summary(wheat_damages) 
  
  # rice
  
  rice <<- DATA %>% 
    filter(crop_pooled == "Rice" & adapt_dummy == 0)
  
  rice_damages <<- lm(Yield.Change ~ Temp.Change + I(Temp.Change^2) +
                        Temp.Change:Baseline_tmp_weighted + I(Temp.Change^2):Baseline_tmp_weighted +
                        f_CO2:C3  + 
                        Precipitation.change +
                        Country2 - 1,
                      data = rice)
  
  summary(rice_damages) 
  
  # soybean
  
  soybean <<- DATA %>% 
    filter(crop_pooled == "Soybean" & adapt_dummy == 0)
  
  soybean_damages <<- lm(Yield.Change ~ Temp.Change + I(Temp.Change^2) +
                           Temp.Change:Baseline_tmp_weighted + I(Temp.Change^2):Baseline_tmp_weighted +
                           f_CO2:C3  + 
                           Precipitation.change + Country2 - 1,
                         data = soybean)
  
  summary(soybean_damages) 
  
  # maize
  
  maize <<- DATA %>% 
    filter(crop_pooled == "Maize" & adapt_dummy == 0)
  
  maize_damages <<- lm(Yield.Change ~ Temp.Change + I(Temp.Change^2) +
                         Temp.Change:Baseline_tmp_weighted + I(Temp.Change^2):Baseline_tmp_weighted +
                         f_CO2:C4  + 
                         Precipitation.change + Country2 - 1,
                       data = maize)
  
  summary(maize_damages) 
  
  
  # ggeffect() wheat
  
  effects_wheat <<- ggeffect(wheat_damages, 
                             terms = c("Temp.Change", "Baseline_tmp_weighted [quart]"), 
                             condition = c(adapt_dummy = 0, f_CO2 = 0),
                             type = "fe")
  
  
  
  # try brms::marginal_effects()
  
  
  # ggeffect() rice
  
  effects_rice <<- ggeffect(rice_damages, 
                            terms = c("Temp.Change", "Baseline_tmp_weighted [quart]"), 
                            condition = c(adapt_dummy = 0, f_CO2 = 0),
                            type = "fe")
  
  
  
  
  
  # ggeffect() soy
  
  effects_soybean <<- ggeffect(soybean_damages, 
                               terms = c("Temp.Change", "Baseline_tmp_weighted [quart]"), 
                               condition = c(adapt_dummy = 0, f_CO2 = 0),
                               type = "fe")
  
  
  
  
  
  # ggeffect() maize
  
  effects_maize <<- ggeffect(maize_damages, 
                             terms = c("Temp.Change", "Baseline_tmp_weighted [quart]"), 
                             condition = c(adapt_dummy = 0, f_CO2 = 0),
                             type = "fe")
  
  
  
}

all_crops_model <- function(DATA){
  
  
  damages <<- lm(Yield.Change ~ Temp.Change:crop_pooled + I(Temp.Change^2):crop_pooled +
                   Temp.Change:crop_pooled:Baseline_tmp_weighted + 
                   I(Temp.Change^2):crop_pooled:Baseline_tmp_weighted +
                   f_CO2:C3 + f_CO2:C4  + 
                   Precipitation.change +
                   Temp.Change:adapt_dummy +
                   adapt_dummy + Country2 - 1,
                 data = DATA)
  
  
  # ggeffect() wheat
  
  effects_crops <<- ggeffect(damages, 
                             terms = c("Temp.Change", "Baseline_tmp_weighted [quart]", "crop_pooled"), 
                             condition = c(adapt_dummy = 0, f_CO2 = 0),
                             type = "fe")
  
  predicted_crops_quantile <<- ggpredict(damages, 
                                         terms = c("Temp.Change", "Baseline_tmp_weighted [quart]", "crop_pooled"), 
                                         condition = c(adapt_dummy = 0, Precipitation.change = 0, f_CO2 = 0))
  
  predicted_crops_values <<- ggpredict(damages, 
                                       terms = c("Temp.Change", "Baseline_tmp_weighted [5:25 by=5]", "crop_pooled"), 
                                       condition = c(adapt_dummy = 0, Precipitation.change = 0, f_CO2 = 0, Country2 = "AU"))
  
}

all_crops_model_no_fe <- function(DATA){
  
  
  damages <<- lm(Yield.Change ~ 0 + Temp.Change:crop_pooled + I(Temp.Change^2):crop_pooled +
                   Temp.Change:crop_pooled:Baseline_tmp_weighted + 
                   I(Temp.Change^2):crop_pooled:Baseline_tmp_weighted +
                   # crop_pooled:Baseline_tmp_weighted + ### NOTE THIS CHANGE ###
                   f_CO2:C3 + f_CO2:C4  + 
                   Precipitation.change +
                   Temp.Change:adapt_dummy +
                   adapt_dummy,
                 data = DATA)
  
  
  # ggeffect() wheat
  
  effects_crops <<- ggeffect(damages, 
                             terms = c("Temp.Change", "Baseline_tmp_weighted [quart]", "crop_pooled"), 
                             condition = c(Precipitation.change = 0, adapt_dummy = 0, f_CO2 = 0))
  
  predicted_crops_quantile <<- ggpredict(damages, 
                                         terms = c("Temp.Change", "Baseline_tmp_weighted [quart]", "crop_pooled"), 
                                         condition = c(adapt_dummy = 0, Precipitation.change = 0, f_CO2 = 0))
  
  predicted_crops_values <<- ggpredict(damages, 
                                       terms = c("Temp.Change", "Baseline_tmp_weighted [18:26 by = 2]", "crop_pooled"), 
                                       condition = c(adapt_dummy = 0, Precipitation.change = 0, f_CO2 = 0))
}

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
  
  predicted_crops <<- ggpredict(damages_cubic, 
                                terms = c("Temp.Change", "Baseline_tmp_weighted [median]", "crop_pooled"), 
                                condition = c(Precipitation.change = 0, adapt_dummy = 0, f_CO2 = 0))
  
}

all_crops_model_no_fe_adapt <- function(DATA){
  
  
  damages <<- lm(Yield.Change ~ 0 + Temp.Change:crop_pooled + I(Temp.Change^2):crop_pooled + 
                   Temp.Change:crop_pooled:Baseline_tmp_weighted + 
                   I(Temp.Change^2):crop_pooled:Baseline_tmp_weighted +
                   f_CO2:C3 + f_CO2:C4  + 
                   Precipitation.change +
                   Temp.Change:Adaptation +
                   Adaptation,
                 data = DATA)
  
  
  # ggeffect() wheat
  
  effects_crops <<- ggeffect(damages, 
                             terms = c("Temp.Change", "Baseline_tmp_weighted [quart]", "crop_pooled"), 
                             condition = c(Precipitation.change = 0, f_CO2 = 0))
  
  predicted_crops_quantile <<- ggpredict(damages, 
                                         terms = c("Temp.Change", "Baseline_tmp_weighted [quart]", "crop_pooled"), 
                                         condition = c(Precipitation.change = 0, f_CO2 = 0))
  
  predicted_crops_values <<- ggpredict(damages, 
                                       terms = c("Temp.Change", "Baseline_tmp_weighted [5:25 by=5]", "crop_pooled"), 
                                       condition = c(Precipitation.change = 0, f_CO2 = 0))
}

all_crops_model_baseline_2000 <- function(DATA){
  
  
  damages <<- lm(Yield.Change ~ Temp.Change:crop_pooled + I(Temp.Change^2):crop_pooled +
                   Temp.Change:crop_pooled:baseline_tmp_2000 + 
                   I(Temp.Change^2):crop_pooled:baseline_tmp_2000 +
                   f_CO2:C3 + f_CO2:C4  + 
                   Precipitation.change +
                   Temp.Change:adapt_dummy +
                   adapt_dummy + Country2 - 1,
                 data = DATA)
  
  
  # ggeffect() wheat
  
  effects_crops <<- ggeffect(damages, 
                             terms = c("Temp.Change", "baseline_tmp_2000 [quart]", "crop_pooled"), 
                             condition = c(adapt_dummy = 0, f_CO2 = 0),
                             type = "fe")
  
  predicted_crops_quantile <<- ggpredict(damages, 
                                         terms = c("Temp.Change", "baseline_tmp_2000 [quart]", "crop_pooled"), 
                                         condition = c(adapt_dummy = 0, Precipitation.change = 0, f_CO2 = 0))
  
  predicted_crops_values <<- ggpredict(damages, 
                                       terms = c("Temp.Change", "baseline_tmp_2000 [5:25 by=5]", "crop_pooled"), 
                                       condition = c(adapt_dummy = 0, Precipitation.change = 0, f_CO2 = 0, Country2 = "AU"))
  
}

all_crops_model_no_fe_baseline_2000 <- function(DATA){
  
  
  damages <<- lm(Yield.Change ~ 0 + Temp.Change:crop_pooled + I(Temp.Change^2):crop_pooled +
                   Temp.Change:crop_pooled:baseline_tmp_2000 + 
                   I(Temp.Change^2):crop_pooled:baseline_tmp_2000 +
                   f_CO2:C3 + f_CO2:C4  + 
                   Precipitation.change +
                   Temp.Change:adapt_dummy +
                   adapt_dummy,
                 data = DATA)
  
  
  # ggeffect() wheat
  
  effects_crops <<- ggeffect(damages, 
                             terms = c("Temp.Change", "baseline_tmp_2000 [quart]", "crop_pooled"), 
                             condition = c(Precipitation.change = 0, adapt_dummy = 0, f_CO2 = 0))
  
  predicted_crops_quantile <<- ggpredict(damages, 
                                         terms = c("Temp.Change", "baseline_tmp_2000 [quart]", "crop_pooled"), 
                                         condition = c(adapt_dummy = 0, Precipitation.change = 0, f_CO2 = 0))
  
  predicted_crops_values <<- ggpredict(damages, 
                                       terms = c("Temp.Change", "baseline_tmp_2000 [5:25 by=5]", "crop_pooled"), 
                                       condition = c(adapt_dummy = 0, Precipitation.change = 0, f_CO2 = 0))
}

all_crops_model_baseline_1995_2005 <- function(DATA){
  
  
  damages <<- lm(Yield.Change ~ Temp.Change:crop_pooled + I(Temp.Change^2):crop_pooled +
                   Temp.Change:crop_pooled:Baseline_tmp_1995_2005_weighted + 
                   I(Temp.Change^2):crop_pooled:Baseline_tmp_1995_2005_weighted +
                   f_CO2:C3 + f_CO2:C4  + 
                   Precipitation.change +
                   Temp.Change:adapt_dummy +
                   adapt_dummy + Country2 - 1,
                 data = DATA)
  
  
  # ggeffect() wheat
  
  effects_crops <<- ggeffect(damages, 
                             terms = c("Temp.Change", "Baseline_tmp_1995_2005_weighted [quart]", "crop_pooled"), 
                             condition = c(adapt_dummy = 0, f_CO2 = 0),
                             type = "fe")
  
  predicted_crops_quantile <<- ggpredict(damages, 
                                         terms = c("Temp.Change", "Baseline_tmp_1995_2005_weighted [quart]", "crop_pooled"), 
                                         condition = c(adapt_dummy = 0, Precipitation.change = 0, f_CO2 = 0))
  
  predicted_crops_values <<- ggpredict(damages, 
                                       terms = c("Temp.Change", "Baseline_tmp_1995_2005_weighted [5:25 by=5]", "crop_pooled"), 
                                       condition = c(adapt_dummy = 0, Precipitation.change = 0, f_CO2 = 0, Country2 = "AU"))
  
}

all_crops_model_no_fe_baseline_1995_2005 <- function(DATA){
  
  
  damages <<- lm(Yield.Change ~ 0 + Temp.Change:crop_pooled + I(Temp.Change^2):crop_pooled +
                   Temp.Change:crop_pooled:Baseline_tmp_1995_2005_weighted + 
                   I(Temp.Change^2):crop_pooled:Baseline_tmp_1995_2005_weighted +
                   f_CO2:C3 + f_CO2:C4  + 
                   Precipitation.change +
                   Temp.Change:adapt_dummy +
                   adapt_dummy,
                 data = DATA)
  
  
  # ggeffect() wheat
  
  effects_crops <<- ggeffect(damages, 
                             terms = c("Temp.Change", "Baseline_tmp_1995_2005_weighted [quart]", "crop_pooled"), 
                             condition = c(Precipitation.change = 0, adapt_dummy = 0, f_CO2 = 0))
  
  predicted_crops_quantile <<- ggpredict(damages, 
                                         terms = c("Temp.Change", "Baseline_tmp_1995_2005_weighted [quart2]", "crop_pooled"), 
                                         condition = c(adapt_dummy = 0, Precipitation.change = 0, f_CO2 = 0))
  
  predicted_crops_values <<- ggpredict(damages, 
                                       terms = c("Temp.Change", "Baseline_tmp_1995_2005_weighted [5:25 by=5]", "crop_pooled"), 
                                       condition = c(adapt_dummy = 0, Precipitation.change = 0, f_CO2 = 0))
}


# Alternative datasets fit to specifications --------------------------------------------------------

# plot distribution of baseline temp

histogram(AGIMPACTS_TRUNCATED_ADAPT$Baseline_tmp_weighted)
qplot(AGIMPACTS_TRUNCATED_ADAPT$Baseline_tmp_weighted, geom="histogram", binwidth = 0.05)
mean(AGIMPACTS_TRUNCATED_ADAPT$Baseline_tmp_weighted > 15, na.rm = TRUE)
mean(AGIMPACTS_TRUNCATED_ADAPT$Baseline_tmp_weighted > 18, na.rm = TRUE)

# if excluding korea
AGIMPACTS_TRUNCATED_NO_KOREA <- AGIMPACTS_TRUNCATED_ADAPT %>% 
  filter(Country2 != "KR")

# take proportional subset of korean point estimates

AGIMPACTS_TRUNCATED_ADAPT %>% group_by(Country2) %>% summarise(n=n()) %>% print(n=Inf)

AGIMPACTS_TRUNCATED_ADAPT %>% filter(Country2 == 'KR') %>% 
  group_by(Temp.Change, Yield.Change, Precipitation.change, CO2.Change) %>% 
  summarise(n=n()) %>% 
  print(n=Inf)

# test removing positive yield.change estimates - note this will bias yield.change coefficient on rice as non-random sampling
AGIMPACTS_TRUNCATED_ADAPT_KR <- AGIMPACTS_TRUNCATED_ADAPT %>% 
  filter(!(Country2 == "KR" & Yield.Change > 0))

dim(AGIMPACTS_TRUNCATED_ADAPT) - dim(AGIMPACTS_TRUNCATED_ADAPT_KR) # thrown out 785 data points 

# baseline period of 1995-2005

mean(AGIMPACTS_TRUNCATED_NO_KOREA$Baseline_tmp_weighted > 15, na.rm = TRUE)
mean(AGIMPACTS_TRUNCATED_NO_KOREA$Baseline_tmp_weighted > 18, na.rm = TRUE)

AGIMPACTS_TRUNCATED_ADAPT %>%
  group_by(Baseline.start, Baseline.end) %>% 
  summarise(n=n()) %>% 
  print(n=Inf)

# baseline period of 1995-2005

# (option 1)
# use baseline tmp at 2000, calculated previously in avg_tmp_country
# first need to match to point estimate row by country and crop
# note that we are only using the four major crops so it's ok to only match by these, first change "Soybeans" to "Soybean"
# pivot long into single column baseline_tmp_2000

avg_tmp_country_matching <- avg_tmp_country %>% 
  rename(Soybean = Soybeans) %>% 
  dplyr::select(!c(Rice.2, Maize.2, Wheat.Winter, country)) %>% 
  pivot_longer(!abbrev, names_to = "crop_pooled", values_to = "baseline_tmp_2000")

AGIMPACTS_BASELINE_PERIOD <- AGIMPACTS_TRUNCATED_ADAPT %>% 
  left_join(avg_tmp_country_matching, by = c("Country2" = "abbrev", "crop_pooled"))

# note baseline_tmp_2000 is unweighted, will need to use exactextractr to weight by crop production fractions

# (option 2)
# to properly use baseline period of 1995-2005, need to calculate average over the 10 year period, weighted

baseline_production_1995_2005_list <- list()

baseline_tmp_averaging_1995_2005 <- function(i){ # rewrite this function to include data set variables
  
  for (i in 1:nrow(baseline_df)) {
    
    baseline_df_test <- baseline_df[i,]
    
    baseline_test_series <- c(1995:2005)
    
    baseline_test_tmp_series <- paste("tmp", baseline_test_series, sep = "_")
    
    avg <- growing_season_tmp_production %>% 
      filter(abbrev %in% baseline_df_test$Country2 & baseline_crop %in% baseline_df_test$Crop) %>% 
      dplyr::select(all_of(baseline_test_tmp_series)) %>% # use external vector to select columns in growing_season_tmp_series
      rowMeans() 
    
    avg <- as.data.frame(avg)
    avg$i <- i
    
    baseline_production_1995_2005_list[[i]] <<- avg
    
  }
}

baseline_tmp_averaging_1995_2005(baseline_df)

# convert list to dataframe

(baseline_avg_tmp_production_1995_2005 <- do.call(rbind.data.frame, baseline_production_1995_2005_list)) 

baseline_avg_tmps_production_1995_2005 <- baseline_df %>% 
  mutate(index = c(1:141)) %>% 
  left_join(baseline_avg_tmp_production_1995_2005, by = c("index" = "i"))


# join with AGIMPACTS_TRUNCATED_ADAPT

AGIMPACTS_TRUNCATED_BASELINE_1995_2005 <- AGIMPACTS_TRUNCATED_ADAPT %>% 
  left_join(baseline_avg_tmps_production_1995_2005, by = c("Crop", "Country2")) %>% 
  dplyr::select(!c("nyears", "index")) %>% 
  rename(Baseline_tmp_1995_2005_weighted = avg)

save.image("agimpacts-230721.RData")


# Run model specifications ------------------------------------------------


# test alternative model specifications

all_crops_model(AGIMPACTS_WEIGHTED_PROD)

all_crops_model_no_fe(AGIMPACTS_WEIGHTED_PROD)


crop_specific_models(DATA = AGIMPACTS_WEIGHTED_PROD)
crop_specific_models(DATA = AGIMPACTS_TRUNCATED_DATA) 

crop_specific_models_no_fe(DATA = AGIMPACTS_WEIGHTED_PROD)
crop_specific_models_no_fe(DATA = AGIMPACTS_TRUNCATED_DATA) 

crop_specific_models_no_adapt(DATA = AGIMPACTS_TRUNCATED_DATA) 

# preferred model

all_crops_model_no_fe(AGIMPACTS_FINAL_INCOMPLETE)

all_crops_model_no_fe(AGIMPACTS_TRUNCATED_DATA) # preferred
all_crops_model(AGIMPACTS_TRUNCATED_DATA) 

all_crops_model_no_fe(AGIMPACTS_TRUNCATED_2005)
all_crops_model_no_fe_cubic(AGIMPACTS_TRUNCATED_DATA)
all_crops_model_no_fe_adapt(AGIMPACTS_TRUNCATED_ADAPT) # note this, have coalesced "No " with "No" 

all_crops_model_no_fe(AGIMPACTS_TRUNCATED_NO_KOREA)
all_crops_model(AGIMPACTS_TRUNCATED_NO_KOREA)

all_crops_model_no_fe(AGIMPACTS_TRUNCATED_ADAPT_KR)
all_crops_model(AGIMPACTS_TRUNCATED_ADAPT_KR)

all_crops_model_no_fe_baseline_2000(AGIMPACTS_BASELINE_PERIOD)
all_crops_model_baseline_2000(AGIMPACTS_BASELINE_PERIOD)

all_crops_model_no_fe_baseline_1995_2005(AGIMPACTS_TRUNCATED_BASELINE_1995_2005) # preferred
all_crops_model_baseline_1995_2005(AGIMPACTS_TRUNCATED_BASELINE_1995_2005)

# fitted values and residuals

fitted <- predict(damages)
residuals <- residuals(damages)

# check lm results

summary(soybean_damages) 
summary(rice_damages) 
summary(wheat_damages) 
summary(maize_damages) 

sink("soybean_damages.txt")
print(summary(soybean_damages))

sink("rice_damages.txt")
print(summary(rice_damages))

sink("wheat_damages.txt")
print(summary(wheat_damages))

sink("maize_damages.txt")
print(summary(maize_damages))

sink("all_crops_no_fe.txt")
print(summary(damages))
sink()

sink("all_crops.txt")
print(summary(damages))
sink()

sink("all_crops_no_fe_2005.txt")
print(summary(damages))
sink()

sink("all_crops_no_fe_cubic.txt")
print(summary(damages))
sink()

sink("all_crops_no_fe_adapt.txt")
print(summary(damages))
sink()

sink("all_crops_no_fe_no_KR.txt")
print(summary(damages))
sink()

sink("all_crops_no_fe_baseline_1995_2005.txt")
print(summary(damages))
sink()

# effect plots

plot(effects_crops) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-50, 50))

# preferred model predicted values
plot(predicted_crops) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-50, 50))

plot(predicted_crops_quantile) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-50, 50))

plot(predicted_crops_values) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-50, 50))


# plot marginal effects

plot(predicted_soybean) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-50, 50))

# plot(predicted_soybean) 

plot(effects_rice) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-50, 50))

# plot(predicted_rice)

plot(effects_wheat) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-50, 50))

plot(effects_maize) +
  scale_x_continuous(limits = c(0, 5)) +
  scale_y_continuous(limits = c(-50, 50))

# plot(predicted_maize)

# save.image("ag-230621.RData")

AGIMPACTS_TRUNCATED_DATA %>% filter(crop_pooled == "Soybean") %>% 
  group_by(Temp.Change) %>% summarise(mean.Yield.Change = mean(Yield.Change)) %>% 
  print(n=Inf)



# run_13_cmip5_prediction_data --------------------------------------------



