
# THIS FOLLOWS FROM 05 BUT BRANCHES INTO GAMM MODELLING AS MUST IMPUTE DATA BEFORE PROCEEDING 

AGIMPACTS_MAIN <- readRDS(here("processed", "AGIMPACTS_MAIN.Rdata")) 


# identify unique baseline periods in CGIAR data --------

# estimate production-weighted baseline growing season temperature based on complete cases of baseline start/end period

baseline_df_incomplete <- AGIMPACTS_MAIN %>% 
  group_by(Crop, Country2, Baseline.start, Baseline.end) %>% 
  summarise(n=n()) %>% 
  mutate(nyears = Baseline.end - Baseline.start + 1) %>% 
  print(n=Inf) # 340 groups

# remove NA baseline start/end years

sum(is.na(baseline_df_incomplete$Baseline.start)) # 7

baseline_incomplete_na <- baseline_df_incomplete %>% 
  filter(!is.na(Baseline.start) | !is.na(Baseline.end))

dim(baseline_incomplete_na) # 333 rows/groups

baseline_incomplete_na[order(baseline_incomplete_na$Baseline.start),] # index 248, AU Wheat 1901:2001 with 20 point estimates is the only group with baseline start year in 1901

baseline_incomplete_na <- baseline_incomplete_na %>% 
  mutate(Baseline.start = replace(Baseline.start, Baseline.start == 1901, 1950)) # manually changed to 1950 - at some point can compute over 1901-2001

# # calculate country-data point baseline growing temperature ---- --------

# function to do dynamic average over baseline periods 

baseline_tmp_averaging_incomplete <- function(i){
  
  baseline_df_test <- baseline_incomplete_na[i,]
  
  baseline_test_series <- c(eval(expr(baseline_df_test$Baseline.start)):eval(expr(baseline_df_test$Baseline.end)))
  
  baseline_test_tmp_series <- paste("tmp", baseline_test_series, sep = "_")
  
  avg <- growing_season_tmp_production %>% # this has changed 11/02/22 with crop production unadjustment
    filter(abbrev %in% baseline_df_test$Country2 & baseline_crop %in% baseline_df_test$Crop) %>% 
    dplyr::select(all_of(baseline_test_tmp_series)) %>% # use external vector to select columns in growing_season_tmp_series
    rowMeans() 
  
  avg <- as.data.frame(avg)

}

# growing_season_tmp_production only from 1950-2010
baseline_temp_incomplete <- lapply(1:333, baseline_tmp_averaging_incomplete)


# convert list to dataframe

baseline_avg_tmp_production_incomplete <- rbindlist(baseline_temp_incomplete, idcol = "i") 

baseline_avg_tmps_production_incomplete <- baseline_incomplete_na %>% 
  mutate(index = c(1:333)) %>% 
  left_join(baseline_avg_tmp_production_incomplete, by = c("index" = "i"))


# join estimated bs growing temp with CGIAR data --------------------------

# note that this naming convention is not done well, as AGIMPACTS_MAIN_INCOMPLETE represents bs temp only
# may make sense when redoing everything per targets() to come back and rename this AGIMPACTS_BS_TEMP
# keep this for now such that we change the file naming as little as possible and keep dependencies clear 
# and as consistent as possible with the agimpacts repo

AGIMPACTS_MAIN_INCOMPLETE <- AGIMPACTS_MAIN %>% 
  left_join(baseline_avg_tmps_production_incomplete, by = c("Crop", "Country2", "Baseline.start", "Baseline.end")) %>% 
  dplyr::select(!c("n", "nyears")) %>% 
  rename(Baseline_tmp_weighted = avg)

dim(AGIMPACTS_MAIN_INCOMPLETE) # 8846   52

dim(AGIMPACTS_MAIN) # 8846   50


AGIMPACTS_MAIN_INCOMPLETE$Reference_fact <- as.factor(AGIMPACTS_MAIN_INCOMPLETE$Reference) # 74 levels
AGIMPACTS_MAIN_INCOMPLETE$Reference_int <- as.integer(AGIMPACTS_MAIN_INCOMPLETE$Reference_fact)
AGIMPACTS_MAIN_INCOMPLETE$Country2_int <- as.factor(AGIMPACTS_MAIN_INCOMPLETE$Country2) # 117 levels

AGIMPACTS_MAIN_INCOMPLETE %>% readr::write_csv(here("processed", "AGIMPACTS_MAIN_INCOMPLETE.csv"))

# note that script 07 on plotting missing data was done on this dataset alone


# calculate country-data point baseline growing precipitation -------------


baseline_pre_averaging_incomplete <- function(i){
  
  baseline_df_test <- baseline_incomplete_na[i,]
  
  baseline_test_series <- c(eval(expr(baseline_df_test$Baseline.start)):eval(expr(baseline_df_test$Baseline.end)))
  
  baseline_test_pre_series <- paste("pre", baseline_test_series, sep = "_")
  
  avg <- growing_season_pre_production %>% # this has changed 11/02/22 with crop production unadjustment
    filter(abbrev %in% baseline_df_test$Country2 & baseline_crop %in% baseline_df_test$Crop) %>% 
    dplyr::select(all_of(baseline_test_pre_series)) %>% # use external vector to select columns in growing_season_tmp_series
    rowMeans() 
  
  avg <- as.data.frame(avg)
  
}

# growing_season_tmp_production only from 1950-2010
baseline_pre_incomplete <- lapply(1:333, baseline_pre_averaging_incomplete)


# convert list to dataframe

baseline_avg_pre_production_incomplete <- rbindlist(baseline_pre_incomplete, idcol = "i") 

baseline_avg_pre_production_incomplete <- baseline_incomplete_na %>% 
  mutate(index = c(1:333)) %>% 
  left_join(baseline_avg_pre_production_incomplete, by = c("index" = "i"))


# join estimated bs growing precip with CGIAR data ------------------------


AGIMPACTS_bs_temp_precip <- AGIMPACTS_MAIN_INCOMPLETE %>% 
  left_join(baseline_avg_pre_production_incomplete, by = c("Crop", "Country2", "Baseline.start", "Baseline.end")) %>% 
  dplyr::select(!c("n", "nyears")) %>% 
  rename(Baseline_pre_weighted = avg)

# note that AGIMPACTS_bs_temp_precip replaces AGIMPACTS_FINAL_INCOMPLETE which was a copy of AGIMPACTS_MAIN_INCOMPLETE
# so any processed csv files of AGIMPACTS_FINAL_INCOMPLETE reflect CGIAR data merged with bs-temp-only + further data manipulations 

# DATA CLEANING -----------------------------------------------------------

# pool crops for the four crop values

AGIMPACTS_bs_tp <- AGIMPACTS_bs_temp_precip %>% 
  mutate(crop_pooled = case_when(Crop %in% c("Wheat", "Wheat (Spring)", "Wheat (Durum)", "Wheat (Winter)", "Wheat (Rainfed)", "Wheat (Irrigated)") ~ "Wheat",
                                 Crop %in% c("Rice", "Rice (Irrigated)", "Rice (Rainfed)") ~ "Rice",
                                 Crop %in% c("Maize", "Maize (Monsoon)", "Maize (Winter)") ~ "Maize",
                                 Crop == "Soybean" ~ "Soybean"))            

# split out adaptation by type rather than dummy, but first recode 
AGIMPACTS_bs_tp$Adaptation[AGIMPACTS_bs_tp$Adaptation %in% c("No", NA)] <- "No"

AGIMPACTS_bs_tp <- AGIMPACTS_bs_tp %>% 
  mutate(adapt_dummy = as.factor(if_else(Adaptation %in% c("No", "NA"), 0, 1)))

crops <- c("Maize", "Rice", "Soybean", "Wheat")

worldmap_clean <- cleangeo::clgeo_Clean(worldmap) 

# first create crop_production_stack in 05 script cru-temperature common steps
sum_production_country_raw <- exactextractr::exact_extract(crop_production_stack, worldmap_clean, 'sum') # note this is at 5 arcminute spatial resolution

country_production_sum_raw <- sum_production_country_raw %>% 
  setNames(paste0("production.", crops)) %>% 
  mutate(Country2=worldmap_clean$ISO_A2,
         CountryName=worldmap_clean$NAME) %>% 
  as.data.table() # note these are much lower than the adjusted production values. it may be wise to go back and re-weight the temperature values with the other approach too?


# given we want to use production tons explicitly for weighting across countries here, use unadjusted values - they are pretty close to original FAOSTAT data https://www.fao.org/faostat/en/#data/QCL

AGIMPACTS_bs_tp <- AGIMPACTS_bs_tp %>% 
  left_join(country_production_sum_raw, by="Country2")

# make country a factor variable

AGIMPACTS_bs_tp <- AGIMPACTS_bs_tp %>% 
  mutate(Country2_fact = as.factor(Country2))

AGIMPACTS_bs_tp %>% readr::write_csv(here("processed", "AGIMPACTS_bs_tp.csv"))

# save as R data file which preserves classes
saveRDS(AGIMPACTS_bs_tp, here("processed", "AGIMPACTS_bs_tp.Rdata")) 


