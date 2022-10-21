
# THIS FOLLOWS FROM 05 BUT BRANCHES AS DO NOT IMPUTE DATA BEFORE PROCEEDING WITH OLS REGRESSION

AGIMPACTS_MAIN_COMPLETE <- AGIMPACTS_MAIN %>% 
  filter(!is.na(Temp.Change)) %>% 
  filter(!is.na(Precipitation.change)) %>% 
  filter(!is.na(CO2.Change)) %>% 
  filter(!is.na(Yield.Change))

dim(AGIMPACTS_MAIN_COMPLETE) # 4139 50, this matches the number of point estimates with 0 NAs across all four climate variable columns

nrow(AGIMPACTS_MAIN) - nrow(AGIMPACTS_MAIN_COMPLETE) # 4707

# filter for only non-NA Baseline.start and Baseline.end 
AGIMPACTS_MAIN_COMPLETE <- AGIMPACTS_MAIN_COMPLETE %>% 
  filter(!is.na(Baseline.start)) %>% 
  filter(!is.na(Baseline.end))

dim(AGIMPACTS_MAIN_COMPLETE) # 4139 50 - this means that any point estimates with missing baseline periods were also missing values in at least one of the four climate variable columns

baseline_df <- AGIMPACTS_MAIN_COMPLETE %>% 
  group_by(Crop, Country2, Baseline.start, Baseline.end) %>% 
  summarise(n=n()) %>% 
  mutate(nyears = Baseline.end - Baseline.start + 1) %>% 
  print(n=Inf) # 141 groups rather than 340

AGIMPACTS_MAIN_COMPLETE %>% 
  group_by(Baseline.start) %>% 
  summarise(n=n()) %>% 
  print(n=Inf) # earliest is now 1950

AGIMPACTS_MAIN %>% write.csv("agimpacts_main")
AGIMPACTS_MAIN_COMPLETE %>% write.csv("agimpacts_main_complete")


# for each i in baseline_df, 
# filter growing_season_tmp_series by Crop (baseline_df) = baseline_crop (growing_season_tmp_series) and Country2 (baseline_df) = abbrev (growing_season_tmp_series)
# then for series Baseline.start : Baseline.end in i, select each column year in that series and define as group/range of columns
# match to colnames in growing_season_tmp_series by string match, OR by column index location and year index location in time series of length 110? which is more efficient? e.g. seq(1901, 2010, 1)
# go with first option# compute rowMean across the group of columns
# store in list, iterate on next i

baseline_df <- as.data.frame(baseline_df)

# function to do dynamic average over baseline periods ----------------------------------------------------------------

baseline_production_list <- list()

baseline_tmp_averaging <- function(i){ # rewrite this function to include data set variables
  
  for (i in 1:nrow(baseline_df)) {
    
    baseline_df_test <- baseline_df[i,]
    
    baseline_test_series <- c(eval(expr(baseline_df_test$Baseline.start)):eval(expr(baseline_df_test$Baseline.end)))
    
    baseline_test_tmp_series <- paste("tmp", baseline_test_series, sep = "_")
    
    avg <- growing_season_tmp_production %>% 
      filter(abbrev %in% baseline_df_test$Country2 & baseline_crop %in% baseline_df_test$Crop) %>% 
      dplyr::select(all_of(baseline_test_tmp_series)) %>% # use external vector to select columns in growing_season_tmp_series
      rowMeans() 
    
    avg <- as.data.frame(avg)
    avg$i <- i
    
    baseline_production_list[[i]] <<- avg
    
  }
}

baseline_tmp_averaging(baseline_df)

# convert list to dataframe

(baseline_avg_tmp_production <- do.call(rbind.data.frame, baseline_production_list)) 

baseline_avg_tmps_production <- baseline_df %>% 
  mutate(index = c(1:141)) %>% 
  left_join(baseline_avg_tmp_production, by = c("index" = "i"))

# compare with unweighted temperatures ------------------------------------

# compare with raster::extract baseline average temperatures "baseline_avg_tmps", save differences
# create comparison table
compare_production_weighting <- baseline_avg_tmps_production %>% 
  rename(production_weighted_avg_tmp = avg) %>% 
  left_join(baseline_avg_tmps) %>% 
  rename(unweighted_avg_tmp = avg) %>% 
  mutate(diff_production_weighting = production_weighted_avg_tmp - unweighted_avg_tmp)

compare_production_weighting %>% write.csv("compare_production_weighting.csv")

# we can see this fixes most of the problematically low temperatures e.g. in CA

# join with AGIMPACTS_MAIN_COMPLETED to create AGIMPACTS_MAIN_COMPLETED_WEIGHTED --------

AGIMPACTS_MAIN_COMPLETED_WEIGHTED <- AGIMPACTS_MAIN_COMPLETED %>% 
  left_join(baseline_avg_tmps_production, by = c("Crop", "Country2", "Baseline.start", "Baseline.end")) %>% 
  dplyr::select(!c("n", "nyears", "index")) %>% 
  rename(Baseline_tmp_weighted = avg)

dim(AGIMPACTS_MAIN_COMPLETED_WEIGHTED) # 4139 51


AGIMPACTS_MAIN_COMPLETED_WEIGHTED %>% readr::write_csv(here("processed", "agimpacts_main_completed_weighted_v2.csv"))


