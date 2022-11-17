

# READ AGIMPACTS DATA -----------------------------------------------------

AGIMPACTS_FINAL <- readr::read_csv(here("processed", "agimpacts_final.csv"),
                                   col_types = readr::cols(.default = "c"))

# note this classes are inferred 1/08/22, may need to tweak based on implications for some analysis
AGIMPACTS_FINAL[,c(1,3,6,8,10,11,16:22,25,27,29:32)] <- lapply(AGIMPACTS_FINAL[,c(1,3,6,8,10,11,16:22,25,27,29:32)], as.numeric)

AGIMPACTS_FINAL[,c(15,23,24,26,28,33,34,36:39,40,43,44)] <- lapply(AGIMPACTS_FINAL[,c(15,23,24,26,28,33,34,36:39,40,43,44)], factor)

# leave country and crop as character for now, need to do some string manipulations on those first before factorising

saveRDS(AGIMPACTS_FINAL, here("processed", "AGIMPACTS_FINAL.RData"))
# add more variables

AGIMPACTS_FINAL_MAIN <- AGIMPACTS_FINAL %>% 
  filter(Crop %in% c("Maize", "Maize (Monsoon)", "Maize (Winter)",
                     "Rice", "Rice (Irrigated)", "Rice (Rainfed)",
                     "Wheat", "Wheat (Durum)", "Wheat (Irrigated)", "Wheat (Rainfed)", "Wheat (Spring)", "Wheat (Winter)",
                     "Soybean"))
# 9,273 observations
AGIMPACTS_FINAL_MAIN <- AGIMPACTS_FINAL_MAIN %>% 
  mutate(CO2.Change = CO2.Projected - CO2.Baseline)  

# Create C3/C4 photosynthetic pathway indicators
# Sage, R. F. and Zhu, X-G (2011) Exploiting the engine of C4 photosynthesis
# maize is C4; rice, wheat, soybean is C3

AGIMPACTS_FINAL_MAIN <- AGIMPACTS_FINAL_MAIN %>% 
  mutate(Photosynthetic_pathway = case_when(
    startsWith(Crop, "Maize") ~ "C4",
    startsWith(Crop, "Rice") ~ "C3",
    startsWith(Crop, "Wheat") ~ "C3",
    startsWith(Crop, "Soybean") ~ "C3"
  ))


AGIMPACTS_FINAL_MAIN <- AGIMPACTS_FINAL_MAIN %>% 
  filter(!is.na(Country))

saveRDS(AGIMPACTS_FINAL_MAIN, here("processed", "AGIMPACTS_FINAL_MAIN.Rdata"))

# READ MONFREDA DATA ------------------------------------------------------


#crop_production_files <- list.files(path = "Damages/", pattern = "^.*\\.(tif|TIF|Tif)$")

crop_production_files <- c("maize_Production.tif",
                           "rice_Production.tif",
                           "soybean_Production.tif",
                           "wheat_Production.tif")

#crop_country_lists <- data.frame(matrix(nrow=243))

crop_production_files2 <- here("data", "Monfreda data", crop_production_files)

# create worldmap boundaries

worldmap <- rworldmap::getMap(resolution = "coarse")

worldmap_clean <- cleangeo::clgeo_Clean(worldmap) # some polygons are not closed, i.e. Canada and Western Sahara


# extract TIF data

crop_production_lists <- function(i){
  crop_production <- raster(crop_production_files2[i])
  crop_production_country <- raster::extract(crop_production, worldmap, fun = sum, na.rm = TRUE) # 243 countries
  crop_country_df <- as.data.frame(crop_production_country)
  crop_country_df$name <- worldmap$NAME
  crop_country_df$abbrev <- worldmap$ISO_A2  
  #crop_country_lists <- cbind(crop_country_lists, crop_country_df) # next time instead of doing this, save as a list and then do cbind afterwards
  
  crop_country_df
}

crop_production <- lapply(1:4, crop_production_lists)

saveRDS(crop_production, here("processed", "crop_production.RData"))

crop_production_list <- do.call("cbind", crop_production)

# make V1 colnames unique to differentiate by crop
colnames(crop_production_list) <- make.unique(colnames(crop_production_list))

# remove duplicate country code and names
crop_production_list <- crop_production_list %>% 
  dplyr::select(!c(name.1, abbrev.1, name.2, abbrev.2, name.3, abbrev.3)) %>% 
  relocate(abbrev, name)

crop_production_list <- crop_production_list %>% dplyr::select(-3, -5, -7, -9)

crop_production_list <- crop_production_list %>% 
  rename(Y_maize = V1,
         Y_rice = V1.1,
         Y_soybean = V1.2,
         Y_wheat = V1.3)

# write crop_production to csv

crop_production_list %>% readr::write_csv(here("processed", "crop_production_list.csv"))

crop_production_list <-  readr::read_csv(here("processed", "crop_production_list.csv"))

# Sum Monfreda crop production by country and rank country by crop production ------------------------------

# in crop_production, generate new variable for each crop that computes country_rank
crop_production_list_ties <- crop_production_list %>% mutate(rank_maize = rank(desc(Y_maize), ties.method = "first"),
                                                             rank_rice = rank(desc(Y_rice), ties.method = "first"),
                                                             rank_soybean = rank(desc(Y_soybean), ties.method = "first"),
                                                             rank_wheat = rank(desc(Y_wheat), ties.method = "first")) 

# choose the country that has highest production of the multiple countries associated with the study data point
multicountrycrop <- AGIMPACTS_FINAL_MAIN %>% 
  group_by(Country, Crop) %>% 
  summarise(unique_countries = n_distinct(Country)) %>%
  mutate(c_occur = stringr::str_count(Country, '[A-Z]')/2) %>% 
  filter(c_occur != 1) %>% 
  dplyr::select(Country, Crop) %>% 
  as.data.frame()

# for each multi-country study, unlist all countries in one long datatable
multicountrycrop <- multicountrycrop %>% 
  mutate(index = 1:nrow(multicountrycrop)) %>% # 1:72
  relocate(index, Country, Crop)

# replace all ; in multicountrycrop with , - this takes care of the two rows in data validation that were imputed with country ; separators rather than ,
library(stringr)
multicountrycrop$Country <- str_replace_all(multicountrycrop$Country, ";", ",")

# https://stackoverflow.com/questions/15347282/split-delimited-strings-in-a-column-and-insert-as-new-rows
s <- strsplit(multicountrycrop$Country, split = ",")

multicountrycrop_long <- data.frame(index = rep(multicountrycrop$index, sapply(s, length)), 
                                    crop = rep(multicountrycrop$Crop, sapply(s, length)),
                                    Country = unlist(s))

# remove blank spaces

multicountrycrop_long$Country <- gsub(" ", "", multicountrycrop_long$Country, fixed = TRUE)


# index match crop_rank by crop_production vars rank_maize, rank_rice, rank_wheat, rank_soy
country_crop_rank <- left_join(multicountrycrop_long, crop_production_list_ties, by = c("Country" = "abbrev"))

# print the country with the highest country_rank in each study-crop indexed group:
sum_rank <- country_crop_rank %>% 
  group_by(index, crop) %>% 
  # currently index describes the 66 (study, crop) groups
  summarise(min_maize = min(rank_maize, na.rm = TRUE),
            min_rice = min(rank_rice, na.rm = TRUE),
            min_soybean = min(rank_soybean, na.rm = TRUE),
            min_wheat = min(rank_wheat, na.rm = TRUE)) %>%  # we will need to print the countries separately from crop_production
  print(n=Inf)

# now return country abbrev using repeated left_join for each crop
# e.g. on min_maize with crop_production$rank_maize

x <- sum_rank %>% 
  left_join(dplyr::select(crop_production_list_ties, abbrev, rank_maize), by = c("min_maize" = "rank_maize")) %>% 
  rename(Maize = abbrev) %>% 
  left_join(dplyr::select(crop_production_list_ties, abbrev, rank_rice), by = c("min_rice" = "rank_rice")) %>% 
  rename(Rice = abbrev) %>% 
  left_join(dplyr::select(crop_production_list_ties, abbrev, rank_soybean), by = c("min_soybean" = "rank_soybean")) %>% 
  rename(Soybean = abbrev) %>% 
  left_join(dplyr::select(crop_production_list_ties, abbrev, rank_wheat), by = c("min_wheat" = "rank_wheat")) %>% 
  rename(Wheat = abbrev)

# then use a fuzzy match to only retain the min_rank and country column that is relevant to the crop

y <- x %>% 
  dplyr::select(!c("min_maize", "min_rice", "min_soybean", "min_wheat"))

# now for each study-crop group index, mutate a new highest_y_country variable in the multicountrycrop dataframe

z <- y %>% 
  mutate(Country1 = case_when(
    crop == "Maize" ~ Maize,
    crop == "Rice" ~ Rice,
    crop == "Soybean" ~ Soybean,
    crop == "Wheat" ~ Wheat,
    crop == "Rice (Irrigated)" ~ Rice,
    crop == "Rice (Rainfed)" ~ Rice,
    crop == "Wheat (Irrigated)" ~ Wheat,
    crop == "Wheat (Rainfed)" ~ Wheat,
    crop == "Wheat (Durum)" ~ Wheat,
    crop == "Wheat (Spring)" ~ Wheat
  ))

# drop other vars

a <- z %>% dplyr::select(!c("Maize", "Rice", "Soybean", "Wheat"))

# append a to the multicountrycrop dataframe

multi_index <- multicountrycrop %>% 
  left_join(dplyr::select(a, index, Country1), by = "index")


# Assign country to agimpacts point estimates for multi-country study --------


# use this to mutate a new country variable in the agimpacts_maincrops dataframe
# left_join by both Country and Crop indices? 
# since we are left-joining by Country, need to make sure the new strings in the list of countries imputed for Moriondo have contain , rather than ; otherwise will not match

b <- AGIMPACTS_FINAL_MAIN$Country[AGIMPACTS_FINAL_MAIN$Reference == "Moriondo et al (2010)"]

AGIMPACTS_FINAL_MAIN$Country[AGIMPACTS_FINAL_MAIN$Reference == "Moriondo et al (2010)"] <- str_replace_all(b, ";", ",")

AGIMPACTS_MAIN <- left_join(AGIMPACTS_FINAL_MAIN, multi_index, by = c("Country", "Crop"))

# convert Country1 from factor to character
AGIMPACTS_MAIN$Country1 <- as.character(AGIMPACTS_MAIN$Country1)

# country1 now represents the imputed single country assignments

# done - now Country2 is the country assignment for all studies, including multi-country studies
AGIMPACTS_MAIN <- AGIMPACTS_MAIN %>% 
  mutate(Country2 = case_when(
    is.na(Country1) ~ Country, # 8858 # original single-country study assignment
    !is.na(Country1) ~ Country1 # 133 # single country assigned to multi-country study 
  ))

AGIMPACTS_MAIN <- AGIMPACTS_MAIN %>% 
  dplyr::select(!Country1)


# make photosynthetic_pathway two dummy variables for C3 and C4
AGIMPACTS_MAIN <- AGIMPACTS_MAIN %>% 
  mutate(C = 1) %>% 
  pivot_wider(names_from = Photosynthetic_pathway,
              values_from = C,
              values_fill = 0)

# make the Co2 function variables

AGIMPACTS_MAIN <- AGIMPACTS_MAIN %>% 
  mutate(f_CO2 = case_when(
    C3 == 1 ~ CO2.Change/(CO2.Change + 100),
    C4 == 1 ~ CO2.Change/(CO2.Change + 50))
  )

# define the quadratic variable
AGIMPACTS_MAIN <- AGIMPACTS_MAIN %>% 
  mutate(Temp.Change.Squared = Temp.Change^2)

# dummy variable for adaptation (1,0) 

AGIMPACTS_MAIN <- AGIMPACTS_MAIN %>% 
  mutate(adapt_dummy = as.factor(if_else(Adaptation %in% c("No", "NA", "No "), 0, 1)))

AGIMPACTS_MAIN %>% readr::write_csv(here("processed", "AGIMPACTS_MAIN.csv"))

saveRDS(AGIMPACTS_MAIN, here("processed", "AGIMPACTS_MAIN.Rdata")) 

# summary stats -----------------------------------------------------------


AGIMPACTS_MAIN %>% 
  group_by(Country, Crop) %>% 
  summarise(n = n()) %>% 
  print(n = Inf) #233 multi-country and crop combos

AGIMPACTS_MAIN %>% 
  group_by(Country2, Crop) %>% 
  summarise(n = n()) %>% 
  print(n = Inf) # 188 single-country and crop combos 

AGIMPACTS_MAIN %>% 
  group_by(Country2) %>% 
  summarise(n=n()) %>% 
  adorn_totals("row") # 8846 values - can see distribution of point estimates by country post-assignment, which countries are represented in the meta-analysis

AGIMPACTS_MAIN %>% 
  group_by(Country2) %>% 
  summarise(n=n()) %>% 
  mutate(point_total = sum(n)) %>% 
  mutate(per = n/point_total) %>% 
  print(n=Inf)

#ggplot(aes(x = per)) + geom_histogram(stat = "bin", binwidth = 0.001)
ggplot(aes(x = per)) + geom_bar(width = 0.001) +
  stat_bin(binwidth = 0.001, geom = "text", aes(label=..count..))

AGIMPACTS_MAIN %>% 
  group_by(Country2) %>% 
  summarise(n=n()) %>% 
  mutate(point_total = sum(n)) %>% 
  mutate(per = n/point_total) %>% 
  ggplot(aes(x = reorder(Country2, -per), y = per)) + geom_bar(stat="identity")+
  ggtitle("Distribution of countries in agimpacts data (main crops), n = 8846") +
  xlab("Country") + ylab("Percent") +
  theme(axis.text.x = element_text(size = 6))


# single country v multi country studies and estimates --------------------

AGIMPACTS_FINAL_MAIN %>% 
  group_by(Country) %>% 
  summarise(unique_countries = n_distinct(Country)) %>%
  mutate(c_occur = stringr::str_count(Country, '[A-Z]')/2) %>% 
  summarise(single = sum(c_occur == 1),
            multiple = sum(c_occur != 1)) # 116 unique countries, 31 multiple-country groups represented

# how about in terms of estimates?
AGIMPACTS_FINAL_MAIN %>% 
  #group_by(Country) %>% 
  #summarise(unique_countries = n_distinct(Country)) %>%
  mutate(c_occur = stringr::str_count(Country, '[A-Z]')/2) %>% 
  summarise(single = sum(c_occur == 1), # 8594
            multiple = sum(c_occur != 1)) # 250

# how about inside references?

AGIMPACTS_FINAL_MAIN %>% 
  group_by(Reference) %>% 
  summarise(unique_countries = n_distinct(Country)) %>%
  ungroup() %>% 
  summarise(single = sum(unique_countries == 1), # 58 studies had only 1 country
            multiple = sum(unique_countries > 1)) # 16 studies had multiple countries


AGIMPACTS_MAIN <- readRDS(here("processed", "AGIMPACTS_MAIN.RData"))
AGIMPACTS_MAIN %>% 
  group_by(Reference) %>% 
  summarise(unique_countries = n_distinct(Country)) %>%
  ungroup() %>% 
  summarise(single = sum(unique_countries == 1), # 58 studies had only 1 country
            multiple = sum(unique_countries > 1)) # 16 studies had multiple countries

