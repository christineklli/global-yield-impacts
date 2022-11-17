

# Soybean share of country-oilseed production -----------------------------

# load all production files in a stack
oilseed_prod_list <- list.files(here("data", "Monfreda data", "Oilseeds"), 
                                     pattern = "^.*\\.(tif|TIF|Tif)$", all.files = TRUE, full.names = FALSE)

# extract country sum of production (df)

oilseed_prod_stack <- stack(here("data", "Monfreda data", "Oilseeds", oilseed_prod_list))

oilseed_country_sum <- exact_extract(oilseed_prod_stack, worldmap_clean, 'sum')

oilseed_country_sum <- oilseed_country_sum %>% 
  mutate(country_name = worldmap_clean@data$NAME,
         country_code = worldmap_clean@data$ISO_A2)

# calculate soybean share of total oilseed production per country (df) - column pct of rowsum

soybean_oilseed_country_share <- oilseed_country_sum %>% 
  summarise(soybean_pct = sum.soybean_Production/rowSums(oilseed_country_sum[,!names(oilseed_country_sum) %in% c("country_name", "country_code")])) %>% 
  mutate(country_name = worldmap_clean@data$NAME,
         country_code = worldmap_clean@data$ISO_A2) %>% 
  relocate(country_name, country_code, soybean_pct)


soybean_oilseed_country_share %>% write.table("soybean_production_share.txt")

# also can convert to xlsx but not csv as will return scientific notation


# Soybean share of GTAP oilseed production --------------------------------

oilseed_country_sum %>% 
  left_join(mapping, by = c("country_name" = "Country")) %>% 
  group_by(id141,n141,c141) %>% 
  summarise(across("sum.castor_Production":"sum.tung_Production", ~sum(., na.rm=TRUE))) %>% 
  ungroup() %>% 
  mutate(soybean_pct = sum.soybean_Production/rowSums(.[4:21])) %>% 
  dplyr::select(!c("sum.castor_Production":"sum.tung_Production")) %>% 
  write.table(here("results", "text files", "soybean_production_share_gtap.txt"))

