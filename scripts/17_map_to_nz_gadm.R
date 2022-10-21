
# READ GADM NZ BOUNDARIES -------------------------------------------------

gadm_lvl_1 <- st_read(here("data", "GADM data", "gadm36_NZL_1.shp"))

ncol(gadm_lvl_1) # 10 fields, 11 cols
names(gadm_lvl_1) # colnames/field names
gadm_lvl_1$NAME_1

# keep only the 15 regions that are not islands
gadm_subset_16 <- gadm_lvl_1[-c(4,10,13),]

ggplot() +
  geom_sf(data = gadm_subset_16, aes(col = factor(NAME_1)), size = 1) +
  labs(color = 'Region (16)')

ggsave(here("results", "figures", "nz_16_gadm.png"))


# READ IN GRIDDED PREDICTIONS ---------------------------------------------


crops <- c("Maize", "Rice", "Soybean", "Wheat")

# parsing failures here as well - 
pooled_predictions <- lapply(1:4, function(i){
  
  readr::read_csv(here("processed", paste0("pooled_predictions_", crops[[i]], ".csv")),
                  col_types = readr::cols(.default = "d"))
  
})


# CALCULATE NONWEIGHTED PREDICTIONS FOR NZ REGIONS --------------------------

pooled_nz_savg <- function(i){
  
raster_0dc <- raster(ncol = 720, nrow = 360) 
values(raster_0dc) <-  pooled_predictions[[i]][["mean_y_0"]]

raster_1dc <- raster(ncol = 720, nrow = 360) 
values(raster_1dc) <- pooled_predictions[[i]][["mean_y_1"]]

raster_2dc <- raster(ncol = 720, nrow = 360) 
values(raster_2dc) <- pooled_predictions[[i]][["mean_y_2"]]

raster_3dc <- raster(ncol = 720, nrow = 360) 
values(raster_3dc) <- pooled_predictions[[i]][["mean_y_3"]]

raster_4dc <- raster(ncol = 720, nrow = 360) 
values(raster_4dc) <- pooled_predictions[[i]][["mean_y_4"]]

raster_5dc <- raster(ncol = 720, nrow = 360) 
values(raster_5dc) <- pooled_predictions[[i]][["mean_y_5"]]

raster_alldc_stack <- stack(raster_0dc, raster_1dc, raster_2dc, raster_3dc, raster_4dc, raster_5dc)


predicted_yield_all_dc_nz <- exactextractr::exact_extract(raster_alldc_stack, gadm_subset_16, 'mean')

predicted_yield_all_dc_nz <- predicted_yield_all_dc_nz %>% rename("0 degrees" = 1, "1 degree" = 2, "2 degrees" = 3,
                                                                            "3 degrees" = 4, "4 degrees" = 5, "5 degrees" = 6)

gadm_subset_16 %>% 
  as.data.frame() %>% 
  dplyr::select(GID_1, NAME_1) %>% 
  cbind(predicted_yield_all_dc_nz) %>% 
  readr::write_csv(here("results", "tables", paste0("predicted_nz_nonweighted", "_", crops[[i]], ".csv")))


}


pooled_nz_predictions_savg <- lapply(1:4, pooled_nz_savg)


# READ IN CROP PRODUCTION STACK DATA --------------------------------------

# Go to script 03 and run lines 41-48, or here:

crop_production_files <- c("maize_Production.tif",
                           "rice_Production.tif",
                           "soybean_Production.tif",
                           "wheat_Production.tif")


crop_production_files2 <- here("data", "Monfreda data", crop_production_files)


# Go to script 05 and run line 126-129, or here:

crop_production_rasters <- lapply(crop_production_files2, raster)

# stack em
crop_production_stack <- stack(crop_production_rasters)


# CALCULATE WEIGHTED PREDICTIONS FOR NZ REGIONS ---------------------------

crop_production_csv <- readr::read_csv(here("processed", "crop_production_list.csv"))

crop_production_csv %>% filter(name == "New Zealand") # 172,429 T of maize, 317,144 T of wheat

pooled_nz_wavg <- function(i){
  
  raster_0dc <- raster(ncol = 720, nrow = 360) 
  values(raster_0dc) <-  pooled_predictions[[i]][["mean_y_0"]]
  
  raster_1dc <- raster(ncol = 720, nrow = 360) 
  values(raster_1dc) <- pooled_predictions[[i]][["mean_y_1"]]
  
  raster_2dc <- raster(ncol = 720, nrow = 360) 
  values(raster_2dc) <- pooled_predictions[[i]][["mean_y_2"]]
  
  raster_3dc <- raster(ncol = 720, nrow = 360) 
  values(raster_3dc) <- pooled_predictions[[i]][["mean_y_3"]]
  
  raster_4dc <- raster(ncol = 720, nrow = 360) 
  values(raster_4dc) <- pooled_predictions[[i]][["mean_y_4"]]
  
  raster_5dc <- raster(ncol = 720, nrow = 360) 
  values(raster_5dc) <- pooled_predictions[[i]][["mean_y_5"]]
  
  raster_alldc_stack <- stack(raster_0dc, raster_1dc, raster_2dc, raster_3dc, raster_4dc, raster_5dc)
  
  
  predicted_yield_all_dc_nz <- exactextractr::exact_extract(raster_alldc_stack, gadm_subset_16, 'weighted_mean', weights = crop_production_stack[[i]])
  
  predicted_yield_all_dc_nz <- predicted_yield_all_dc_nz %>% rename("0 degrees" = 1, "1 degree" = 2, "2 degrees" = 3,
                                                                    "3 degrees" = 4, "4 degrees" = 5, "5 degrees" = 6)
  
  gadm_subset_16 %>% 
    as.data.frame() %>% 
    dplyr::select(GID_1, NAME_1) %>% 
    cbind(predicted_yield_all_dc_nz) %>% 
    readr::write_csv(here("results", "tables", paste0("predicted_nz_weighted", "_", crops[[i]], ".csv")))
  
  
}


pooled_nz_predictions_wavg <- lapply(1:4, pooled_nz_wavg)


# PLOT WEIGHTED REGIONAL PREDICTIONS --------------------------------------


nz_crops <- c("Maize", "Wheat")

r <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, crs=4326, res=0.5)

r_nz <- raster(xmn=165, xmx=180, ymn=-50, ymx=-30, crs=4326, res=0.05)

plot_nz_wavg <- function(i){
  
  raster_0dc <- raster(ncol = 720, nrow = 360) 
  values(raster_0dc) <-  pooled_predictions[[i]][["mean_y_0"]]
  
  raster_1dc <- raster(ncol = 720, nrow = 360) 
  values(raster_1dc) <- pooled_predictions[[i]][["mean_y_1"]]
  
  raster_2dc <- raster(ncol = 720, nrow = 360) 
  values(raster_2dc) <- pooled_predictions[[i]][["mean_y_2"]]
  
  raster_3dc <- raster(ncol = 720, nrow = 360) 
  values(raster_3dc) <- pooled_predictions[[i]][["mean_y_3"]]
  
  raster_4dc <- raster(ncol = 720, nrow = 360) 
  values(raster_4dc) <- pooled_predictions[[i]][["mean_y_4"]]
  
  raster_5dc <- raster(ncol = 720, nrow = 360) 
  values(raster_5dc) <- pooled_predictions[[i]][["mean_y_5"]]
  
  raster_alldc_stack <- stack(raster_0dc, raster_1dc, raster_2dc, raster_3dc, raster_4dc, raster_5dc)
  
  
  predicted_yield_all_dc_nz <- exactextractr::exact_extract(raster_alldc_stack, gadm_subset_16, 'weighted_mean', weights = crop_production_stack[[i]])
  
  predicted_yield_all_dc_nz <- predicted_yield_all_dc_nz %>% rename("0 degrees" = 1, "1 degree" = 2, "2 degrees" = 3,
                                                                    "3 degrees" = 4, "4 degrees" = 5, "5 degrees" = 6)
  
  gadm_subset_16_copy <- gadm_subset_16
  gadm_subset_16_copy <- gadm_subset_16_copy %>% cbind(predicted_yield_all_dc_nz)


predicted_yield_0dc <- rasterize(gadm_subset_16_copy, r_nz, field = "X0.degrees")
predicted_yield_1dc <- rasterize(gadm_subset_16_copy, r_nz, field = "X1.degree")
predicted_yield_2dc <- rasterize(gadm_subset_16_copy, r_nz, field = "X2.degrees")
predicted_yield_3dc <- rasterize(gadm_subset_16_copy, r_nz, field = "X3.degrees")
predicted_yield_4dc <- rasterize(gadm_subset_16_copy, r_nz, field = "X4.degrees")
predicted_yield_5dc <- rasterize(gadm_subset_16_copy, r_nz, field = "X5.degrees")

predicted_yield_nz_alldc_raster <- stack(predicted_yield_0dc,
                                              predicted_yield_1dc,
                                              predicted_yield_2dc,
                                              predicted_yield_3dc,
                                              predicted_yield_4dc,
                                              predicted_yield_5dc)

rasterVis::levelplot(predicted_yield_nz_alldc_raster, 
                     col.regions = rev(terrain.colors(10000)),
                     at = seq(-20,5),
                     names.attr = c("0 degrees warming",
                                    "1 degree warming",
                                    "2 degrees warming",
                                    "3 degrees warming",
                                    "4 degrees warming",
                                    "5 degrees warming"))
}

nz_plots <- lapply(c(1,4), plot_nz_wavg)

# save plots

plot_nz_16_predictions <- function(i){
  
  mypath <- file.path(here("results", "figures"),
                      paste0("predicted_nz_wavg_", nz_crops[[i]], ".png"))
  
  png(file = mypath)
  
  plot(nz_plots[[i]])
  
  dev.off()
  
}

lapply(c(1:2), plot_nz_16_predictions)

# understanding these results - worse for maize than wheat
# in southern South Island, yields will improve for wheat, as it gets warmer 
# maybe because in NZ the baseline temperature is below optimal temperature for wheat 
# but will worsen for maize, not sure why, maybe because baseline temperature already exceeds optimal temperature for maize?
# but impossible for baseline temp be both below optimal wheat temp and above maize temp 

# AGGREGATE TO 8 REGIONS --------------------------------------------------

# Would still need to coalesce with nonweighted result for West Coast

# But eg for Northern South Island, is it a better approach to coalesce and then aggregate
# or to aggregate and then coalesce? 

# first create table of production by 16 regions, by exactextractr on crop_production_stack[[i]] on gadm_subset_16

crop_production_nz <- lapply(1:4, function(i) {
  
  y_tot <- exactextractr::exact_extract(crop_production_stack[[i]], gadm_subset_16, 'sum')
  
  y_tot %>% 
    as.data.frame() %>% 
    cbind(GID_1 = gadm_subset_16$GID_1,
          NAME_1 = gadm_subset_16$NAME_1) %>% 
    relocate(GID_1, NAME_1) %>% 
    rename(y_tot = 3)
    
  })

# create correspondence of 16-8 regions

nz_region_corr <- tribble(
  ~region_16, ~region_8,
  "Auckland", "nzl_nni",
  "Bay of Plenty", "nzl_upcni",
  "Canterbury", "nzl_esi",
  "Gisborne", "nzl_eni",
  "Hawke's Bay", "nzl_eni",
  "Manawatu-Wanganui", "nzl_swni",
  "Marlborough", "nzl_nsi",
  "Northland", "nzl_nni",
  "Otago", "nzl_ssi",
  "Southland", "nzl_ssi",
  "Taranaki", "nzl_swni",
  "Tasman", "nzl_nsi",
  "Nelson", "nzl_nsi",
  "Waikato", "nzl_upcni",
  "Wellington", "nzl_swni",
  "West Coast", "nzl_wsi"
) %>% 
  mutate(region_8 = as.factor(region_8),
         region_16 = as.factor(region_16))

# check only 8 regions
n_distinct(nz_region_corr$region_8)

# then do aggregation of weighted results

nz_results_16 <- lapply(1:4, function(i) {
  
  pooled_nz_predictions_wavg[[i]] %>% 
  left_join(nz_region_corr, by = c("NAME_1" = "region_16")) %>%
  left_join(crop_production_nz[[i]][,-1], by = "NAME_1") # remove GID_1 duplicate column
  
})

nz_results_8 <- lapply(1:4, function(i) {
  
  nz_results_16[[i]] %>% 
    group_by(region_8) %>% 
    summarise(across(`0 degrees`:`5 degrees`, ~weighted.mean(., w = y_tot, na.rm=TRUE)))
  
})  

# then coalesce non-weighted results for west coast/nzl_wsi

# first aggregate nonweighted results to 8 regions

nz_nonweighted_results_8 <- lapply(1:4, function(i) {
  
  pooled_nz_predictions_savg[[i]] %>%
    left_join(nz_region_corr, by = c("NAME_1" = "region_16")) %>% 
    group_by(region_8) %>% 
    summarise(across(`0 degrees`:`5 degrees`, ~mean(., na.rm=TRUE)))
  
})  


predicted_nz_coalesced <- lapply(c(1,4), function(i) { # note that predicted_yield_country_savg doesn't have the first column X1 that's in the wavg results
  df <- data.frame(region_8 = nz_results_8[[i]][1],
             "0 degrees" = coalesce(nz_results_8[[i]][2], nz_nonweighted_results_8[[i]][2]),
             "1 degree" = coalesce(nz_results_8[[i]][3], nz_nonweighted_results_8[[i]][3]),
             "2 degrees" = coalesce(nz_results_8[[i]][4], nz_nonweighted_results_8[[i]][4]),
             "3 degrees" = coalesce(nz_results_8[[i]][5], nz_nonweighted_results_8[[i]][5]),
             "4 degrees" = coalesce(nz_results_8[[i]][6], nz_nonweighted_results_8[[i]][6]),
             "5 degrees" = coalesce(nz_results_8[[i]][7], nz_nonweighted_results_8[[i]][7])) %>% 
    dplyr::mutate("imputed" = ifelse(!is.na(nz_results_8[[i]][2]), 0, # must record list of countries coalesced from savg results
                            ifelse(is.na(nz_results_8[[i]][2]) & !is.na(nz_nonweighted_results_8[[i]][2]), 1, 0))) %>% 
    rename("0 degrees" = 2,
           "1 degree" = 3,
           "2 degrees" = 4,
           "3 degrees" = 5,
           "4 degrees" = 6,
           "5 degrees" = 7,
           "imputed" = 8)
    
    names(df)[8] <- "imputed"
    
    df
}
)

# write to csv


lapply(c(1:2), function(i) {
  
  readr::write_csv(predicted_nz_coalesced[[i]], 
                   here("results", "tables", paste0("predicted_nz_coalesced_", nz_crops[[i]], ".csv")))
  
}) 


# CALCULATE SAVG FOR JUST RICE - 8 REGIONS --------------------------------


nz_results_16_rice_savg <- lapply(2, function(i) {
  
  pooled_nz_predictions_savg[[i]] %>% 
    left_join(nz_region_corr, by = c("NAME_1" = "region_16")) 
  
})

nz_results_8_rice_savg <- lapply(1, function(i) {
  
  nz_results_16_rice_savg[[i]] %>% 
    group_by(region_8) %>% 
    summarise(across(`0 degrees`:`5 degrees`, ~mean(., na.rm=TRUE)))
  
})  

nz_results_8_rice_savg[[1]] %>% readr::write_csv(here("results", "tables", "predicted_nz_rice_savg.csv"))
  
  
# CALCULATE CONFIDENCE INTERVALS ------------------------------------------


