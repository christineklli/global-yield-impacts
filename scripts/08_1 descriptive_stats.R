# it will be useful to compare temperature and precipitation baselines before and after imputation
# understand the coverage of 'drought' conditions
# note that meteorological drought, agricultural drought and hydrological drought are all defined differently
# understand these conditions

# READ IN INCOMPLETE, UNIMPUTED DATA --------------------------------------

AGIMPACTS_bs_tp <- readRDS(here("processed", "AGIMPACTS_bs_tp.Rdata")) 


# descriptive stats -------------------------------------------------------

ggplot(data=AGIMPACTS_bs_tp_yields) +
  geom_point(aes(x=Temp.Change,y=Yield.Change, color=crop_pooled))

# baseline temperature x precipitation ------------------------------------

# plots

g <- ggplot(AGIMPACTS_bs_tp, 
            aes(Baseline_tmp_weighted, Baseline_pre_weighted)) +
  geom_point(alpha=0.5) +
  theme_bw() +
  labs(x="Baseline growing season temperature (°C)",
       y="Baseline growing season precipitation (mm/month)")

h <- ggMarginal(g, type = "histogram",
                xparams=list(fill="#CC0000", color="white"),
                yparams=list(fill="#000066", color="white"))

k <- ggMarginal(g)

# save  plot
png(file = file.path(here("results", "figures", "bs_tmp_precip.png")))
plot(h)
dev.off()

png(file = file.path(here("results", "figures", "bs_tmp_precip_density.png")))
plot(k)
dev.off()

# coloured by crop

i <- ggplot(AGIMPACTS_bs_tp, 
            aes(Baseline_tmp_weighted, Baseline_pre_weighted, colour = crop_pooled)) +
  geom_point(alpha=0.5) +
  theme_bw() +
  labs(x="Baseline growing season temperature (°C)",
       y="Baseline growing season precipitation (mm/month)",
       color="Crop")

j <- ggMarginal(i, 
                groupColour = TRUE,
                groupFill = TRUE)

# save  plot
png(file = file.path(here("results", "figures", "bs_tmp_precip_crop.png")))
plot(j)
dev.off()


# Removed 1020 rows containing missing values (geom_point). 
# missing values driven by missing common baseline period info

# break out data by crop for facet grid

png(file = file.path(here("results", "figures", "bs_tp_facet_crop.png")))

ggplot(AGIMPACTS_bs_tp, aes(Baseline_tmp_weighted, Baseline_pre_weighted)) +
  geom_point(alpha=0.5) +
  theme_bw() +
  labs(x="Baseline growing season temperature (°C)",
       y="Baseline growing season precipitation (mm/month)") +
  facet_grid(vars(crop_pooled))

dev.off()

# READ IN IMPUTED DATA ------------------------------------------------------------

crops <- c("Maize", "Rice", "Soybean", "Wheat")

crop_imputed_data_restricted <- lapply(1:4, function(j){
  lapply(1:5, function(i,j){
    readRDS(here("processed", paste0("crop_data_", crops[[j]], "_", i, ".Rdata")))
  }, j)
})


# baseline precip x temp among imputed datasets ---------------------------

# represent all 5 imputed datasets by crop - facet grid crop x imputed dataset

# enable facet grid by rbindlist by crop and then by imputation index

crop_imputed_m <- lapply(1:4, function(i){
  crop_imputed_data_restricted[[i]] %>% 
  rbindlist(idcol="m")
})

crop_imputed_dt <- rbindlist(crop_imputed_m)

n_annotations <- crop_imputed_dt %>% 
  add_count(.,crop_pooled,m) %>% 
  group_by(crop_pooled,m) %>% 
  summarise(N=n())
  #summarise(N = paste0("N = ", n())) # the geom_text gets truncated
  #mutate(n=ifelse(row_number(n)!=1,NA,n))
  
n <- ggplot(crop_imputed_dt, 
       aes(Baseline_tmp_weighted, Baseline_pre_weighted,
           color = crop_pooled)) +
  geom_point(alpha=0.5) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x="Baseline growing season temperature (°C)",
       y="Baseline growing season precipitation (mm/month)") +
  facet_grid(vars(crop_pooled), vars(m))

# get x and y ranges from plot object

x_range <- layer_scales(n)$x$range$range
y_range <- layer_scales(n)$y$range$range

png(file = file.path(here("results", "figures", "imputed_bs_tp_facet_m_crop.png")))

n + geom_text(data = n_annotations,
              x = x_range[2] - 0.5 *(diff(x_range)),
              y = y_range[2] - 0.1 * (diff(y_range)),
              aes(label = N),
              # size = 5,
              hjust = 1)

dev.off()

# present m coloured but faceted only by crop


png(file = file.path(here("results", "figures", "imputed_bs_tp_facet_crop.png")))

ggplot(crop_imputed_dt, 
       aes(Baseline_tmp_weighted, Baseline_pre_weighted,
           color = as.factor(m))) +
  geom_point(alpha=0.5) +
  theme_bw() +
  labs(x="Baseline growing season temperature (°C)",
       y="Baseline growing season precipitation (mm/month)",
       color="m") +
  facet_grid(vars(crop_pooled))

dev.off()



# yield change and temperature change by crop ---------------------------

ggplot(crop_imputed_dt, 
       aes(Temp.Change, Yield.Change,
           color = crop_pooled)) +
  geom_point(alpha=0.5) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x="Temperature change (°C)",
       y="Yield Change (%)") +
  facet_grid(vars(crop_pooled), vars(m))

# hard to read difference between maize and yield

# check sample sizes --------------------------------------------------

# sample sizes of each m; crop dataset

str(crop_imputed_data_restricted[[1]], max.level=1)

str(crop_imputed_data_restricted[[2]], max.level=1)

str(crop_imputed_data_restricted[[3]], max.level=1)

str(crop_imputed_data_restricted[[4]], max.level=1)

# can we save these sample sizes of [m; crop]  to a table
# and then put them into the top left corners of each facet panel?


# baseline period - time dynamics -----------------------------------------

# CGIAR baseline period spans - are there feedback loops?
# ie temp affects water supply affects yield change - how dynamic are yield responses to changes in water supply?
# i don't think this is something that baseline period length can tell us
# it's more to do with individual study design - i.e. if simulation then maybe... maybe not
# and would probably only be possible if IAM/GCM/impact model coupled with global hydrological model

