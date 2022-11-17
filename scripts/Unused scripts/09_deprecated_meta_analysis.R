# decide how much of scripts 9 and 10 to re-run (meta-analysis, model cross-validation)
# prioritise descriptive statistics first - baseline temperature and precipitation ranges


# Influence Analysis https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/heterogeneity.html#influence-analysis
# first run gen.m

#########

# dependency (correlation) between effect sizes can artificially reduce hetereogeneity
# leading to false-positive results (unit-of-analysis error)
# explicitly accounting for known dependent structures can help us estimate true mean pooled effect sizes
# by accounting for the different sources of error and sampling of studies, countries etc.

# sources of within-study dependency (level 3)
# non-random sample of countries chosen
# dependency between countries chosen (i.e. within a region) 
# therefore nest countries within studies (level 2)
# sources of within-study-within-country dependency
# there are various observed sources of nested dependency within-study-within-country
# same GCM or impact models or study design assumptions chosen by study author
# similar temperature scenarios chosen
# other observed confounding variables e.g. precipitation, adaptation, etc. 
# therefore it is suitable to have at least three levels to account for these known nested dependencies

# rewrite the application of random effect model meta-analysis in plain language
# explaining the sources of dependency in the data 
# I'd like to understand the econometrics/maths more behind how multi-level RE actually manages to reduce error
# in order to estimate true mean pooled effect size


# READ IN DATA ------------------------------------------------------------

crops <- c("Maize", "Rice", "Soybean", "Wheat")

crop_imputed_data_restricted <- lapply(1:4, function(j){
  lapply(1:5, function(i,j){
    readRDS(here("processed", paste0("crop_data_", crops[[j]], "_", i, ".Rdata")))
  }, j)
})


crop_imputed_m <- lapply(1:4, function(i){
  crop_imputed_data_restricted[[i]] %>% 
    rbindlist(idcol="m")
})

crop_imputed_dt <- rbindlist(crop_imputed_m)

AGIMPACTS_bs_tp <- readRDS(here("processed", "AGIMPACTS_bs_tp.RData"))

# between-study and between-country heterogeneity -------------------------

# meta-analysis results - variance distribution across levels showed that
# variance in effect sizes fro wheat was mostly explained by between-country heterogeneity
# rather than between-study heterogenetiy
# hypothesise that this is because studies focussing on wheat mostly focus on one country only
# test this by grouping by crop and study and summarising the number of countries per crop-study
# studies may cover multiple crops too - we are trying to look at whether there are marked differences
# between wheat and other crops that are revealed

crop_imputed_dt %>% # 555 rows
  group_by(m,crop_pooled,Reference_fact) %>% 
  summarise(n=uniqueN(Country2_fact))


AGIMPACTS_bs_tp %>% # 112 rows, more manageable, imputed datasets should have similar patterns
  group_by(crop_pooled,Reference_fact) %>% 
  summarise(n=uniqueN(Country2_fact)) %>% 
  print(n=Inf)

# all four crops have studies with only 1 country and at least one multi-country study
# this seems to cast doubt on the hypothesis
# so why else could wheat variance be mostly explained by between-country heterogeneity?
# could it be bc between-study heterogeneity is actually v low? studies on wheat show similar results even w diff study designs?
# unlikely bc between study het is meant to capture statistical het
# could be because wheat studies just contain higher within-study heterogeneity
# we should explore this further - investigate which studies are driving this variation 
# can we do a multi-level forest plot by crop? there may be a few wheat studies that show a very large range
# related to sub-groups


# META-ANALYSIS WITH 3 LEVELS -----------------------------------------------------------

# only for first imputed dataset

meta_data_restricted <- function(i ,j){
  
  crop_imputed_data_restricted[[j]][[i]] %>% 
    mutate(es.id = row_number()) %>% 
    group_by(Reference_fact, Country2_fact)
  #%>% # note that we are defining only three levels here; point estimate, country and study. there may be more levels, e.g. model, CO2, Precip?
  #  mutate(n = n(),
  #          mean = mean(Yield.Change, na.rm=TRUE),
  #          se = sd(Yield.Change)/sqrt(n),
  #          weights = 1/(se^2)) %>% 
  #   filter(!is.infinite(weights))
  
}

meta_data_restricted_crops <- lapply(1:4, function(j){lapply(1:5, meta_data_restricted, j)})

# Calculate standard errors, Pearson's correlation coefficient and Fisher's z
meta_data_restricted <- function(i ,j){
  
  crop_imputed_data_restricted[[j]][[i]] %>% 
    mutate(es.id = row_number()) %>% 
    group_by(Reference_fact, Country2_fact) %>% # note that we are defining only three levels here; point estimate, country and study. there may be more levels, e.g. model, CO2, Precip?
    mutate(n = n(),
           r = cor(Temp.Change, Yield.Change, use="complete.obs"),
           z = 0.5*log(1+r)/(1-r), # Fisher's z transformation to reduce bias from range restriction
           se.z = 1/sqrt(n-3)) %>% # pearson's se_r = se = (1-r^2)/sqrt(n-2))  
    filter(!is.na(r) & !is.na(z) & !is.na(se.z) & !is.infinite(se.z)) 
  
}

meta_data_crops_restricted <- lapply(1:4, function(j){lapply(1:5, meta_data_restricted, j)})

# Construct multi-level meta analysis model based on 
# effect sizes - correlation btwn yield and temperature change
# may want to repeat for yield and precipitation change 
# and temperature change x precipitation change?
meta_l3_model_restricted <- function(i, j){
  
  model <- metafor::rma.mv(yi = z, 
                           V = se.z^2,
                           slab = Reference_fact,
                           data = meta_data_crops_restricted[[i]][[j]], # DATASETS = i, CROPS = j (unlike usual)
                           random = ~ 1 | Reference_fact/Country2_fact, 
                           test = "t",
                           method = "REML")
  
}


meta_l3_models_crops_restricted <- lapply(1:5, function(j){lapply(1:4, meta_l3_model_restricted, j)}) # note that datasets are indexed first, followed by crop

# save models - these took almost all day to run!
  lapply(1:4, function(j){
    lapply(1:5, function(i,j){
      meta_l3_models_crops_restricted[[i]][[j]] %>% 
      saveRDS(here("processed", paste0("meta_l3_models", crops[[j]], "_", i, ".Rdata")))
    }, j)
  })
  
# note that the list indexing is actually correct this time around, crop[[i]]m[[j]] 
sink(here("results", "text files", "meta_m1_full_model_restricted.txt"))
meta_l3_models_crops_restricted
sink()

meta_l3_models_crops_restricted <- lapply(1:4, function(j){
  lapply(1:5, function(i,j){
         readRDS(here("processed", paste0("meta_l3_models", crops[[j]], "_", i, ".Rdata")))
  }, j)
})


# convert estimated Fisher's z back to regular correlation

esc::convert_z2r( -0.1577) # -0.1676036; this is not a significant correlation

# calculate multilevel version of I^2 that shows precise amount of heterogeneity captured by each level in our model

meta_varcomp_restricted <- lapply(1:4, function(j){lapply(1:5, function(i,j){dmetar::var.comp(meta_l3_models_crops_restricted[[j]][[i]])}, j)})
# this is CROPS first and then DATASETS

sink(here("results", "text files", "meta_var_comp_restricted.txt"))
meta_varcomp_restricted
sink()

# plot(meta_m1_var.comp_restricted[[1]][[1]])

# only report results for m1 of each crop
plots_m1_var_comp_restricted <- lapply(1:4, function(i){plot(meta_varcomp_restricted[[i]][[1]])})

cowplot::plot_grid(plots_m1_var_comp_restricted[[1]],
                   plots_m1_var_comp_restricted[[2]],
                   plots_m1_var_comp_restricted[[3]],
                   plots_m1_var_comp_restricted[[4]],  nrow = 2, 
                   labels=c("Maize", "Rice", "Soy", "Wheat"),  
                   label_size=10, 
                   hjust = -0.5)


# META-ANALYSIS WITH 2 LEVELS ---------------------------------------------


meta_l3.removed_restricted <- function(i, j){
  
  model <- metafor::rma.mv(yi = z,
                           V = se.z^2,
                           slab = Reference_fact,
                           data = meta_data_crops_restricted[[i]][[j]],
                           random = ~ 1 | Reference_fact/Country2_fact,
                           test = "t",
                           method = "REML",
                           sigma2 = c(0, NA))} # this removes last level

meta_l3.removed_crops_restricted <- lapply(1:5, function(j){lapply(1:4, meta_l3.removed_restricted, j)})

sink(here("results", "text files", "meta_l3removed_model_restricted.txt"))
meta_m1_l3.removed_crops
sink()


# COMPARE META-ANALYSIS WITH 2 LEVELS AND 3 LEVELS ----------------------

anova_test <- function(i,j){
  
  anova(meta_l3_models_crops_restricted[[i]][[j]],
        meta_l3.removed_crops_restricted[[i]][[j]])
  
}

anova_test_l3_crops_restricted <- lapply(1:4, function(j){lapply(1:5, anova_test_restricted, j)})

sink(here("results", "text files", "meta_l3removed_anova_restricted.txt"))
anova_test_l3_crops_restricted
sink()



# multi-level forest plots ------------------------------------------------

# need to do this separately for each crop-specific dataset
# should not have crop as highest level of clustering as we aren't trying to estimate a pooled effect size across crops

# in Harrer et al. forest plots are run on metagen() model for simple data,  metafor() is to nested data 
# https://stackoverflow.com/questions/71322412/how-to-plot-multi-level-meta-analysis-by-study-in-contrast-to-the-subgroup

# therefore use metafor() model object

# wheat studies contain higher within-study heterogeneity
# we should explore this further - investigate which studies are driving this variation 
# multi-level forest plot by crop - there may be a few wheat studies that show a very large range
# related to sub-groups

# maize, m1 model

# https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggcoefstats.html
# see 'parametric random-effects meta-analysis' section - terms represent study random subjects
# distribution of estimate (correlation)?
# however will this still print row by row?
# https://stackoverflow.com/questions/72509941/how-to-make-a-forest-plot-for-a-mixed-model
# dot and whisker or caterpillar plots for mixed models

# https://www.metafor-project.org/doku.php/tips:forest_plot_with_aggregated_values

# https://stackoverflow.com/questions/71322412/how-to-plot-multi-level-meta-analysis-by-study-in-contrast-to-the-subgroup

dat <- meta_data_crops_restricted[[1]][[1]] # this needs to be converted to escalc first
dat <- escalc(measure="ZCOR", yi=z, vi=se.z, data=dat)
res <- rma.mv(yi, vi, random = ~ 1 | Reference_fact/Country2_fact, data=dat)
res
agg <- aggregate(dat, cluster=dat$Reference_fact, V=vcov(res, type="obs")) # this DOES work
agg
res_agg <- rma(yi, vi, method="EE", data=agg)
with(agg, forest(yi, vi, slab=paste("Study", Reference_fact))) # from the stackexchange example

saveRDS(res, here("processed","forest_maize_m1.Rdata"))


readRDS(here("processed","forest_maize_m1.Rdata"))

# view agg - should be able to merge reference names by number
# unfortunately we dropped the reference string variable before imputation
# probably easiest at this stage to create a concordance table
# of reference (chr) and reference (fact) from original dataset

ref_concordance <- AGIMPACTS_bs_tp %>% 
  dplyr::select(Reference, Reference_fact, Reference_int) %>% 
  distinct()


agg_ref <- agg %>% 
  left_join(dplyr::select(ref_concordance, Reference, Reference_int), 
            by = "Reference_int")

png(here("results", "figures", "maize_m1_forestplot.png"))
with(agg_ref, forest(yi, vi, slab=Reference)) # from the stackexchange example
dev.off()


# should clean up the reference names so they follow consistent naming convention
# also perhaps we should be doing this on the unimputed data??
# and pooled diamond that covers all studies?
# also what does the bigger cube mean?
# repeat but try aggregating by country
# wrap into a function, compare across different crops

# this is not a priority - can come back to do this


par(tck=-.01, mgp=c(1,0.01,0), mar=c(2,4,0,2))

forest(res, xlim=c(-4,5), mlab="Pooled Estimate", header=TRUE, ilab=ki, ilab.xpos=-2)
text(-2, res$k+2, "Estimates", font=2)


# multi-level influence analysis ------------------------------------------


