

# READ IN INCOMPLETE, UNIMPUTED DATA --------------------------------------

AGIMPACTS_bs_tp <- readRDS(here("processed", "AGIMPACTS_bs_tp.Rdata")) 

# Graph correlations ------------------------------------------------------


png(file = file.path(here("results", "figures", "cor_plot.png")))

ggpairs(AGIMPACTS_bs_tp[, c("CO2.Change", 
                            "Temp.Change", 
                            "Yield.Change", 
                            "Precipitation.change",
                            "Baseline_tmp_weighted",
                            "Baseline_pre_weighted")],
        diag = list(continuous = "density"),
        axisLabels = "show") 

dev.off()

# plot baseline-temp for each country

ggplot(data = AGIMPACTS_MAIN_INCOMPLETE, aes(x=Country2, y=Baseline_tmp_weighted)) +
  geom_point()

# there is more than one data point per country, though they tend to be closer than for study

# plot baseline-temp for each study
ggplot(data = AGIMPACTS_MAIN_INCOMPLETE, aes(x=Reference, y=Baseline_tmp_weighted)) +
  geom_point()


# Graph patterns of missing data ------------------------------------------

# note these won't change - adding baseline precip hasn't changed these patterns
# as missingness for bs temp and bs precip stem from missing baseline year info

missing <- VIM::aggr(AGIMPACTS_MAIN, 
                numbers=TRUE, sortVars=TRUE, labels=names(AGIMPACTS_MAIN), cex.axis=.5, gap=0, 
                ylab=c("Proportion of missingness","Missingness Pattern"))

VIM::aggr(AGIMPACTS_MAIN[,c("Baseline.start", "Baseline.end", "CO2.Change", "Temp.Change", "Yield.Change", "Precipitation.change")], 
     numbers=TRUE, sortVars=TRUE, labels=names(AGIMPACTS_MAIN[,c("Baseline.start", "Baseline.end", "CO2.Change", "Temp.Change", "Yield.Change", "Precipitation.change")]), 
     cex.axis=.6, gap=0, 
     ylab=c("Proportion of missingness","Missingness Pattern"))

VIM::aggr(AGIMPACTS_MAIN[,c("Baseline.start", "Baseline.end", "CO2.Change", "Temp.Change", "Yield.Change", "Precipitation.change")], 
     numbers=TRUE, sortVars=TRUE, combined=TRUE,
     labels=names(AGIMPACTS_MAIN[,c("Baseline.start", "Baseline.end", "CO2.Change", "Temp.Change", "Yield.Change", "Precipitation.change")]), 
     cex.axis=.6, gap=0, 
     ylab=c("Proportion of missingness","Missingness Pattern"))

VIM::aggr(AGIMPACTS_MAIN[,c("Baseline.start", "Baseline.end", "CO2.Change", "Temp.Change", "Yield.Change", "Precipitation.change")], 
     numbers=TRUE, sortVars=TRUE, 
     labels=names(AGIMPACTS_MAIN[,c("Baseline.start", "Baseline.end", "CO2.Change", "Temp.Change", "Yield.Change", "Precipitation.change")]), 
     cex.axis=1, gap=0, oma=c(10,5,5,5), 
     ylab=c("Proportion of missingness","Missingness Pattern"))

# actual count for baseline start and end dates

sum(is.na(AGIMPACTS_MAIN$Baseline.end)) # 72
# baseline temperatures still missing for 72 missing baseline start/end years

sum(is.na(AGIMPACTS_MAIN_INCOMPLETE$Baseline_tmp_weighted)) # 1000 missing baseline temps
sum(is.na(AGIMPACTS_MAIN_INCOMPLETE$Temp.Change)) # 2338
sum(is.na(AGIMPACTS_MAIN_INCOMPLETE$Yield.Change)) # 54
sum(is.na(AGIMPACTS_MAIN_INCOMPLETE$Precipitation.change))    # 3275
sum(is.na(AGIMPACTS_MAIN_INCOMPLETE$CO2.Change)) # 3201
sum(is.na(AGIMPACTS_MAIN_INCOMPLETE$adapt_dummy)) # 0
sum(is.na(AGIMPACTS_MAIN_INCOMPLETE$Country2)) # 0

