library(AirSensor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)

# Set up dataset for testing plot
setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")

# Get pas for a state
pas <- pas_load()
pas_state <- pas %>% pas_filter(stateCode == "WA")
# Get the pat of interest
pat1 <- pat_createNew(pas_state, label = "Nickelsville Georgetown")
pat2 <- pat_createNew(pas_state, label = "Whitworth University Soccer/Softball Complex")
pat3 <- pat_createNew(pas_state, label = "TimberlakesCC")
pat4 <- pat_createNew(pas_state, label = "Lynden")
pat5 <- pat_createNew(pas_state, label = "North Richland")

# put the different sensors into a list:
pat_list <- list(pat1, pat2, pat3, pat4, pat5)
names(pat_list) <- c("pat1", "pat2", "pat3", "pat4", "pat5")

# pre-define a list of dataframes to store the agg-stats
aggregationStats <- list(data.frame())

# agg_stats for one station for testing:
agg_stat_test <- pat_aggregateOutlierCounts(pat1)

# calculate agg stats for each station
for (i in seq_along(pat_list)) {
  aggregationStats[[i]] <- pat_aggregateOutlierCounts(pat_list[[i]])
}

PurpleAirQC_aggregationPlot(aggregationStats[[1]], ylim = "free_y")


# Begin SOH_pctReporting()

samplingFreq <- 30
total_pm25ACount <- summarise(agg_stat_test, sum(agg_stat_test$pm25_A_count))
pctReporting <- total_pm25ACount/samplingFreq







# # example for loop, works: 
# for (i in seq_along(pat_list)) {
#   print(names(pat_list[[i]]))
# }

