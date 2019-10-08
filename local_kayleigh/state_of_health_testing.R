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
pat6 <- pat_load("SCSB_20", 20190101, 20191010)


# put the different sensors into a list:
pat_list <- list(pat1, pat2, pat3, pat4, pat5)
names(pat_list) <- c("pat1", "pat2", "pat3", "pat4", "pat5")

# pre-define a list of dataframes to store the agg-stats
#aggregationStats <- list(data.frame())
aggregationStatsList <- list()

# agg_stats for one station for testing:
agg_stat_test <- pat_aggregateOutlierCounts(pat6)

# calculate agg stats for each station
for (i in seq_along(pat_list)) {
  aggregationStatsList[[i]] <- pat_aggregateOutlierCounts(pat_list[[i]])
}

PurpleAirQC_aggregationPlot(aggregationStats[[1]], autoRange = TRUE)


# Begin SOH_pctReporting()

samplingFreq <- 30 
samplesperDay <- samplingFreq*24
tbl <- 
  agg_stat_test %>%
  mutate(day = as.Date(datetime, format="%Y-%m-%d")) %>%
  group_by(day) %>%
  summarise(daily_sum = sum(pm25_A_count)) %>%
  mutate(pct_Reporting = daily_sum/samplesperDay*100)

# Begin SoH_pctValid()

humidity_low <- 0
humidity_high <- 100
temp_low <- -40
temp_high <- 185
pm25_low <- 0
pm25_high <- 1000







# # example for loop, works: 
# for (i in seq_along(pat_list)) {
#   print(names(pat_list[[i]]))
# }

