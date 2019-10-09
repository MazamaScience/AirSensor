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
# # To make a list and loop: 
# pat2 <- pat_createNew(pas_state, label = "Whitworth University Soccer/Softball Complex")
# pat3 <- pat_createNew(pas_state, label = "TimberlakesCC")
# pat4 <- pat_createNew(pas_state, label = "Lynden")
# pat5 <- pat_createNew(pas_state, label = "North Richland")
# pat6 <- pat_load("SCSB_20", 20190101, 20191010)
# 
# # put the different sensors into a list:
# pat_list <- list(pat1, pat2, pat3, pat4, pat5)
# names(pat_list) <- c("pat1", "pat2", "pat3", "pat4", "pat5")
# 
# # pre-define a list of dataframes to store the agg-stats
# aggregationStatsList <- list()
# 
# # calculate agg stats for each station
# for (i in seq_along(pat_list)) {
#   aggregationStatsList[[i]] <- pat_aggregateOutlierCounts(pat_list[[i]])
# }
# PurpleAirQC_aggregationPlot(aggregationStats[[1]], autoRange = TRUE)

# agg_stats for one station for testing:
agg_stat_test <- pat_aggregateOutlierCounts(pat1)

# ----- Begin SOH_pctReporting() -----------------------------------------------

samplingFreq <- 30 
samplesPerDay <- samplingFreq*24
tbl_test <- 
  pat1 %>%
  pat_aggregateOutlierCounts( period = "day") %>%
  # agg_stat_test %>%
  # mutate(day = as.Date(datetime, format="%Y-%m-%d")) %>%
  # group_by(day) %>%
  # summarise(daily_sum = sum(pm25_A_count)) %>%
  mutate(pct_Reporting = pm25_A_count/samplesPerDay*100)

# ----- Begin SoH_pctValid() ---------------------------------------------------
# test with bad data:
pat1 <- example_pat_failure_B

samplingFreq <- 30 
samplesPerDay <- samplingFreq*24
baseline_tbl <-
  pat1 %>%
  pat_aggregateOutlierCounts(period = "day")

valid_tbl <-
  pat1 %>%
  pat_qc()%>%
  pat_aggregateOutlierCounts(period = "day") %>%
  mutate(pm25_A_pctValid = pm25_A_count/baseline_tbl$pm25_A_count*100) %>%
  mutate(pm25_B_pctValid = pm25_B_count/baseline_tbl$pm25_B_count*100) %>%
  mutate(temperature_pctValid = 
           temperature_count/baseline_tbl$temperature_count*100) %>%
  mutate(humidity_pctValid = humidity_count/baseline_tbl$humidity_count*100) %>%
  select("datetime", contains("Valid"))

# ----- Begin SoH_pctDC() ------------------------------------------------------

pat1 <- example_pat_failure_B

pct_DC_tbl <-
  pat1 %>%
  pat_aggregate(period = "30 min") %>%
  filter()











