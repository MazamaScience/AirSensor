library(AirSensor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)
library(purrr)

# Set up dataset for testing plot
setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")

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
# timeseriesTbl_multiPlot(aggregationStats[[1]], autoRange = TRUE)

# agg_stats for one station for testing:
agg_stat_test <- pat_aggregateOutlierCounts(pat1)

# ----- Begin PurpleAirSoH_dailyPctReporting() -----------------------------------------------

samplingFreq <- 30 
samplesPerDay <- samplingFreq*24
tbl_test <- 
  pat1 %>%
  pat_aggregateOutlierCounts( period = "day") %>%
  agg_stat_test %>%
  mutate(day = as.Date(datetime, format="%Y-%m-%d")) %>%
  group_by(day) %>%
  summarise(daily_sum = sum(pm25_A_count)) %>%
  mutate(pct_Reporting = pm25_A_count/samplesPerDay*100)

# ----- Begin PurpleAirSoH_dailyPctValid() ---------------------------------------------------
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

# ----- Begin PurpleAirSoH_dailyPctDC() ------------------------------------------------------


pat1 <- example_pat_failure_B
agg_test <- pat_aggregateOutlierCounts(pat1)

pat_b_zero <- pat_createNew(pas, "SCAP_46", 
                            startdate = "2019-07-01", 
                            enddate = "2019-07-08",
                            timezone = "America/Los_Angeles")
timezone<- "America/Los_Angeles"
pat1<- pat_b_zero
pct_DC_tbl <-
  pat1 %>%
  pat_aggregate(period = "1 hour") %>%
  mutate(daystamp = strftime(datetime, "%Y%m%d", tz = timezone) ) %>%
  group_by(daystamp) %>% 
  add_tally(pm25_A_sd==0, name = "DCSignalCount_pm25_A") %>%
  add_tally(pm25_B_sd==0, name = "DCSignalCount_pm25_B") %>%
  add_tally(temperature_sd==0, name = "DCSignalCount_temperature") %>%
  add_tally(humidity_sd==0, name = "DCSignalCount_humidity") %>%
  summarise_at(.vars = c("DCSignalCount_pm25_A", "DCSignalCount_pm25_B", 
                         "DCSignalCount_temperature", "DCSignalCount_humidity"),
               max) %>%
  mutate(DC_PM25_A_hourCount = DCSignalCount_pm25_A/1 ) %>%
  mutate(DC_PM25_B_hourCount = DCSignalCount_pm25_B/1 ) %>%
  mutate(DC_temperature_hourCount = DCSignalCount_temperature/1 ) %>%
  mutate(DC_humidity_hourCount = DCSignalCount_humidity/1 ) %>%
  mutate(pctDC_PM25_A = DC_PM25_A_hourCount/24*100) %>%
  mutate(pctDC_PM25_B = DC_PM25_B_hourCount/24*100) %>%
  mutate(pctDC_temperature = DC_temperature_hourCount/24*100) %>%
  mutate(pctDC_humidity = DC_humidity_hourCount/24*100)%>%
  mutate(datetime = MazamaCoreUtils::parseDatetime(daystamp, timezone = timezone))

# mutate(hourCount = n/2) %>%
# mutate(pctDC = hourCount/24*100)


#QC
test <- filter(agg_test, temperature_sd==0)
plot(agg_test$datetime, agg_test$temperature_sd)
points(test$datetime,test$temperature_sd, col = "red")

#plot(pct_DC_tbl$datetime, pct_DC_tbl$temperature_sd)
plot(pct_DC_tbl$datetime, pct_DC_tbl$DCSignalCount_temperature, col = "pink")
points(pct_DC_tbl$datetime, pct_DC_tbl$DCSignalCount_pm25_A, col = "red")
points(pct_DC_tbl$datetime, pct_DC_tbl$DCSignalCount_pm25_B, col = "blue")
points(pct_DC_tbl$datetime, pct_DC_tbl$DCSignalCount_humidity, col = "green")

plot(pct_DC_tbl$day, pct_DC_tbl$pctDC_temperature, col = "pink")
points(pct_DC_tbl$day, pct_DC_tbl$pctDC_PM25_A, col = "red")
points(pct_DC_tbl$day, pct_DC_tbl$pctDC_PM25_B, col = "blue")
points(pct_DC_tbl$day, pct_DC_tbl$pctDC_humidity, col = "green")

# # For Reference, this is how to use tally() in a function:
# PurpleAirSoH_dailyPctDC <- function(
#   pat = NULL,
#   aggregation_period = "30 min",
#   parameter_sd = NULL
# ){
#   pct_DC_tbl <-
#     pat %>%
#     pat_aggregate(period = aggregation_period) %>%
#     dplyr::mutate(day = as.Date(.data$datetime, format="%Y-%m-%d")) %>%
#     dplyr::group_by(.data$day) %>% 
#     dplyr::tally(.data[[parameter_sd]]==0)%>%
#     dplyr::mutate(hourCount = n/hourFactor) %>%
#     dplyr::mutate(pctDC = hourCount/24*100)
#   
# }

# ----- Begin PurpleAirSoH_dailyCorrelation() testing -------

pat <- example_pat
timezone<- "America/Los_Angeles"
#pat_multiPlot(pat)
#pat_scatterPlot(pat)

pat_tbl<-
  pat %>%
  # Aggregate by 10 min to avoid NA offset between channels A and B. Aggregate
  # to 10 min fo speed
  pat_aggregate(period = "10 min") %>%
  mutate(localTime = lubridate::with_tz(datetime, tzone=pat$meta$timezone)) %>%
  dplyr::mutate(localDay = strftime(localTime, "%F", tz=pat$meta$timezone)) %>%
  dplyr::group_by(.data$localDay)

# Preallocate a list
correlation_list <- list()


# Loop through each unique day in the dataset
for ( day in unique(pat_tbl$localDay) ) {
  
  # pull out the data associated with one day at a time
  day_tbl <- 
    dplyr::filter(pat_tbl, localDay == day)
  

  correlation_list[[day]] <- day_tbl

  
  # calculate the correlation between several variables
  pm25_A_pm25_B_cor <- cor(day_tbl$pm25_A_mean, day_tbl$pm25_B_mean, use = "pairwise.complete.obs")
  pm25_A_humidity_cor <- cor(day_tbl$pm25_A_mean, day_tbl$humidity_mean, use = "pairwise.complete.obs")
  pm25_A_temperature_cor <- cor(day_tbl$pm25_A_mean, day_tbl$temperature_mean, use = "pairwise.complete.obs")
  pm25_B_humidity_cor <- cor(day_tbl$pm25_B_mean, day_tbl$humidity_mean, use = "pairwise.complete.obs")
  pm25_B_temperature_cor <- cor(day_tbl$pm25_B_mean, day_tbl$temperature_mean, use = "pairwise.complete.obs")
  temperature_humidity_cor <- cor(day_tbl$temperature_mean, day_tbl$humidity_mean, use = "pairwise.complete.obs")

  
  # add the correlation per day, per variable comparison to a list
  correlation_list[[day]] <- list(
    pm25_A_pm25_B_cor = pm25_A_pm25_B_cor,
    pm25_A_humidity_cor = pm25_A_humidity_cor,
    pm25_A_temperature_cor = pm25_A_temperature_cor,
    pm25_B_humidity_cor = pm25_B_humidity_cor,
    pm25_B_temperature_cor = pm25_B_temperature_cor,
    temperature_humidity_cor = temperature_humidity_cor
  )

}

colnames <- c( "datetime","pm25_A_pm25_B_cor", "pm25_A_humidity_cor", "pm25_A_temperature_cor",
               "pm25_B_humidity_cor", "pm25_B_temperature_cor",
               "temperature_humidity_cor")

# reformat the list as a tibble
int_correlation_tbl <- dplyr::as_tibble(correlation_list)

# reformat as a matrix in order to have the desired outcome after transpose
correlation_matrix<- t(as.matrix(int_correlation_tbl))

# reformat as a data.frame in order to change datetime to POSIXCT and add the colnames
correlation_df <- data.frame(correlation_matrix)
# reformat to tibble to change the datetime from rownames to an actual column of data
correlation_df <- tibble::rownames_to_column(correlation_df, var="datetime") 
# change datetime into a POSIXCT and add the column names
correlation_df$datetime <- MazamaCoreUtils::parseDatetime(correlation_df$datetime, timezone = timezone)
colnames(correlation_df) <-colnames

# re-define each of the columns as numeric rather than lists for easier plotting in ggplot
correlation_df$pm25_A_pm25_B_cor <- as.numeric(correlation_df$pm25_A_pm25_B_cor)
correlation_df$pm25_A_temperature_cor <- as.numeric(correlation_df$pm25_A_temperature_cor)
correlation_df$pm25_A_humidity_cor <- as.numeric(correlation_df$pm25_A_humidity_cor)
correlation_df$pm25_B_temperature_cor <- as.numeric(correlation_df$pm25_B_temperature_cor)
correlation_df$pm25_B_humidity_cor <- as.numeric(correlation_df$pm25_B_humidity_cor)
correlation_df$temperature_humidity_cor <- as.numeric(correlation_df$temperature_humidity_cor)

# plot the columns to ensure the values fall within the expected range.
gg <- ggplot(correlation_df ) +
  geom_point(aes(x = datetime, y = pm25_A_pm25_B_cor), color = "black") +
  geom_point(aes(x = datetime, y = pm25_A_humidity_cor), color = "red") +
  geom_point(aes(x = datetime, y = pm25_A_temperature_cor), color = "blue") +
  geom_point(aes(x = datetime, y = pm25_B_humidity_cor), color = "green") +
  geom_point(aes(x = datetime, y = pm25_B_temperature_cor), color = "pink") 
  geom_point(aes(x = datetime, y = temperature_humidity_cor), color = "cyan") 

plot(gg)


# ----- testing -------------


baseline_tbl <-
  pat %>%
  #mutate(localTime = lubridate::with_tz(.data$datetime, tzone=timezone)) %>%
  pat_aggregateOutlierCounts(period = "day")



