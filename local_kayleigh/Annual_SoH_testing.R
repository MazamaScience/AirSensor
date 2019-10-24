library(AirSensor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)
library(purrr)

# ---- Series of pats required for testing all the SoH() functions -----------

ex_pat <- example_pat

# pat for testing pctDC
setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
pas <- pas_load(archival = TRUE)
pat_b_zero <- pat_createNew(pas, "SCAP_46", startdate = "2019-07-01", enddate = "2019-07-08", timezone = "America/Los_Angeles")

# new pat for testing pctReporting
pat_new <- pat_createNew(pas, label = "#SFAQ16", startdate = "20190601", enddate = "20190822")

# ----- Whole year from USFS -------------------------------------------------

setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
pas <- pas_load()
pas %>% pas_filter(stateCode == "WA") %>% pas_leaflet()

pat <- pat_load("MV Clean Air Ambassador @ Willowbrook Farm", startdate = 20190101, enddate = 20191023)
pat %>% pat_multiplot()

SoH <- pat_dailyStateOfHealth(pat)

# ----- Plotting ------------------------------------------------------------
plot(pat$data$datetime, pat$data$pm25_A, pch='.', 
     xlim = MazamaCoreUtils::timeRange(starttime = "20190801", endtime = "20191101", timezone = "UTC") ,
     ylim = c(0, 200))
plot(pat$data$datetime, pat$data$pm25_A, pch='.', ylim = c(0, 200))
points(SoH$datetime, SoH$pm25_A_pctDC, col = "red")
points(SoH$datetime, SoH$pm25_A_pctReporting, col = "blue")
points(SoH$datetime, SoH$pm25_A_pctValid, col = "green")



points(tbl$datetime, tbl$humidity_count, pch = '.', col = 'red')




