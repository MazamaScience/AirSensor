library(AirSensor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)
library(purrr)

aggregationPeriod <- "1 day"
#timezone <- "America/Los_Angeles"

# ---- Series of pats required for testing all the SoH() functions -----------

ex_pat <- example_pat

# pat for testing pctDC
setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
pas <- pas_load(archival = TRUE)
pat_b_zero <- pat_createNew(pas, "SCAP_46", startdate = "2019-07-01", enddate = "2019-07-08", timezone = "America/Los_Angeles")

# new pat for testing pctReporting
pat_new <- pat_createNew(pas, label = "#SFAQ16", startdate = "20190601", enddate = "20190822")


# ----Pacific ----------------------------------

pat <- example_pat

pat$data$datetime <- lubridate::with_tz(pat$data$datetime, tzone = "America/Los_Angeles")

hour <- lubridate::hour(pat$data$datetime)
start <- pat$data$datetime[ min(which(hour == 0)) ]
end <- pat$data$datetime[ max(which(hour == 23)) ]

pat_filt <- pat_filterDate(pat, start, end, timezone = "America/Los_Angeles") 

# then use dyplyr filter

d_pacific <- pat$data

tbl_pacific <- d_pacific %>%
  dplyr::mutate(daystamp = strftime(datetime, "%Y%m%d", tz = "America/Los_Angeles")) %>%
  dplyr::group_by(daystamp) %>% 
  dplyr::summarise_at(.vars = c("pm25_A", "pm25_B", "temperature", "humidity"), mean, na.rm = TRUE)

tbl_pacific_hour <- d_pacific %>%
  dplyr::mutate(hourstamp = strftime(datetime, "%Y%m%d%H", tz = "America/Los_Angeles")) %>%
  dplyr::group_by(hourstamp) %>% 
  dplyr::summarise_at(.vars = c("pm25_A", "pm25_B", "temperature", "humidity"), mean, na.rm = TRUE) 

tbl_pacific_hour$hourstamp <- MazamaCoreUtils::parseDatetime(tbl_pacific_hour$hourstamp, timezone = "America/Los_Angeles") 

# --- UTC --------------------------

d_utc <- pat$data

tbl_utc <- d_utc %>%
  dplyr::mutate(daystamp = strftime(datetime, "%Y%m%d", tz = "UTC")) %>%
  dplyr::group_by(daystamp) %>% 
  dplyr::summarise_at(.vars = c("pm25_A", "pm25_B", "temperature", "humidity"), mean, na.rm = TRUE)

tbl_utc_hour <- d_utc %>%
  dplyr::mutate(hourstamp = strftime(datetime, "%Y%m%d%H", tz = "UTC")) %>%
  dplyr::group_by(hourstamp) %>% 
  dplyr::summarise_at(.vars = c("pm25_A", "pm25_B", "temperature", "humidity"), mean, na.rm = TRUE) 

tbl_utc_hour$hourstamp <- MazamaCoreUtils::parseDatetime(tbl_utc_hour$hourstamp, timezone = "UTC") 


plot(tbl_pacific$daystamp, tbl_pacific$pm25_A)
points(tbl_utc$daystamp, tbl_utc$pm25_A, col = "red")

plot(tbl_pacific_hour$hourstamp, tbl_pacific_hour$pm25_A, ylim = c(1, 2))
points(tbl_utc_hour$hourstamp, tbl_utc_hour$pm25_A, col = "red")




