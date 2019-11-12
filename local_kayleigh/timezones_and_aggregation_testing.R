library(AirSensor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)
library(purrr)
library(skimr)

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


# --- testing manual aggregation using dplyr -----------------------------------

tbl <-
  pat$data %>%  
  
  #dplyr::mutate(daystamp = lubridate::round_date(datetime, "30 mins") )%>%
  dplyr::mutate(daystampFloor = lubridate::floor_date(datetime, "10 mins") )%>%
  dplyr::mutate(hourstamp = strftime(.data$datetime, "%Y%m%d%H", tz = timezone)) 

# ---- testing A/B match up with NA's ------------------------------------------

setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
pas <- pas_load(archival = TRUE)

# Clean channels

SCAP_41_early <- pat_createNew(pas, "SCAP_41", startdate = "2019-02-01", enddate = "2019-04-01", timezone = "America/Los_Angeles")
# 19.2%
SCAP_41_late <- pat_createNew(pas, "SCAH_29", startdate = "2019-08-01", enddate = "2019-10-01", timezone = "America/Los_Angeles")
# 0.04%

SCAP_19_early <- pat_createNew(pas, "SCAP_19", startdate = "2019-02-01", enddate = "2019-04-01", timezone = "America/Los_Angeles")
# 9.9%
SCAP_19_late <- pat_createNew(pas, "SCAP_19", startdate = "2019-08-01", enddate = "2019-10-01", timezone = "America/Los_Angeles")
# 37%

SCNP_05_early <- pat_createNew(pas, "SCNP_05", startdate = "2019-02-01", enddate = "2019-04-01", timezone = "America/Los_Angeles")
# 12%
SCNP_05_late <- pat_createNew(pas, "SCNP_05", startdate = "2019-08-01", enddate = "2019-10-01", timezone = "America/Los_Angeles")
# 18.7%

# Noisy channels
# Channel A blows up in the second half of the year
SCAP_14_early <- pat_createNew(pas, "SCAP_14", startdate = "2019-02-01", enddate = "2019-04-01", timezone = "America/Los_Angeles")
# 33%
SCAP_14_late <- pat_createNew(pas, "SCAP_14", startdate = "2019-08-01", enddate = "2019-10-01", timezone = "America/Los_Angeles")
# 4.9%

# Missing summer, somewhat noisy?
MV_early <- pat_createNew(pas, "MV Clean Air Ambassador @ Willowbrook Farm", startdate = "2019-02-01", enddate = "2019-04-01", timezone = "America/Los_Angeles")
# 17%
MV_late <- pat_createNew(pas, "MV Clean Air Ambassador @ Willowbrook Farm", startdate = "2019-08-01", enddate = "2019-10-01", timezone = "America/Los_Angeles")
# 16.5%

# oddly rhythmic signal in the pct reporting plots
SCTV_40_early <- pat_createNew(pas, "SCTV_40", startdate = "2019-02-01", enddate = "2019-04-01", timezone = "America/Los_Angeles")
# 7.4%
SCTV_40_late <- pat_createNew(pas, "SCTV_40", startdate = "2019-08-01", enddate = "2019-10-01", timezone = "America/Los_Angeles")
# 11.7%

# day with alternating NA's
day_NA <- pat_filterDate(SCAP_19_late, startdate = 20190801, enddate = 20190802, timezone = "America/Los_Angeles")
# day with no NA's
day <- pat_filterDate(SCAP_19_late, startdate = 20190910, enddate = 20190911, timezone = "America/Los_Angeles")
# another day with alternating NA's
day_NA2 <- pat_filterDate(SCAP_19_late, startdate = 20190925, enddate = 20190926, timezone = "America/Los_Angeles")
pat_multiplot(day)
data <- day_NA$data
data_2 <- day_NA2$data

pat<- SCAP_19_late

# timeseriesTbl_multiplot(data)
# 
# #time <- data$datetime
# 
# A_missing <-
#   data %>%
#   filter(
#     is.na(.data$pm25_A) & !is.na(.data$pm25_B)
#   )
# B_missing <-
#   data %>%
#   filter(
#     !is.na(.data$pm25_A) & is.na(.data$pm25_B)
#   )
# timeseriesTbl_multiplot(A_missing, pattern = "pm25", style = "point")
# correlation <- cor(x=data$pm25_A, y=data$pm25_B, use = "pairwise.complete.obs")
# model <- lm(data$pm25_A ~ data$pm25_B)
# summary <- summary(model)

A_is_NA <- length(which(is.na(data$pm25_A) & !is.na(data$pm25_B)))
where_A_NA <- which(is.na(data$pm25_A) & !is.na(data$pm25_B))
B_is_NA <- length(which(!is.na(data$pm25_A) & is.na(data$pm25_B)))
where_B_NA <-which(!is.na(data$pm25_A) & is.na(data$pm25_B))
total_NA <- B_is_NA + A_is_NA
complete_pairs <- length(which(!is.na(data$pm25_A) & !is.na(data$pm25_B)))
where_complete <- which(!is.na(data$pm25_A) & !is.na(data$pm25_B))

fraction <- total_NA/complete_pairs
percent <- fraction*100

A_is_NA_2 <- length(which(is.na(data_2$pm25_A) & !is.na(data_2$pm25_B)))
where_A_NA_2 <- which(is.na(data_2$pm25_A) & !is.na(data_2$pm25_B))
B_is_NA_2 <- length(which(!is.na(data_2$pm25_A) & is.na(data_2$pm25_B)))
where_B_NA_2 <-which(!is.na(data_2$pm25_A) & is.na(data_2$pm25_B))
total_NA_2 <- B_is_NA_2 + A_is_NA_2
complete_pairs_2 <- length(which(!is.na(data_2$pm25_A) & !is.na(data_2$pm25_B)))
where_complete_2 <- which(!is.na(data_2$pm25_A) & !is.na(data_2$pm25_B))

fraction_2 <- total_NA_2/complete_pairs_2
percent_2 <- fraction_2*100

# ----- testing SoH's for special cases ----------------------------------------


setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
pas <- pas_load()

# A-channel "magic number"
pat_SCEM_05 <- pat_load("SCEM_05", startdate = 20190101, enddate = 20191023)
pat_multiplot(pat_SCEM_05)
pat_SCEM_05 %>% pat_filterDate(20190909, 20190912) %>% pat_multiplot()

SoH_SCEM_05 <- pat_dailySoH(pat_SCEM_05)

# Another case
pat_SCAP_46 <- pat_load("SCAP_46", startdate = 20190101, enddate = 20191023)
pat_multiplot(pat_SCAP_46)
pat_SCAP_46 %>% pat_filterDate(20190909, 20190912) %>% pat_multiplot()

SoH_SCAP_46 <- pat_dailySoH(pat_SCAP_46)

pat_empty_SCAP_46 <- pat_filterDate(pat_SCAP_46, 20190118, 20190130, timezone = "America/Los_Angeles")

# And another one
pat_SCAP_14 <- pat_load("SCAP_14", startdate = 20190101, enddate = 20191023)
pat_multiplot(pat_SCAP_14)
pat_SCAP_14 %>% pat_filterDate(20190909, 20190912) %>% pat_multiplot()

SoH_SCAP_14 <- pat_dailySoH(pat_SCAP_14)


gg1 <- ggplot(pat_SCAP_14$data) +
  geom_point(aes(datetime, pm25_A), pch = ".", alpha = 0.5, color = "darkred") +
  geom_point(aes(datetime, pm25_B), pch = ".", alpha = 0.3, color = "darkblue") 
gg1




day <- 20190102
SCAP_14_partial <- pat_filterDate(pat_SCAP_14,20190101, 20190526, timezone = "America/Los_Angeles")
SCAP_14_27 <- pat_filterDate(pat_SCAP_14,20190527, 20190529, timezone = "America/Los_Angeles")


pat <- SCAP_14_partial








