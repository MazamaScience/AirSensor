wind2020_orig <- read.csv("/Users/kayleigh/Data/Australia_on_fire/2020.csv", header = TRUE)
wind2019_orig <- read.csv("/Users/kayleigh/Data/Australia_on_fire/2019.csv", header = TRUE)

wind2019 <- 
  wind2019_orig %>%
  dplyr::select(date = DATE, wd = HourlyWindDirection, ws = HourlyWindSpeed)

wind2019$date <- MazamaCoreUtils::parseDatetime(wind2019$date, timezone = "Australia/Canberra")


wind2020 <- 
  wind2020_orig %>%
  dplyr::select(date = DATE, wd = HourlyWindDirection, ws = HourlyWindSpeed)

wind2020$date <- MazamaCoreUtils::parseDatetime(wind2020$date, timezone = "Australia/Canberra")

wind <- dplyr::bind_rows(wind2019, wind2020)
min_date <- MazamaCoreUtils::parseDatetime(20191201, timezone = "Australia/Canberra")
wind_filt <- dplyr::filter(wind, date >= min_date)
wind_filt <- unique(wind_filt)


canberra_wind <- dplyr::filter(wind_filt, lubridate::minute(wind_filt$date) == 0)

filename <- c("canberra_wind.rda")
filepath <- file.path("/Users/kayleigh/Data/Australia_on_fire/", filename)
save(list = "canberra_wind", file = filepath)


