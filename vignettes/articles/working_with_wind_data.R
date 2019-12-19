## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(AirSensor)
library(dplyr)
library(worldmet)

## ----show_site_map-------------------------------------------------------
worldmet::getMeta(lon = -118, lat = 34, n = 25, returnMap = TRUE)

## ----load_site_data, warning=FALSE---------------------------------------
# Always specify a timezone wherever possible!
timezone <- "America/Los_Angeles"

# Define the timeframe.
start <- lubridate::ymd("2019-06-01", tz = timezone)
end   <- lubridate::ymd("2019-06-30", tz = timezone)

# Load Los Alamitos Airport site data for the timeframe.
siteData <- worldmet::importNOAA(code = "722975-53141",
                                 year = 2019,
                                 parallel = FALSE)

names(siteData)

## ----subset_wind_data, echo=TRUE-----------------------------------------
windData <- 
  dplyr::select(siteData, c("date", "wd", "ws")) %>% 
  dplyr::filter(date >= start, date < end)

## ----plot_data, echo=TRUE, warning=FALSE---------------------------------
setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")

sensor <- 
  AirSensor::sensor_load(startdate = start, enddate = end) %>%
  AirSensor::sensor_filterMeta(monitorID == "SCSB_02")

AirSensor::sensor_pollutionRose(sensor, windData, statistic = "prop.mean")
AirSensor::sensor_polarPlot(sensor, windData, resolution = "normal")

