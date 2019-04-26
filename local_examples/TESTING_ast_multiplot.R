# Experimenting with QC of raw pm25 data

# required libraries
library(AirSensor)
library(dplyr)

pat <-
  AirSensor::example_pat %>%
  pat_filterDate(20180801, 20180901)

ast <- createASTimeseriesObject(pat, "10 min")

layout(matrix(1:3))
plot(ast$data$date, ast$data$temperature, pch = 15, main = "temperature")
plot(ast$data$date, ast$data$temperature_ct, pch = 15)
plot(ast$data$date, ast$data$temperature_sd, pch = 15)

layout(matrix(1:3))
plot(ast$data$date, ast$data$humidity, pch = 15, main = "humidity")
plot(ast$data$date, ast$data$humidity_ct, pch = 15)
plot(ast$data$date, ast$data$humidity_sd, pch = 15)

layout(matrix(1:3))
plot(ast$data$date, ast$data$pm25, pch = 15, main = "pm25")
plot(ast$data$date, ast$data$pm25_ct, pch = 15)
plot(ast$data$date, ast$data$pm25_sd, pch = 15)

pat <-
  AirSensor::example_pat_failure

ast <- createASTimeseriesObject(pat, "10 min")

layout(matrix(1:3))
plot(ast$data$date, ast$data$temperature, pch = 15, main = "temperature")
plot(ast$data$date, ast$data$temperature_ct, pch = 15)
plot(ast$data$date, ast$data$temperature_sd, pch = 15)

layout(matrix(1:3))
plot(ast$data$date, ast$data$humidity, pch = 15, main = "humidity")
plot(ast$data$date, ast$data$humidity_ct, pch = 15)
plot(ast$data$date, ast$data$humidity_sd, pch = 15)

layout(matrix(1:3))
plot(ast$data$date, ast$data$pm25, pch = 15, main = "pm25")
plot(ast$data$date, ast$data$pm25_ct, pch = 15)
plot(ast$data$date, ast$data$pm25_sd, pch = 15)
