# Experimenting with QC of raw pm25 data

# required libraries
library(AirSensor)
library(dplyr)

pat <-
  AirSensor::example_pat %>%
  pat_filterDate(20180801, 20180901)

ast <- pat_createASTimeseries(pat, "10 min")

layout(matrix(1:3))
plot(ast$data$datetime, ast$data$temperature_mean, pch = 15, main = "temperature")
plot(ast$data$datetime, ast$data$temperature_count, pch = 15, main = "temperature count")
plot(ast$data$datetime, ast$data$temperature_sd, pch = 15, main = "temperature sd")

layout(matrix(1:3))
plot(ast$data$datetime, ast$data$humidity_mean, pch = 15, main = "humidity")
plot(ast$data$datetime, ast$data$humidity_count, pch = 15, main = "humidity count")
plot(ast$data$datetime, ast$data$humidity_sd, pch = 15, main = "humidity sd")

layout(matrix(1:6))
plot(ast$data$datetime, ast$data$pm25_A_mean, pch = 15, main = "pm25 A")
plot(ast$data$datetime, ast$data$pm25_A_count, pch = 15, main = "pm25 A count")
plot(ast$data$datetime, ast$data$pm25_A_sd, pch = 15, main = "pm25 A sd")
plot(ast$data$datetime, ast$data$pm25_B_mean, pch = 15, main = "pm25 B")
plot(ast$data$datetime, ast$data$pm25_B_count, pch = 15, main = "pm25 B count")
plot(ast$data$datetime, ast$data$pm25_B_sd, pch = 15, main = "pm25 B sd")

pat <-
  AirSensor::example_pat_failure

ast <- pat_createASTimeseries(pat, "10 min")

layout(matrix(1:3))
plot(ast$data$datetime, ast$data$temperature_mean, pch = 15, main = "temperature")
plot(ast$data$datetime, ast$data$temperature_count, pch = 15, main = "temperature count")
plot(ast$data$datetime, ast$data$temperature_sd, pch = 15, main = "temperature sd")

layout(matrix(1:3))
plot(ast$data$datetime, ast$data$humidity_mean, pch = 15, main = "humidity")
plot(ast$data$datetime, ast$data$humidity_count, pch = 15, main = "humidity count")
plot(ast$data$datetime, ast$data$humidity_sd, pch = 15, main = "humidity sd")

layout(matrix(1:6))
plot(ast$data$datetime, ast$data$pm25_A_mean, pch = 15, main = "pm25 A")
plot(ast$data$datetime, ast$data$pm25_A_count, pch = 15, main = "pm25 A count")
plot(ast$data$datetime, ast$data$pm25_A_sd, pch = 15, main = "pm25 A sd")
plot(ast$data$datetime, ast$data$pm25_B_mean, pch = 15, main = "pm25 B")
plot(ast$data$datetime, ast$data$pm25_B_count, pch = 15, main = "pm25 B count")
plot(ast$data$datetime, ast$data$pm25_B_sd, pch = 15, main = "pm25 B sd")
