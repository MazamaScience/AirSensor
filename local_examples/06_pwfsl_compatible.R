# required libraries
library(AirSensor)
library(PWFSLSmoke)

ws_monitor_A <-
  example_pat %>%
  pat_createASTimeseries() %>%
  ast_createAirSensor("pm25_A_mean")

ws_monitor_B <-
  example_pat %>%
  pat_createASTimeseries() %>%
  ast_createAirSensor("pm25_B_mean")

layout(matrix(1:2))
monitor_dailyBarplot(ws_monitor_A)
monitor_dailyBarplot(ws_monitor_B)
