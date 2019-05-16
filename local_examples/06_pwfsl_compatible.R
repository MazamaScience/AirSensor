# required libraries
library(AirSensor)
library(PWFSLSmoke)

ws_monitor <-
  example_pat %>%
  pat_createASTimeseries() %>%
  ast_createAirSensor()

monitor_dailyBarplot(ws_monitor)
