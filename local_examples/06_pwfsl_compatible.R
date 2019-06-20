# required libraries
library(AirSensor)
library(PWFSLSmoke)

ws_monitor_A <-
  example_pat %>%
  pat_createAirSensor(channel = "a")

ws_monitor_B <-
  example_pat %>%
  pat_createAirSensor(channel = "b")

layout(matrix(1:2))
monitor_dailyBarplot(ws_monitor_A)
monitor_dailyBarplot(ws_monitor_B)

# Or the ggplot version
library(AirMonitorPlots)

gg_A <- 
  example_pat %>%
  pat_createAirSensor(channel = "a") %>%
  monitor_ggDailyBarplot()

gg_B <- 
  example_pat %>%
  pat_createAirSensor(channel = "b") %>%
  monitor_ggDailyBarplot()

multi_ggplot(gg_A, gg_B)
