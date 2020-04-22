library(PWFSLSmoke)
library(AirMonitorPlots)
library(AirSensor)

setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")

# Download hourly data and use PWFSLSmoke routines to manipulate it
scap_hourly <-
  sensor_load(startdate = 20190702, enddate = 20190706, timezone = "America/Los_Angeles") %>%
  monitor_subsetBy(stringr::str_detect(monitorID, "^SCAP"))

# Show off AirMonitorPlots capabilities
ggplot_pm25Timeseries(scap_hourly) +
  geom_pm25Points() + 
  facet_wrap(facets = vars(monitorID), ncol=4) + 
  ylim(0,200) +
  custom_aqiStackedBar() +
  ggtitle("Los Angeles Fireworks!")
