library(PWFSLSmoke)
library(AirMonitorPlots)
library(AirSensor)

# Download hourly data and use PWFSLSmoke routines to manipulate it
scap_hourly <-
  sensor_load() %>%
  monitor_subsetBy(stringr::str_detect(monitorID, "^SCAP")) %>%
  monitor_subset(tlim=c(20190702,20190706), timezone = "America/Los_Angeles")

# Show off AirMonitorPlots capabilities
ggplot_pm25Timeseries(scap_hourly) +
  geom_pm25Points() + 
  facet_wrap(facets = vars(monitorID), ncol=4) + 
  ylim(0,200) +
  custom_aqiStackedBar() +
  ggtitle("Los Angeles Fireworks!")
