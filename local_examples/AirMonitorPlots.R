# Required libraries
library(PWFSLSmoke)
library(AirSensor)
library(AirMonitorPlots)

# ----- Using purr::map() to work with lists of PAT objects --------------------

# Create a list of Nipomo "pat"s and convert them into "sensor"s
scnp <-
  pas_load() %>%
  dplyr::filter(stringr::str_detect(label, "^[Ss][Cc][Nn][Pp]_..$")) %>%
  pull(label) %>%
  purrr::map(pat_loadMonth, "201901") %>%
  purrr::map(pat_createAirSensor) %>%
  monitor_combine()

# Working with not-on-CRAN AirMonitorPlots
ggplot_pm25Diurnal(scnp,
                   startdate = 20190115, 
                   enddate = 20190122,
                   offsetBreaks = TRUE) + 
  stat_meanByHour(outpu = "AQIColors") + 
  facet_grid(rows = vars(monitorID)) +
  ggtitle("Nipomo Area Sensors")


# ----- Plotting pre-generated "sensor" objects --------------------------------

if ( FALSE ) {
 
  # Create sensor objects from data downloads
  scap <-
    pas_load() %>%
    dplyr::filter(stringr::str_detect(label, "^[Ss][Cc][Aa][Pp]_..$")) %>%
    pull(label) %>%
    purrr::map(pat_loadMonth, "201907") %>%
    purrr::map(pat_createAirSensor) %>%
    monitor_combine() %>%
    monitor_subset(tlim=c(20190702,20190706), timezone = "America/Los_Angeles")
  
}

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
