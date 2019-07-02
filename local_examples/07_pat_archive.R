# required libraries
library(AirSensor)
library(PWFSLSmoke)

# Load today's PAS object
pas <- pas_load()

# SCNP labels
scnp_labels <-
  pas %>%
  dplyr::filter(stringr::str_detect(label, "^[Ss][Cc][Nn][Pp]_..$")) %>%
  pull(label)
  
# Load a list of "PAT"pat" objects
patList <- 
  purrr::map(scnp_labels, pat_loadMonth, "201901")

# Create a list of "airsensor" objects
airsensorList <- 
  purrr::map(patList, pat_createAirSensor)

# Now we can use PWFSLSmoke functions
scnp <- monitor_combine(airsensorList)

layout(matrix(seq(length(scnp_labels))))
for ( monitorID in scnp$meta$monitorID ) {
  monitor_dailyBarplot(scnp, monitorID)
}

# NOTE:  We could have created the scnp object this in one fell swoop
scnp <-
  pas %>%
  dplyr::filter(stringr::str_detect(label, "^[Ss][Cc][Nn][Pp]_..$")) %>%
  pull(label) %>%
  purrr::map(pat_loadMonth, "201901") %>%
  purrr::map(pat_createAirSensor) %>%
  monitor_combine()

# Working with not-on-CRAN AirMonitorPlots

library(AirMonitorPlots)

ggplot_pm25Diurnal(scnp,
                   startdate = 20190115, 
                   enddate = 20190122,
                   offsetBreaks = TRUE) + 
  stat_meanByHour(outpu = "AQIColors") + 
  facet_grid(rows = vars(monitorID)) 

