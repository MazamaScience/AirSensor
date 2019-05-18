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
  
# Load a list of PAT objects
patList <- 
  purrr::map(scnp_labels, pat_loadMonth, "201901")

# Create a list of AST objects
astList <-
  purrr::map(patList, pat_createASTimeseries, "1 hour")

# Create a list of AS objects
asList <- 
  purrr::map(astList, ast_createAirSensor, "pm25_A_mean")

# Now we can use PWFSLSmoke functions
scnp <- monitor_combine(asList)

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
  purrr::map(pat_createASTimeseries, "1 hour") %>%
  purrr::map(ast_createAirSensor, "pm25_A_mean") %>%
  monitor_combine()

# Working with not-on-CRAN AirMonitorPlots

library(AirMonitorPlots)

ggplot_pm25Diurnal(scnp,
                   startdate = 20190115, 
                   enddate = 20190122,
                   offsetBreaks = TRUE) + 
  stat_meanByHour(outpu = "AQIColors") + 
  facet_grid(rows = vars(monitorID)) 

