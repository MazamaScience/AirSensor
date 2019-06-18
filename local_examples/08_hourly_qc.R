# required libraries
library(AirSensor)
library(PWFSLSmoke)

# Load today's PAS object
pas <- pas_load()

# sctv labels
sctv_labels <-
  pas %>%
  dplyr::filter(stringr::str_detect(label, "^[Ss][Cc][Tt][Vv]_..$")) %>%
  pull(label)
  
# Load a list of PAT objects
patList <- 
  purrr::map(sctv_labels, pat_loadMonth, "201906")

# Create a list of AST objects
astList <-
  purrr::map(patList, pat_createASTimeseries, "1 hour")

# # Create a list of AS objects
# asList <- 
#   purrr::map(astList, ast_createAirSensor, "pm25_A_mean")
# 
# # Now we can use PWFSLSmoke functions
# sctv <- monitor_combine(asList)
# 
# layout(matrix(seq(length(sctv_labels))))
# for ( monitorID in sctv$meta$monitorID ) {
#   monitor_dailyBarplot(sctv, monitorID)
# }
# 
# # NOTE:  We could have created the sctv object this in one fell swoop
# sctv <-
#   pas %>%
#   dplyr::filter(stringr::str_detect(label, "^[Ss][Cc][Nn][Pp]_..$")) %>%
#   pull(label) %>%
#   purrr::map(pat_loadMonth, "201901") %>%
#   purrr::map(pat_createASTimeseries, "1 hour") %>%
#   purrr::map(ast_createAirSensor, "pm25_A_mean") %>%
#   monitor_combine()
# 
# # Working with not-on-CRAN AirMonitorPlots
# 
# library(AirMonitorPlots)
# 
# ggplot_pm25Diurnal(sctv,
#                    startdate = 20190115, 
#                    enddate = 20190122,
#                    offsetBreaks = TRUE) + 
#   stat_meanByHour(outpu = "AQIColors") + 
#   facet_grid(rows = vars(monitorID)) 
# 
