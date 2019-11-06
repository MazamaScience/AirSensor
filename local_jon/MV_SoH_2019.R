library(AirSensor)

# ===== PWFSL 2019 for OR, WA ==================================================

setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
pas <- pas_load()

# Find the labels of interest, only one per sensor
labels <-
  pas %>%
  pas_filter(is.na(parentID)) %>%
  pas_filter(DEVICE_LOCATIONTYPE == "outside") %>%
  pas_filter(stateCode %in% PWFSLSmoke::CONUS) %>%
  pas_filter(stringr::str_detect(label, "Home")) %>%
  dplyr::pull(label)

# Loop over labels
start <- MazamaCoreUtils::parseDatetime("2019-01-01", timezone = "America/Los_Angeles")
end <-   MazamaCoreUtils::parseDatetime("2019-11-01", timezone = "America/Los_Angeles")

sensorList <- list()
i <- 0
for ( label in labels ) {
  
  print(paste0("Working on ", label, " ..."))
  i <- i + 1
  
  result <- try({
    sensorList[[label]] <- 
      pat_load(label, start, end)
    
  })
  # Keep chugging in the face of errors
  
  if ( i > 10 )
    stop
  
}

# Apply dailyPctReporting metrics to each pat
pctReportingList <- purrr::map(sensorList, PurpleAirSoH_dailyPctReporting)

# # Create a list whose names are SoH metrics and whose elements are tibbles of
# # data with column names matching the sensor label.
# sensor_by_metric <- purrr::transpose(pctReportingList)
# 
# # Create a tidy tibble for a single metric
# a <- purrr::map(pctReportingList, dplyr::select, "datetime", "pm25_A_pctReporting")

# Add the sensor label to each dataframe
a <- pctReportingList
for ( name in names(a) ) {
  a[[name]]$label <- 
    stringr::str_replace(name, ".*\\@", "") %>%
    stringr::str_trim()
}

# Create a tidy tibble for a single metric
b <- purrr::map(a, dplyr::select, "datetime", "pm25_A_pctReporting", "label")

# Combein data from multiple sensors
c <- dplyr::bind_rows(b)

# Plot

library(ggplot2)

gg <- 
  ggplot(data = c, aes(datetime, pm25_A_pctReporting)) + 
  ylim(0,150) +
  geom_hline(yintercept = 100, color = "salmon", size = 0.5) +
  geom_point(shape = "square", size = 0.8)

gg + 
  facet_wrap(~label, ncol = 1) +
  ggtitle("Methow Valley Celan Air Ambassador -- Channel A % Reporting")




