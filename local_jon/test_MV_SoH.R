#
# Test SoH calculations on latest data for Methow Valley sensors.
#

library(AirSensor)

setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
pas <- pas_load()

# Get various text identifiers
deviceDeploymentIDs <- pas_getDeviceDeploymentIDs(pas, pattern = "MV Clean Air Ambassador")
labels <- pas_getLabels(pas, pattern = "MV Clean Air Ambassador")
siteNames <-
  labels %>%
  stringr::str_replace(".*\\@", "") %>%
  stringr::str_trim()

# Load pat objects into a list
patList <- list()
for ( deviceDeploymentID in deviceDeploymentIDs ) {
  
  print(sprintf("Loading %s", deviceDeploymentID))
  
  result <- try({
    patList[[deviceDeploymentID]] <- 
      pat_loadLatest(deviceDeploymentID, days = 45) %>%
      pat_trimDate()
  })
  # Keep chugging in the face of errors
  
}

# Apply dailyPctReporting metrics to each pat
pctReportingList <- purrr::map(patList, PurpleAirSoH_dailyPctReporting)

# Add the siteName to each dataframe
i <- 0
for ( deviceDeploymentID in deviceDeploymentIDs ) {
  i <- i + 1
  pctReportingList[[deviceDeploymentID]]$siteName <- siteNames[i]
}

# Combine data from multiple sensors
pctReportingTbl <- dplyr::bind_rows(pctReportingList)

# Plot

library(ggplot2)

gg <- 
  ggplot(data = pctReportingTbl, aes(datetime, pm25_A_pctReporting)) + 
  ylim(0,150) +
  geom_hline(yintercept = 100, color = "salmon", size = 0.5) +
  geom_point(shape = "square", size = 0.8)

gg + 
  facet_wrap(~siteName, ncol = 2) +
  ggtitle("Methow Valley Celan Air Ambassador -- Channel A % Reporting")




