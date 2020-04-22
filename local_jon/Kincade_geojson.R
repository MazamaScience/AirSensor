# Kincade fire geojson file

library(dplyr)
library(MazamaCoreUtils)
library(AirSensor)

setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")

logger.setup()
logger.setLevel(TRACE)

# Load synoptic data
pas <- pas_load()

# Kincade fire area
Kincade_pas <- pas_filterArea(pas, -124, -120, 36, 40)

# Quick view
###pas_leaflet(Kincade_pas)

# Get timeseries identifiers
deviceDeploymentIDs <- 
  Kincade_pas %>%
  pas_getDeviceDeploymentIDs()

# Set the times
startdate <- "2019-10-20"
enddate <- "2019-10-30"
datetime <- MazamaCoreUtils::parseDatetime(
  "2019-10-24 12:00",
  timezone = "America/Los_Angeles"
)

# Grab all data
recordList = list()
for ( deviceDeploymentID in deviceDeploymentIDs ) {
  
  result <- try({
    pat <- pat_load(
      id = deviceDeploymentID,
      startdate = startdate,
      enddate = enddate
    )
  }, silent = TRUE)
  
  if ( "try-error" %in% class(result) ) {
    
    logger.warn("Unable to load %s", deviceDeploymentID)
    
  } else {
    
    logger.trace("Loaded %s", deviceDeploymentID)
    
    result <- try({
      sensor <- pat_createAirSensor(pat)
    }, silent = TRUE)
    
    if ( "try-error" %in% class(result) ) {
      
      logger.warn("Error with %s: %s", deviceDeploymentID, geterrmessage())
      
    } else {
      
      recordList[[deviceDeploymentID]] <- tibble(
        longitude = sensor$meta$longitude,
        latitude = sensor$meta$latitude,
        pm25 = 
          sensor$data %>%
          filter(datetime == !!datetime) %>%
          pull(deviceDeploymentID) %>%
          round(1)
      )
      
    }
    
  } # End of record
  
} # End of loop

# Combine all records
tbl <- dplyr::bind_rows(recordList)

# Write out geojson
geojsonio::geojson_write(
  tbl,
  file = "Kincade_20191024_1200.geojson",
  precision = 6
)




