library(AirSensor)

# ===== PWFSL 2019 for OR, WA ==================================================

setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
pas <- pas_load()

# Find the labels of interest, only one per sensor
labels <-
  pas %>%
  pas_filter(is.na(parentID)) %>%
  pas_filter(DEVICE_LOCATIONTYPE == "outside") %>%
  #pas_filter(stateCode == "OR") %>%
  #pas_filter(stringr::str_detect(label, "^LRAPA")) %>%
  dplyr::pull(label)

# Loop over labels
start <- MazamaCoreUtils::parseDatetime("2019-11-01 00:00", timezone = "America/Los_Angeles")
end <-   MazamaCoreUtils::parseDatetime("2019-11-01 01:00", timezone = "America/Los_Angeles")
dataList <- list()
sensorList <- list()
i <- 0
for ( label in labels ) {
  
  print(paste0("Working on ", label, " ..."))
  i <- i + 1
  
  result <- try({
    pat <- 
      pat_loadLatest(label) %>%
      pat_filter(datetime >= start & datetime < end)
    
    pat_data <- pat$data
    pat_data$ID <- pat$meta$ID
    pat_data$label <- pat$meta$label
      
    dataList[[label]] <- pat_data
    sensorList[[label]] <- pat
    
  })
  # Keep chugging in the face of errors
  
  
  if ( i > 10 )
    stop
  
}

lrapa_data <- 
  dplyr::bind_rows(dataList) %>%
  dplyr::arrange(datetime, label)

earlierNames <- c("datetime", "ID", "label")
laterNames <- setdiff(names(lrapa_data), earlierNames)
allNames <- c(earlierNames, laterNames)

lrapa_data <-
  lrapa_data %>%
  dplyr::select(allNames)



