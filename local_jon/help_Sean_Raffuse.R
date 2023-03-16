library(AirSensor)
library(dplyr)

setArchiveBaseUrl("https://airsensor.aqmd.gov/PurpleAir/v1")

pas <- 
  pas_load("20190406", archival = TRUE) %>%
  pas_filter(stateCode == "CA")

CA_outdoor_ids <- 
  pas %>%
  pas_getDeviceDeploymentIDs(
    isOutside = TRUE
  )

pat <- 
  pat_createNew(
    id = CA_outdoor_ids[1], 
    pas = pas, 
    startdate = 20190406, 
    enddate = 20190410,
    verbose = TRUE
  )

pat %>% pat_multiplot()
