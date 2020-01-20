library(MazamaCoreUtils)
library(AirSensor)
library(dplyr)
library(ggplot2)

pas <- AirSensor::example_pas
pat <- AirSensor::example_pat

id <- 
  pas %>%
  pas_filter(label == "Seattle") %>%
  dplyr::pull(deviceDeploymentID)

id_home <- 
  pas %>%
  pas_filter(label == "home") %>%
  dplyr::pull(deviceDeploymentID)

pat <- pat_createNew(id = id, pas = pas, startdate = 20180701, enddate = 20180901)
  
pat <- pat_createNew(label = "Seattle", pas = pas, startdate = 20180701, enddate = 20180901)
