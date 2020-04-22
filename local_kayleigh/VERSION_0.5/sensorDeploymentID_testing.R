library(AirSensor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)
library(purrr)
library(skimr)
library(MazamaLocationUtils)
library(AirSensor)

setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")

pas <- pas_load()

locationID <- MazamaLocationUtils::location_createID(pas$longitude, pas$latitude)
pat_fileID <- paste0(locationID, "_", pas$ID)

# pas_getColumn


