#
# This is for defining global variables and imports for the AirShiny web 
# application. You can run the application by clicking 'Run App' above.
#
# Find out more about Shiny applications here: 
#
#    http://shiny.rstudio.com/
#
# - Mazama Science
#

library(AirSensor)
library(MazamaCoreUtils)

PAS <- AirSensor::pas_load()

PAS_COMM <- na.omit(unique(PAS$communityRegion)) 
