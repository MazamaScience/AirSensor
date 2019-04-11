# Just load and look at current synoptic data

# required libraries
library(MazamaCoreUtils)
library(MazamaSpatialUtils)
library(PWFSLSmoke)
library(AirSensor)

initializeMazamaSpatialUtils()

# Load latest data from Purple Air -- this can take 20 seconds
pas <- pas_load()

# -- OR -- load the example data
# pas <- get(load('data/example_pas.rda'))

# Filter for sensors in California
pas_ca <-
  pas %>%
  filter(stateCode == 'CA')

# Look at different parameters
pas_leaflet(pas_ca, param = "pm25_1hr")

pas_leaflet(pas_ca, param = "temperature")

pas_leaflet(pas_ca, param = "humidity")

pas_leaflet(pas_ca, param = "pwfsl_closestDistance", maptype = "satellite")





