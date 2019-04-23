# Just load and look at current synoptic data

# required libraries
library(AirSensor)

# Load this morning's synoptic data
pas <- pas_load()

# -- OR -- create a new "pas" object with the latest Purple Air data 
# pas <- pas_loadLatest() # this can take 20 sec

# Filter for sensors in California
ca <-
  pas %>%
  filter(stateCode == 'CA')

# Look at different parameters
pas_leaflet(ca, parameter = "pm25_1hr")

pas_leaflet(ca, parameter = "temperature")

pas_leaflet(ca, parameter = "humidity")

pas_leaflet(ca, parameter = "pwfsl_closestDistance", maptype = "satellite")





