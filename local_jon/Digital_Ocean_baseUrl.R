# Using Digital Ocean data archive

library(AirSensor)

setArchiveBaseUrl("http://165.232.140.3/PurpleAir/v1")
pas <- pas_load()
pas %>% pas_filter(stateCode == "CA") %>% pas_leaflet()
