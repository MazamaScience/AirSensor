# ----- Setup ------------------------------------------------------------------

library(AirSensor)
AirSensor::initializeMazamaSpatialUtils()
setArchiveBaseUrl("https://airsensor.aqmd.gov/PurpleAir/v1") # SCAQMD sensors 

# ----- Load PAS ---------------------------------------------------------------

pas <- 
  pas_load(20220526, archival = TRUE) %>% 
  pas_filter(stateCode == "UT")

pas %>% pas_leaflet() # To pick a sensor

# ----- Load PAT ---------------------------------------------------------------

label <- "Emigration Canyon 1"
startdate <- 20220701
enddate <- 20220708

pat <- pat_createNew(
  label = label,
  pas = pas,
  startdate = startdate, 
  enddate = enddate
)

# ----- Examine PAT ------------------------------------------------------------

pat %>% pat_multiplot()


