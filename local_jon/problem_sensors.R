


# Below is a list of sensors and timeframes I was using to explore the results of 
# applying different QC algorithms. I was primarily focused on examples where we 
# see moderate to severe sensor noise from one or both channels (as opposed to more 
# extreme/obvious sensor malfunctions, such as one channel reading zero).
# 
# Sensor: O “SCNP_17”, startdate = 20200501, enddate = 20200520
# Sensor: ? “CARB_SMOKE_SLO_CDF”, startdate = 20200501, enddate = 20200520
# Sensor: O “1105 Trail View Place, Nipomo, CA”, startdate = 20200501, enddate = 20200520
# Sensor: O "CARB_Smoke_SBCAPCD_Santa Maria", startdate = 20200501, enddate = 20200520
# Sensor: O "SCAP_19", startdate = 20200501, enddate = 20200520
# Sensor: O "SCTV_31", startdate = 20200501, enddate = 20200520
# Sensor: X "RUSD_1 (Arlington HS)", startdate = 20200516, enddate = 20200521
# 
# GitHub IO examples:
#   Sensor: “SCNP_20”, startdate = 20190411, enddate = 20190521
# Sensor: "SCAP_46", startdate = 20190701, enddate = 20190710
# 
# GitHub IO example (Australian wildfire):
#   Sensor: “Downer”, startdate = 20191225, enddate = 20200110

# ----- Setup ------------------------------------------------------------------

library(AirSensor)
AirSensor::initializeMazamaSpatialUtils()
setArchiveBaseUrl("https://airsensor.aqmd.gov/PurpleAir/v1") # SCAQMD sensors 

pas <- pas_load(20190901, archival = TRUE)
pas <- pas_createNew(countryCodes = "AU", includePWFSL = FALSE)

# ----- Load PAT ---------------------------------------------------------------

label <- "Downer"
startdate <- 20191225
enddate <- 20200110

pat <- pat_createNew(
  label = label,
  pas = pas,
  startdate = startdate, 
  enddate = enddate
)

# ----- Examine PAT ------------------------------------------------------------

pat %>% pat_multiplot()


