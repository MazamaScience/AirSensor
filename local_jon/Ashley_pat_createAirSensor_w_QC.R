library(AirSensor)
setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1/")
pas <- pas_load()

id <- pas_getDeviceDeploymentIDs(pas, "SCAP_50")
pat <- pat_load(id = id, pas = pas, label = "SCAP_50", startdate = 20200701, enddate = 20200715, timezone = "America/Los_Angeles")
pat_multiPlot(pat)
pat_externalFit(pat) # fails here
pat_externalFit(pat, qc_algorithm = "hourly_AB_00") # fails here
pat_externalFit(pat, qc_algorithm = "hourly_AB_02") # fails here

sensor <- pat_createAirSensor(pat = pat, FUN = PurpleAirQC_hourly_AB_00) # does not fail here
sensor <- pat_createAirSensor(pat = pat, FUN = PurpleAirQC_hourly_AB_01) # fails here
sensor <- pat_createAirSensor(pat = pat, FUN = PurpleAirQC_hourly_AB_02) # does not fail here
