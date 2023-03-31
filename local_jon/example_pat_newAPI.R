# Examples for the ASIC conference

library(AirSensor)

initializeMazamaSpatialUtils("~/Data/Spatial")

source("global_vars.R")
setAPIKey("PurpleAir-read", PURPLE_AIR_API_READ_KEY)

# 9392 shows good qualities
pat <-
  pat_createNew(
    api_key = PURPLE_AIR_API_READ_KEY,
    pas = example_pas,
    sensor_index = "9392",
    startdate = "2022-07-01",
    enddate = "2022-07-08",
    timezone = "UTC",
    verbose = TRUE
  )

pat %>% pat_multiplot("pm25_over")


