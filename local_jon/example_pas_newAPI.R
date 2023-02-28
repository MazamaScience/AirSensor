# Examples for the ASIC conference

library(AirSensor2)

initializeMazamaSpatialUtils("~/Data/Spatial")

source("global_vars.R")
setAPIKey("PurpleAir-read", PA_API_READ_KEY)

# ----- Colville ---------------------------------------------------------------

Colville_pas <-
  pas_createNew(
    countryCodes = "US",
    stateCodes = "WA",
    counties = c("Okanogan", "Ferry"),
    lookbackDays = 1,
    outsideOnly = TRUE
  )

Colville_pas %>% pas_leaflet("temperature")

# ----- Australia --------------------------------------------------------------

AU_pas <-
  pas_createNew(
    countryCodes = "AU",
    lookbackDays = 1,
    outsideOnly = TRUE
  )

AU_pas %>% pas_leaflet("pm2.5_1week")

AU_pas %>% pas_leaflet("pm2.5_24hour")

AU_pas %>% pas_leaflet("pm2.5_60minute")

# ----- Pasadena ---------------------------------------------------------------

LA_pas <-
  pas_createNew(
    countryCodes = "US",
    stateCodes = "CA",
    counties = c("Los Angeles"),
    lookbackDays = 1,
    outsideOnly = TRUE
  )

LA_pas %>% pas_leaflet("pm2.5_24hour")

Pasadena_pas <-
  LA_pas %>%
  pas_filterNear(-118.131944, 34.156111, radius = "5 km")

Pasadena_pas %>% pas_leaflet("pm2.5_24hour")

Pasadena_pas %>% pas_leaflet("elevation")


# ----- California -------------------------------------------------------------

CA_pas <-
  pas_createNew(
    countryCodes = "US",
    stateCodes = "CA",
    lookbackDays = 1,
    outsideOnly = TRUE
  )

CA_pas %>% pas_leaflet("pm2.5_60minute")

CA_pas %>% pas_leaflet("elevation")






