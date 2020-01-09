library(dplyr)            # data manipulation
library(ggplot2)          # plotting
library(PWFSLSmoke)       # USFS monitor data access and plotting
library(AirSensor)        # PurpleAir sensor data access and plotting
library(AirMonitorPlots)  # Advanced plots for monitors


archiveBaseDir <- path.expand("~/Data/Australia_on_fire")
if ( !dir.exists(archiveBaseDir) ) {
  dir.create(archiveBaseDir)
}
setArchiveBaseDir(archiveBaseDir)

filePath <- file.path(archiveBaseDir, "pas_au.rda")

if ( !file.exists(filePath) ) {
  initializeMazamaSpatialUtils()
  pas_au <- pas_createNew(countryCodes = "AU", includePWFSL = TRUE)
  save(pas_au, file = filePath)
}

pas <- get(load(filePath))
pas_leaflet(pas_au)

label_au <- c("PIE Wagga", "MORUYA HEADS", "Hamilton ", "Gadd Street Air Quality", 
              "Heathcote", "Para Hills West", "Downer", "Chisholm ACT Australia")


pat_test <- pat_createNew(pas = pas_au, label = "PIE Wagga")
deviceDeploymentID <- rep(0, length(label_au))

for (i in seq(label_au)){
  deviceDeploymentID[i] <-
    pas_au %>%
    pas_filter(.data$DEVICE_LOCATIONTYPE == "outside") %>%
    pas_filter(is.na(.data$parentID)) %>%
    pas_filter(.data$countryCode == "AU") %>%
    pas_filter(.data$label == label_au[i]) %>%
    dplyr::pull(deviceDeploymentID)
  
}

ql_ids <- pas_getDeviceDeploymentIDs(pas_au, countryCodes = c("AU"))

pat_gadd <- pat_createNew(pas = pas_au, id = deviceDeploymentID[4], startdate = 20191201, enddate = 20200108)

pat_multiplot(pat_para)

pat <- pat_createNew("01309a490e3cf5b4_36471", label = NULL, pas = pas_au, startdate = 20191101, enddate = 20191231)

lon <- pat_gadd$meta$longitude
lat <- pat_gadd$meta$latitude

pas_melbourne <- 
  pas_au %>%
  pas_filterNear(longitude = lon, latitude = lat, radius = "60 km") %>%
  pas_filter(.data$DEVICE_LOCATIONTYPE == "outside")

pas_leaflet(pas_melbourne)
View(pas_melbourne)

pat_externalFit(pat_gadd)
pat_internalFit(pat_gadd)
pat_dailySoHIndexPlot(pat_gadd)

#---- Generic testing -----
setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
pas_test <- pas_load(
  datestamp = NULL,
  retries = 30,
  timezone = "America/Los_Angeles",
  archival = FALSE
)

pas_test <- pas_createNew(
  baseUrl = "https://www.purpleair.com/json",
  countryCodes = c("US"),
  includePWFSL = TRUE,
  lookbackDays = 1
)

pat_test <- pat_createNew(pas = pas_test, label =  "Alamo Square")

pas_test_au <- pas_createNew(
  baseUrl = "https://www.purpleair.com/json",
  countryCodes = c("AU"),
  includePWFSL = TRUE,
  lookbackDays = 1
)

pat_test <- pat_createNew(pas = pas_test_au, label = "PIE Wagga")


