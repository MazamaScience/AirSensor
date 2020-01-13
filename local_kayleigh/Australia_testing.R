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

label <- c("Gadd Street Air Quality")

deviceDeploymentID <- pas_getDeviceDeploymentIDs(pas = pas, 
                                                 countryCodes = c("AU"), 
                                                 pattern = label)

testdeviceID <- pas_getDeviceDeploymentIDs(pas = pas, 
                                           countryCodes = c("AU"), 
                                           pattern = "Werribbee Street North Park")

pat_gadd <- pat_createNew(pas = pas, id = deviceDeploymentID, 
                          startdate = 20191115, enddate = 20200108)

pat_test <- pat_createNew(pas = pas, id = testdeviceID, 
                          startdate = 20191115, enddate = 20200108)

pat_multiplot(pat_test)


lon <- pat_gadd$meta$longitude
lat <- pat_gadd$meta$latitude

pas_melbourne <- 
  pas_au %>%
  pas_filterNear(longitude = lon, latitude = lat, radius = "60 km") 

pas_leaflet(pas_melbourne)

labels <- pas_getLabels(pas_melbourne, countryCodes = c("AU"))
deviceDeploymentIDs <- pas_getDeviceDeploymentIDs(pas_melbourne, countryCodes = c("AU"))
IDsubset <- deviceDeploymentIDs[1:3]

patList <- list()
for ( id in IDsubset ) { 
  patList[[id]] <- pat_createNew(id, pas = pas_melbourne, startdate = 20191225, enddate = 20200108) }

pat_beacon <- patList[[1]]
pat_werribee <- patList[[2]]
pat_donvale <- patList[[3]]

pat_multiplot(pat_donvale)


 pat_externalFit(pat_gadd)
pat_internalFit(pat_gadd)
pat_dailySoHIndexPlot(pat_gadd)

# deviceDeploymentID <- list()
# for (i in seq(label_au)){
#   deviceDeploymentID[i] <- pas_getDeviceDeploymentIDs(pas = pas_au, countryCodes = c("AU"), pattern = label_au[i])
# }

# deviceDeploymentID <- rep(0, length(label_au))
# 
# for (i in seq(label_au)){
#   deviceDeploymentID[i] <-
#     pas_au %>%
#     pas_filter(.data$DEVICE_LOCATIONTYPE == "outside") %>%
#     pas_filter(is.na(.data$parentID)) %>%
#     pas_filter(.data$countryCode == "AU") %>%
#     pas_filter(.data$label == label_au[i]) %>%
#     dplyr::pull(deviceDeploymentID)
# }

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

pat_test <- pat_createNew(pas = pas_au, label = "PIE Wagga")


