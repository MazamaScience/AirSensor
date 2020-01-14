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

pas_filePath <- file.path(archiveBaseDir, "pas_au.rda")

wind_filePath <- file.path(archiveBaseDir, "canberra_wind.rda")

if ( !file.exists(pas_filePath) ) {
  initializeMazamaSpatialUtils()
  pas_au <- pas_createNew(countryCodes = "AU", includePWFSL = TRUE)
  save(pas_au, file = pas_filePath)
}

canberra_wind <- get(load(wind_filePath))
pas <- get(load(pas_filePath))


pas_leaflet(pas_au)

# label_au <- c("PIE Wagga", "MORUYA HEADS", "Hamilton ", "Gadd Street Air Quality", 
#               "Heathcote", "Para Hills West", "Downer", "Chisholm ACT Australia")


# pat_test <- pat_createNew(pas = pas_au, label = "PIE Wagga")

label <- c("Jannali" )

deviceDeploymentID <- pas_getDeviceDeploymentIDs(pas = pas, 
                                                 countryCodes = c("AU"), 
                                                 pattern = label)

# testdeviceID <- pas_getDeviceDeploymentIDs(pas = pas, 
#                                            countryCodes = c("AU"), 
#                                            pattern = "Windang, Ocean Street")

pat_jannali <- pat_createNew(pas = pas, id = deviceDeploymentID, 
                             startdate = 20191229, enddate = 20200110)
filename <- c("pat_jannali.rda")
filepath <- file.path("/Users/kayleigh/Data/Australia_on_fire/", filename)
save(list = "pat_jannali", file = filepath)


jannali_filePath <- file.path(archiveBaseDir, "pat_jannali.rda")
if ( file.exists(jannali_filePath) ) {
  pat_jannali <- get(load(jannali_filePath))
} else {
  pat_jannali <- pat_createNew(pas = pas, id = deviceDeploymentID, 
                               startdate = 20191229, enddate = 20200110)
}


# pat_windang <- pat_createNew(pas = pas, id = testdeviceID, 
#                           startdate = 20191210, enddate = 20200110)

pat_multiplot(pat_jannali)


lon <- pat_jannali$meta$longitude
lat <- pat_jannali$meta$latitude

#### explore all the sensors in sydney
pas_sydney <- 
  pas_au %>%
  pas_filterNear(longitude = lon, latitude = lat, radius = "50 km") 

pas_leaflet(pas_sydney)

### sydney doesn't look great but where are the worst stations located? Canberra has it rough
pas_v_unhealthy <- 
  pas_au%>%
  pas_filter(pm25_1hr > 150)

pas_leaflet(pas_v_unhealthy)

### if you wanted you could download all the pat's for canberra but lets just look at a few with the longest data history
filePath_chisholm <- file.path(archiveBaseDir, "pat_chisholm.rda")
if ( file.exists(filePath_chisholm) ) {
  pat_chisholm <- get(load(filePath_chisholm))
} 

filePath_moruya <- file.path(archiveBaseDir, "pat_moruya.rda")
if ( file.exists(filePath_moruya) ) {
  pat_moruya <- get(load(filePath_moruya))
} 

filePath_windang <- file.path(archiveBaseDir, "pat_windang.rda")
if ( file.exists(filePath_windang) ) {
  pat_moruya <- get(load(filePath_windang))
} else {
  
  labels <- pas_getLabels(pas_v_unhealthy, countryCodes = c("AU"))
  deviceDeploymentIDs <- pas_getDeviceDeploymentIDs(pas_v_unhealthy, countryCodes = c("AU"))
  unhealthy_IDsubset <- deviceDeploymentIDs[c(3, 5, 6)]
  
  patList <- list()
  for ( id in unhealthy_IDsubset ) {
    patList[[id]] <- pat_createNew(id, pas = pas_v_unhealthy, startdate = 20191210, enddate = 20200110) 
  }
  
  pat_chisholm <- patList[[1]]
  pat_moruya <- patList[[2]]
  pat_windang <- patList[[3]]
}


# labels <- pas_getLabels(pas_v_unhealthy, countryCodes = c("AU"))
# deviceDeploymentIDs <- pas_getDeviceDeploymentIDs(pas_v_unhealthy, countryCodes = c("AU"))
# unhealthy_IDsubset <- deviceDeploymentIDs[c(3, 5, 6)]
# 
# patList <- list()
# for ( id in unhealthy_IDsubset ) {
#   patList[[id]] <- pat_createNew(id, pas = pas_v_unhealthy, startdate = 20191210, enddate = 20200110) 
# }
# 
# pat_chisholm <- patList[[1]]
# pat_moruya <- patList[[2]]
# pat_windang <- patList[[3]]
#Saving pat's so they can be downloaded more easily
filename <- c("pat_chisholm.rda")
filepath <- file.path("/Users/kayleigh/Data/Australia_on_fire/", filename)
save(list = "pat_chisholm", file = filepath)
filename <- c("pat_moruya.rda")
filepath <- file.path("/Users/kayleigh/Data/Australia_on_fire/", filename)
save(list = "pat_moruya", file = filepath)
filename <- c("pat_windang.rda")
filepath <- file.path("/Users/kayleigh/Data/Australia_on_fire/", filename)
save(list = "pat_windang", file = filepath)


colors <- c("Chisholm" = "#1b9e77", "Moruya" = "#d95f02", "Windang" = "#7570b3")
gg <- ggplot(data = pat_chisholm$data) +
  geom_point(aes(x = pat_chisholm$data$datetime, y = pat_chisholm$data$pm25_A, 
                 color = "Chisholm"), alpha = 0.5) +
  geom_point(data = pat_moruya$data, 
             aes(x = pat_moruya$data$datetime, y = pat_moruya$data$pm25_A,
                 color = "Moruya"), alpha = 0.5) +
  geom_point(data = pat_windang$data, 
             aes(x = pat_windang$data$datetime, y = pat_windang$data$pm25_A, 
                 color = "Windang"), alpha = 0.5) +
  labs(title = "PM 2.5 channel A for multiple sensors" ) +
  xlab("date") +
  ylab("ug/m3") +
  scale_colour_manual(name="Sensor",values=colors) +
  theme(legend.position= c(0.9, 0.8))

gg
### interesting that two of these three sensors report real looking data out of spec. T
### these three are spread all over Canberra, what if we look at three right next to each other
filePath_bungendore <- file.path(archiveBaseDir, "pat_bungendore.rda")
if ( file.exists(filePath_bungendore) ) {
  pat_bungendore <- get(load(filePath_bungendore))
} 

filePath_downer <- file.path(archiveBaseDir, "pat_downer.rda")
if ( file.exists(filePath_downer) ) {
  pat_downer <- get(load(filePath_downer))
} else {
  proximity_IDsubset <- deviceDeploymentIDs[c(2, 4)]
  
  pat_bungendore <- pat_createNew(id = proximity_IDsubset[1], pas = pas_v_unhealthy,
                                  startdate = 20191229, enddate = 20200110)
  
  pat_downer <- pat_createNew(id = proximity_IDsubset[2], pas = pas_v_unhealthy,
                              startdate = 20191220, enddate = 20200110)
}
# proximity_IDsubset <- deviceDeploymentIDs[c(2, 4)]
# 
# pat_bungendore <- pat_createNew(id = proximity_IDsubset[1], pas = pas_v_unhealthy,
#                                 startdate = 20191229, enddate = 20200110)
# 
# 
# pat_downer <- pat_createNew(id = proximity_IDsubset[2], pas = pas_v_unhealthy,
#                             startdate = 20191220, enddate = 20200110)

#Saving pat's so they can be downloaded more easily
filename <- c("pat_downer.rda")
filepath <- file.path("/Users/kayleigh/Data/Australia_on_fire/", filename)
save(list = "pat_downer", file = filepath)
filename <- c("pat_bungendore.rda")
filepath <- file.path("/Users/kayleigh/Data/Australia_on_fire/", filename)
save(list = "pat_bungendore", file = filepath)

colors <- c("Chisholm" = "#1b9e77", "Downer" = "#f1a340", "Bungendore" = "#998ec3")
gg <- ggplot(data = pat_chisholm$data) +
  geom_point(aes(x = pat_chisholm$data$datetime, y = pat_chisholm$data$pm25_A, 
                 color = "Chisholm"), alpha = 0.5) +
  geom_point(data = pat_downer$data, 
             aes(x = pat_downer$data$datetime, y = pat_downer$data$pm25_A,
                 color = "Downer"), alpha = 0.5) +
  geom_point(data = pat_bungendore$data, 
             aes(x = pat_bungendore$data$datetime, y = pat_bungendore$data$pm25_A, 
                 color = "Bungendore"), alpha = 0.5) +
  labs(title = "PM 2.5 channel A for multiple sensors" ) +
  xlab("date") +
  ylab("ug/m3") +
  scale_colour_manual(name="Sensor",values=colors) +
  theme(legend.position= c(0.9, 0.8))

gg

### interesting that all three of these stations record out of spec at the same time
### To figure out if these are real, let's look at the state of health of the sensor

pat_dailySoHIndexPlot(pat_chisholm)

### both channels are reporting good health so lets look a little deeper. Not really sure
### what to make of these values, perhaps the smoke is so bad that the sensors don't
### know how to report the values but three sensors in close proximity that report 
### out of spec values at the same time is compelling nevertheless.

### to get a better look at the data, lets conver the data into an airsensor object
airsensor_chisholm <- pat_createAirSensor(
  pat = pat_chisholm,
  period = "1 hour",
  parameter = "pm25",
  channel = "a",
  qc_algorithm = "hourly_AB_01",
  min_count = 20,
  aggregation_FUN = pat_aggregate
)

PWFSLSmoke::monitor_dailyBarplot(airsensor_chisholm)







canberra_labels <- c("Chisholm ACT Australia", "Bungendore, NSW Australia", "Downer")

pas_canberra <- 
  pas_au%>%
  pas_filterNear(latitude = pat_chisholm$meta$latitude, longitude = pat_chisholm$meta$longitude, radius = "40 km")
lon <- pat_chisholm$meta$longitude
lat <- pat_chisholm$meta$latitude

closestSite <- worldmet::getMeta(lon = lon, lat = lat, n = 1, 
                                 plot = FALSE)[1,]

m <- pas_leaflet(pas_canberra)


leaf <- m %>%
  leaflet::addCircleMarkers(lng = closestSite$longitude, lat = closestSite$latitude)
leaf

sensor_pollutionRose(airsensor_chisholm, canberra_wind, statistic = "prop.mean")


# 
# lats <- c(pat_chisholm$meta$latitude, pat_bungendore$meta$latitude, pat_downer$meta$latitude, wind2020_orig$LATITUDE)
# lons <- c(pat_chisholm$meta$longitude, pat_bungendore$meta$longitude, pat_downer$meta$longitude, wind2020_orig$LONGITUDE)
#The raw recordings are stored
### in the "pm25_atm_*" channels. The sensor converts the particle counts into what it considers
##
### if 
pat_scatterplot(pat_chisholm, parameters = c("datetime", "pm25_A", "pm25_atm_A"))
###



id_heathcote <- deviceDeploymentIDs[8]
id_glen <- deviceDeploymentIDs[5]


pat_heathcote <- pat_createNew(id_heathcote, pas = pas_sydney, 
                               startdate = 20191201, enddate = 20200110) 
pat_glen <- pat_createNew(id_glen, pas = pas_sydney, 
                          startdate = 20191201, enddate = 20200110) 
pat_ormond <- patList[[1]]
pat_werribee <- patList[[2]]


pat_multiplot(pat_donvale)


pat_externalFit(pat_gadd)
pat_internalFit(pat_gadd)
pat_dailySoHIndexPlot(pat_gadd)


# patList <- list()
# for ( id in IDsubset ) { 
#   patList[[id]] <- pat_createNew(id, pas = pas_melbourne, startdate = 20191225, enddate = 20200108) }
# 
# pat_ormond <- patList[[1]]
# pat_werribee <- patList[[2]]

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


