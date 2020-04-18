library(MazamaCoreUtils)
library(AirSensor)
library(dplyr)
library(ggplot2)

pas <- AirSensor::example_pas
pat <- AirSensor::example_pat

id_seattle <- 
  pas %>%
  pas_filter(label == "Seattle") %>%
  dplyr::pull(deviceDeploymentID)

id_home <- 
  pas %>%
  pas_filter(label == "home") %>%
  dplyr::pull(deviceDeploymentID)

id <- pas_getDeviceDeploymentIDs(pas = pas, pattern = "Seattle")
id <- pas_getDeviceDeploymentIDs(pas, "^Seattle$")
id_home <- pas_getDeviceDeploymentIDs(pas, "^Home$")

pat_test <- pat_createNew(id = id, pas = pas, startdate = 20180701, enddate = 20180901)
  
pat <- pat_createNew(label = "Seattle", pas = pas, startdate = 20180701, enddate = 20180901)


## purpleair_failure_modes

setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
#setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
pas <- pas_load(archival = TRUE)
pas <- example_pas
noisy_2_id <- pas_getDeviceDeploymentIDs(pas, pattern = "SCAP_14")
pat_a_noisy_2 <- pat_createNew(pas, 
                               id = noisy_2_id,
                               label = "SCAP_14", 
                               startdate = 20190701, 
                               enddate = 20190708,
                               timezone = "America/Los_Angeles")
humidity_id <- pas_getDeviceDeploymentIDs(pas, 
                                          pattern = "BikeSGV - West Pasadena")

id <- pas_getDeviceDeploymentIDs(pas, pattern = "SCEM_05")


zero_id <- pas_getDeviceDeploymentIDs(pas, pattern = "SCAP_46")

# Australia 

archiveBaseDir <- path.expand("~/Data/Australia_on_fire")
if ( !dir.exists(archiveBaseDir) ) {
  dir.create(archiveBaseDir)
}

setArchiveBaseDir(archiveBaseDir)

filePath_pas <- file.path(archiveBaseDir, "pas_au.rda")

if ( !file.exists(filePath_pas) ) {
  initializeMazamaSpatialUtils()
  pas_au <- pas_createNew(countryCodes = "AU", includePWFSL = TRUE)
  save(pas_au, file = filePath_pas)
}

pas <- get(load(filePath_pas))


jannali_id <- pas_getDeviceDeploymentIDs(pas, pattern = "Jannali")


unhealthy_labels <- c("Chisholm ACT Australia", 
                      "MORUYA HEADS",
                      "Windang, Ocean Street")

patList <- list()
for ( label in unhealthy_labels ) {
  id <- pas_getDeviceDeploymentIDs(pas, pattern = label)
  patList[[label]] <- pat_createNew(id = id, 
                                    label = label,
                                    pas = pas_au, 
                                    startdate = 20191210, 
                                    enddate = 20200110) 
}








