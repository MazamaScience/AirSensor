library(MazamaCoreUtils)
library(PWFSLSmoke)
library(AirSensor)

####### Load then filter the pas which contains the meta for all the sensors

setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")

logger.setup()
logger.setLevel(TRACE)

state_pas <- 
  pas_load(archival = TRUE) %>% 
  pas_filter(stateCode == "CA") #filter by CA

deviceDeploymentIDs <- pas_getDeviceDeploymentIDs(state_pas) # grab unique space/time sensor identification

####### Load the pats in a list. Each pat is labeled with with device deployment ID and contains time series and meta

patList <- list()
patEmptyList <- list()
DATESTAMP <- "201908"


count <- 0
for ( id in deviceDeploymentIDs ) {
  
  count <- count + 1
  logger.trace("%04d/%d -- pat_loadMonth(%s)", count, length(deviceDeploymentIDs), id)
  
  # Load January data and trim the date so we don't statistics for partial days
  pat <- 
    pat_loadMonth(id, datestamp = DATESTAMP) 
  
  # Can only trimDate if it isn't empty
  if ( !pat_isEmpty(pat) )
    pat <- pat_trimDate(pat)
  
  if ( pat_isEmpty(pat) ) {
    # TODO fix pat_isEmpty so it checks pat$data, not pat$meta
    patEmptyList[[id]] <- pat
  } else {
    patList[[id]] <- pat
  }
  
}

logger.trace("%d pats with data, %d without", length(patList), length(patEmptyList))

