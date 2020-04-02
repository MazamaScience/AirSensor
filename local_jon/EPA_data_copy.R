#
# This script describes how to use the AirSensor package to copy pre-generated,
# archival PurpleAir data from the USFS AirFire repository.
#
# Assumptions:
#  * user is comfortable with R, RStudio, git and GitHub
#  * user is comfortable using RStudio to build an R package from source
#  * user is comfortable using RStudio to read AirSensor package documentation
#
# Setup:
#
# 1) Use RStudio to install the PWFSLSmoke package. This should grab a bunch of
#    prerequisites.
#
# 2) Use RStuido to start a new project from: 
#      https://github.com/MazamaScience/AirSensor.git
# 
# 3) Use the RStudio Git tab to switch to the "jon" branch which has the latest 
#    code.
#
# 4) Build > Install and Restart (This may reveal missing packages which you 
#    will have to install.)
#
# Now you are ready to begin. This demonstration script should be run a few 
# lines at a time to learn how the functions work. Another script can be written
# to harvest data from multiple/all US states and multiple years.

# Required libraries
library(dplyr)
library(MazamaCoreUtils)
library(AirSensor)

# NOTE:  The S3 bucket where data are stored does not allow browsing so you have
# NOTE:  to know which files you are after. The package provides functions that
# NOTE:  automatically create the necessary URLs.

# Location of the archive data files.
setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")

# Logging utility from MazamaCoreUtils
logger.setup()
logger.setLevel(TRACE)

# ----- Synoptic data ----------------------------------------------------------

# Load pas (Purple Air Synoptic) data for the entire country
pas <- pas_load(archival = TRUE)

# NOTE:  For this example, we will only access data from Iowa

# All sensors in Iowa
IA_pas <- pas_filter(pas, stateCode == "IA")

# NOTE:  Now you can have a look at the synoptic data (aka "spatial metadata")
#
# > View(IA_pas)

# NOTE:  The IA dataframe includes separate entries for A and B channels but
# NOTE:  A and B data are stored in a single pat (Purple Air Timeseries) file.
# NOTE:  The pas_getDeviceDeploymentIDs() function applies various filters and
# NOTE:  returns a vector of deviceDeploymentIDs, each of which corresponds to a 
# NOTE:  single Purple Air Timeseries.

# Get unique timeseries identifiers ()
deviceDeploymentIDs <- 
  IA_pas %>%
  pas_getDeviceDeploymentIDs()

# ----- Downloading timeseries data --------------------------------------------

# Separate functions exist to load timeseris data for a deviceDeploymentID
# * pat_loadLatest() -- latest 7 days of data
# * pat_loadMonth() -- data for a particular YYYYMM datestamp
# * pat_load() -- automatically loads, trims and joins timeseries

# Time range of interest
startdate <- "2019-10-01"
enddate <- "2019-11-01"
timezone <- "UTC"

# Grab all data
patList = list()
for ( deviceDeploymentID in deviceDeploymentIDs ) {
  
  result <- try({
    pat <- pat_load(
      id = deviceDeploymentID,
      startdate = startdate,
      enddate = enddate,
      timezone = timezone
    )
  }, silent = TRUE)
  
  if ( "try-error" %in% class(result) ) {
    
    logger.warn("Unable to load %s", deviceDeploymentID)
    
    # NOTE:  We will see this error message quite often. It means that no data
    # NOTE:  is available for that sensor at that time for some reason.
    # NOTE:  Note that we are using a list of currently available sensors to
    # NOTE:  ask for archival data when some sensors didn't even exist. 
    # NOTE:  Reasons for no data include:
    # NOTE:   * sensor didn't exist at this time
    # NOTE:   * sensor did not report data at this time
    # NOTE:   * sensor was at a different location at this time
    # NOTE:   * something went wrong with USFS data processing (ThingSpeak request)

  } else {
    
    logger.trace("Loaded %s", deviceDeploymentID)
    
    # We have successfully load a pat object.
    
    # NOTE:  Each pat objects consists of a small amount of spatial metadata
    # NOTE:  and a large amount of data:
    #
    # > dim(pat$meta)
    # [1]  1 20
    # > dim(pat$data)
    # [1] 20761    18
    
    # We can easily output each of these as a CSV file
    OUTPUT_DIR <- "."
    
    fileName <- sprintf("201910_%s_meta.csv", deviceDeploymentID)
    filePath <- file.path(OUTPUT_DIR, fileName)
    readr::write_csv(pat$meta, path = filePath)
    
    fileName <- sprintf("201910_%s_data.csv", deviceDeploymentID)
    filePath <- file.path(OUTPUT_DIR, fileName)
    readr::write_csv(pat$data, path = filePath)
 
  } # End of pat_load() error handling
  
} # End of loop

