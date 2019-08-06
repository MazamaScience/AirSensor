#!/usr/local/bin/Rscript

# This Rscript will process archived 'pat' data files into a single 'airsensor'
# file containing hourly data for all sensors.
#
# Test this script from the command line with:
#
# ./createLatestAirSensor_exec.R
#
# Run it inside a docker continer with something like:
#
# docker run --rm -v /Users/jonathan/Projects/MazamaScience/AirSensor/local_executables:/app -w /app mazamascience/airsensor /app/createLatestAirSensor_exec.R --pattern=^SCNP_..$
#

#  --- . --- . AirSensor 0.3.12
VERSION = "0.3.6" 

library(optparse)      # to parse command line flags

# The following packages are attached here so they show up in the sessionInfo
suppressPackageStartupMessages({
  library(futile.logger)
  library(MazamaCoreUtils)
  library(AirSensor)
})

# ----- Get command line arguments ---------------------------------------------

if ( interactive() ) {
  
  # RStudio session
  opt <- list(
    outputDir = getwd(),
    logDir = getwd(),
    datestamp = "",
    timezone = "America/Los_Angeles",
    pattern = "^SCNP_..$"
  )  
  
} else {
  
  # Set up OptionParser
  library(optparse)
  
  option_list <- list(
    make_option(
      c("-o","--outputDir"), 
      default=getwd(), 
      help="Output directory for generated .RData files [default=\"%default\"]"
    ),
    make_option(
      c("-l","--logDir"), 
      default=getwd(), 
      help="Output directory for generated .log file [default=\"%default\"]"
    ),
    make_option(
      c("-p","--pattern"), 
      default="^[Ss][Cc].._..$", 
      help="String pattern passed to stringr::str_detect  [default=\"%default\"]"
    ),
    make_option(
      c("-V","--version"), 
      action="store_true", 
      default=FALSE, 
      help="Print out version number [default=\"%default\"]"
    )
  )
  
  # Parse arguments
  opt <- parse_args(OptionParser(option_list=option_list))
  
}

# Print out version and quit
if ( opt$version ) {
  cat(paste0("createLatestAirSensor_exec.R ",VERSION,"\n"))
  quit()
}

# ----- Validate parameters ----------------------------------------------------

if ( !dir.exists(opt$outputDir) ) 
  stop(paste0("outputDir not found:  ",opt$outputDir))

if ( !dir.exists(opt$logDir) ) 
  stop(paste0("logDir not found:  ",opt$logDir))

# ----- Set up logging ---------------------------------------------------------

logger.setup(
  traceLog = file.path(opt$logDir, paste0("createLatestAirSensor_TRACE.log")),
  debugLog = file.path(opt$logDir, paste0("createLatestAirSensor_DEBUG.log")), 
  infoLog  = file.path(opt$logDir, paste0("createLatestAirSensor_INFO.log")),
  errorLog = file.path(opt$logDir, paste0("createLatestAirSensor_ERROR.log"))
)

# For use at the very end
errorLog <- file.path(opt$logDir, paste0("createLatestAirSensor_ERROR.log"))

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

# Start logging
logger.info("Running createLatestAirSensor_exec.R version %s",VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse="\n")
logger.debug("R session:\n\n%s\n", sessionString)

# ------ Create AirSensor objects ----------------------------------------------

result <- try({
  
  logger.info("Loading PA Synoptic data")
  pas <- pas_load()
  
  # Find the labels of interest
  labels <-
    pas %>%
    pas_filter(is.na(parentID)) %>%
    pas_filter(stringr::str_detect(label, opt$pattern)) %>%
    dplyr::pull(label)
  
  logger.info("Loading PAT data for %d sensors", length(labels))
  
  airSensorList <- list()
  
  for ( label in labels ) {
    
    logger.trace("Working on %s", label)
    
    result <- try({
      
      airSensorList[[label]] <- 
        pat_loadLatest(label) %>%
        pat_createAirSensor(
          period = "1 hour",
          parameter = "pm25",
          channel = "ab",
          qc_algorithm = "hourly_AB_01",
          min_count = 20
        )
      
    }, silent = TRUE)
    if ( "try-error" %in% class(result) ) {
      logger.warn(geterrmessage())
    }
    
  }
  
  airsensor <- PWFSLSmoke::monitor_combine(airSensorList)
  class(airsensor) <- c("airsensor", "ws_monitor", "list")
  
  filename <- paste0("airsensor_scaqmd_latest7.rda")
  filepath <- file.path(opt$outputDir, filename)
  
  logger.info("Writing 'airsensor' data to %s", filename)
  save(list="airsensor", file = filepath)
  
}, silent=TRUE)

# Handle errors
if ( "try-error" %in% class(result) ) {
  msg <- paste("Error creating monthly AirSensor file: ", geterrmessage())
  logger.fatal(msg)
} else {
  # Guarantee that the errorLog exists
  if ( !file.exists(errorLog) ) dummy <- file.create(errorLog)
  logger.info("Completed successfully!")
}

