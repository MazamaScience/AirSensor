#!/usr/local/bin/Rscript

# This Rscript will download the latest synoptic JSON file from Purple Air. 
#
# Test this script from the command line with:
#
# ./createPAS_exec.R
#
# Run it inside a docker continer with something like:
#
# docker run --rm -v /Users/jonathan/Projects/MazamaScience/AirSensor/local_executables:/app -w /app mazamascience/airsensor /app/createPAS_exec.R 
#

#  --- . --- . AirSensor 0.3.12
VERSION = "0.1.5"

# The following packages are attached here so they show up in the sessionInfo
suppressPackageStartupMessages({
  library(futile.logger)
  library(MazamaCoreUtils)
  library(AirSensor)
  
  setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
})

# ----- Get command line arguments ---------------------------------------------

if ( interactive() ) {
  
  # RStudio session
  opt <- list(
    outputDir = getwd(),
    logDir = getwd(),
    spatialDataDir = "~/Data/Spatial",
    version = FALSE
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
      c("-s","--spatialDataDir"), 
      default="/home/mazama/data/Spatial", 
      help="Directory containing spatial datasets used by MazamaSpatialUtils [default=\"%default\"]"
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
  cat(paste0("createPAS_exec.R ", VERSION, "\n"))
  quit()
}

# ----- Validate parameters ----------------------------------------------------

if ( !dir.exists(opt$outputDir) ) 
  stop(paste0("outputDir not found:  ",opt$outputDir))

if ( !dir.exists(opt$logDir) ) 
  stop(paste0("logDir not found:  ",opt$logDir))

if ( !dir.exists(opt$spatialDataDir) ) 
  stop(paste0("spatialDataDir not found:  ",opt$spatialDataDir))

# ----- Set up logging ---------------------------------------------------------

logger.setup(
  traceLog = file.path(opt$logDir, "createPAS_TRACE.log"),
  debugLog = file.path(opt$logDir, "createPAS_DEBUG.log"), 
  infoLog  = file.path(opt$logDir, "createPAS_INFO.log"), 
  errorLog = file.path(opt$logDir, "createPAS_ERROR.log")
)

# For use at the very end
errorLog <- file.path(opt$logDir, "createPAS_ERROR.log")

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

# Start logging
logger.info("Running createPAS_exec.R version %s",VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse="\n")
logger.debug("R session:\n\n%s\n", sessionString)

# ------ Create PAS ------------------------------------------------------------

result <- try({
  
  # Set up MazamaSpatialUtils
  AirSensor::initializeMazamaSpatialUtils(opt$spatialDataDir)
  
  # Save it with the UTC YYYYmmddHH stamp
  timestamp <- strftime(lubridate::now("UTC"), "%Y%m%d%H", tz="UTC")
  filename <- paste0("pas_", timestamp, ".rda")
  filepath <- file.path(opt$outputDir, filename)
  
  logger.info("Obtaining 'pas' data for %s", timestamp)
  pas <- pas_createNew(
    baseUrl = 'https://www.purpleair.com/json',
    countryCodes = c('US'),
    includePWFSL = TRUE,
    lookbackDays = 1
  )
  
  logger.info("Writing 'pas' data to %s", filename)
  save(list="pas", file=filepath)  
  
  # Save it with the YYYYmmdd stamp
  timestamp <- strftime(lubridate::now("UTC"), "%Y%m%d", tz="UTC")
  filename <- paste0("pas_", timestamp, ".rda")
  filepath <- file.path(opt$outputDir, filename)
  
  logger.info("Writing 'pas' data to %s", filename)
  save(list="pas", file=filepath)  
  
}, silent=TRUE)

# Handle errors
if ( "try-error" %in% class(result) ) {
  msg <- paste("Error creating daily PAS file: ", geterrmessage())
  logger.fatal(msg)
} else {
  # Guarantee that the errorLog exists
  if ( !file.exists(errorLog) ) dummy <- file.create(errorLog)
  logger.info("Completed successfully!")
}

