#!/usr/local/bin/Rscript

# This Rscript will download the latest synoptic JSON file from Purple Air. 

# This script is desgined to be run daily as a cron job.

# 1 2 3 4 5 /Users/jonathan/Projects/PWFSL/mazamascience/mazamapurpleair/createDailyPAS_exec.R --outputDir=/Users/jonathan/Data/AirNow/RData --logDir=/Users/jonathan/Data/AirNow/RData

# You can test things by firing up the docker image interactively with bash and 
# then Running R and testing this script a few lines at a time:
#
# docker run --rm -v /home/monitoring/Projects/mazamascience/mazamapurpleair:/monitoring/v4 -v /data/monitoring:/monitoring/v4/data -w /monitoring/v4 -ti mazamascience/mazamapurpleair bash

VERSION = "0.1.0" #  --- . --- . first pass

library(methods)       # always included for Rscripts
library(optparse)      # to parse command line flags

# The following packages are attached here so they show up in the sessionInfo
suppressPackageStartupMessages({
  library(MazamaCoreUtils)
  library(MazamaSpatialUtils)
  library(MazamaPurpleAir)
})

# Set up OptionParser
option_list <- list(
  make_option(c("-o","--outputDir"), default=getwd(), help="Output directory for generated .RData files [default=\"%default\"]"),
  make_option(c("-l","--logDir"), default=getwd(), help="Output directory for generated .log file [default=\"%default\"]"),
  make_option(c("-s","--spatialDataDir"), default='/home/mazama/data/Spatial', help="Directory containing spatial datasets used by MazamaSpatialUtils [default=\"%default\"]"),
  make_option(c("-V","--version"), action="store_true", default=FALSE, help="Print out version number [default=\"%default\"]")
)

# Parse arguments
opt <- parse_args(OptionParser(option_list=option_list))

# For debugging
if ( FALSE ) {
  
  # Desktop
  opt <- list(outputDir = getwd(),
              logDir = getwd(),
              spatialDataDir = "~/Data/Spatial")  
  
  # Docker container
  opt <- list(outputDir = getwd(),
              logDir = getwd(),
              spatialDataDir = "/home/mazama/data/Spatial")  
  
}

# Print out version and quit
if ( opt$version ) {
  cat(paste0('createDailyPAS_exec.R ',VERSION,'\n'))
  quit()
}

# Sanity checks
if ( !dir.exists(opt$outputDir) ) stop(paste0("outputDir not found:  ",opt$outputDir))
if ( !dir.exists(opt$logDir) ) stop(paste0("logDir not found:  ",opt$logDir))
if ( !dir.exists(opt$spatialDataDir) ) stop(paste0("spatialDataDir not found:  ",opt$spatialDataDir))

# Assign log file names
debugLog <- file.path(opt$logDir, 'createDailyPAS_DEBUG.log')
infoLog  <- file.path(opt$logDir, 'createDailyPAS_INFO.log')
errorLog <- file.path(opt$logDir, 'createDailyPAS_ERROR.log')

# Set up logging
logger.setup(debugLog=debugLog, infoLog=infoLog, errorLog=errorLog)

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

# Set up MazamaSpatialUtils
initializeMazamaSpatialUtils(opt$spatialDataDir)

logger.debug('Running createDailyPAS_exec.R version %s',VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse='\n')
logger.debug('R session:\n\n%s\n', sessionString)

# ------ Download data ---------------------------------------------------------

result <- try({
  
  datestamp <- strftime(lubridate::now('America/Los_Angeles'), "%Y%m%d")
  filename <- paste0("pas_", datestamp, ".rda")
  filepath <- file.path(opt$outputDir, filename)
  
  logger.info('Obtaining "pas" data for %s', datestamp)
  pas <- pas_load()
  
  logger.info('Writing "pas" data to %s', filename)
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

