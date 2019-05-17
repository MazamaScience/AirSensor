#!/usr/local/bin/Rscript

# This Rscript will download the latest synoptic JSON file from Purple Air. 

# This script is desgined to be run monthly as a cron job.

# 1 2 3 4 5 /Users/jonathan/Projects/PWFSL/mazamascience/airsensor/createMonthlyPAT_exec.R --outputDir=/Users/jonathan/Data/AirNow/RData --logDir=/Users/jonathan/Data/AirNow/RData

# You can test things by firing up the docker image interactively with bash and 
# then Running R and testing this script a few lines at a time:
#
# docker run --rm -v /home/monitoring/Projects/mazamascience/airsensor:/monitoring/v4 -v /data/monitoring:/monitoring/v4/data -w /monitoring/v4 -ti mazamascience/airsensor bash

VERSION = "0.1.2" #  --- . --- . AirSensor 0.2.10

library(methods)       # always included for Rscripts
library(optparse)      # to parse command line flags

# The following packages are attached here so they show up in the sessionInfo
suppressPackageStartupMessages({
  library(futile.logger)
  library(MazamaCoreUtils)
  library(AirSensor)
})

# Set up OptionParser
option_list <- list(
  make_option(c("-o","--outputDir"), default=getwd(), help="Output directory for generated .RData files [default=\"%default\"]"),
  make_option(c("-l","--logDir"), default=getwd(), help="Output directory for generated .log file [default=\"%default\"]"),
  make_option(c("-d","--datestamp"), default='', help="Datestamp specifying the year and month as YYYYMM [default=current month]"),
  make_option(c("-t","--timezone"), default='America/Los_Angeles', help="timezone used to interpret datestamp  [default=\"%default\"]"),
  make_option(c("-p","--pattern"), default='^[Ss][Cc].._..$', help="String patter passed to stringr::str_detect  [default=\"%default\"]"),
  make_option(c("-V","--version"), action="store_true", default=FALSE, help="Print out version number [default=\"%default\"]")
)

# Parse arguments
opt <- parse_args(OptionParser(option_list=option_list))

# For debugging
if ( FALSE ) {
  
  # Desktop
  opt <- list(outputDir = getwd(),
              logDir = getwd(),
              datestamp = "",
              timezone = "America/Los_Angeles",
              pattern = "^[Ss][Cc].._..$")  
  
  # Docker container
  opt <- list(outputDir = getwd(),
              logDir = getwd(),
              datestemp = "",
              timezone = "America/Los_Angeles",
              pattern = "^[Ss][Cc].._..$")  
  
}

# Print out version and quit
if ( opt$version ) {
  cat(paste0('createMonthlyPAT_exec.R ',VERSION,'\n'))
  quit()
}

# Sanity checks
if ( !dir.exists(opt$outputDir) ) stop(paste0("outputDir not found:  ",opt$outputDir))
if ( !dir.exists(opt$logDir) ) stop(paste0("logDir not found:  ",opt$logDir))

# Assign log file names
debugLog <- file.path(opt$logDir, 'createMonthlyPAT_DEBUG.log')
infoLog  <- file.path(opt$logDir, 'createMonthlyPAT_INFO.log')
errorLog <- file.path(opt$logDir, 'createMonthlyPAT_ERROR.log')

# Set up logging
logger.setup(debugLog=debugLog, infoLog=infoLog, errorLog=errorLog)

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

logger.debug('Running createMonthlyPAT_exec.R version %s',VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse='\n')
logger.debug('R session:\n\n%s\n', sessionString)

# ------ Download data ---------------------------------------------------------

result <- try({
  
  # Default to the current month
  if ( opt$datestamp == "" ) {
    now <- lubridate::now(opt$timezone)
    opt$datestamp <- strftime(now, "%Y%m%d")
  }
  
  # Handle the case where the day is already specified
  datestamp <- stringr::str_sub(paste0(opt$datestamp,"01"), 1, 8)
  monthstamp <- stringr::str_sub(datestamp, 1, 6)
  
  # Get times
  starttime <- lubridate::ymd(datestamp, tz=opt$timezone)
  endtime <- lubridate::ceiling_date(starttime + lubridate::ddays(20), unit="month")
  
  # Get strings
  startdate <- strftime(starttime, "%Y-%m-%d", tz = opt$timezone)
  enddate <- strftime(endtime, "%Y-%m-%d", tz = opt$timezone)
  
  logger.info('Loading PA Synoptic data')
  pas <- pas_load()
  
  # Find the labels of interest
  labels <-
    pas %>%
    pas_filter(stringr::str_detect(label, opt$pattern)) %>%
    dplyr::pull(label)

  logger.info('Loading PA Timeseries data for %d sensors', length(labels))
  
  for ( label in labels ) {

    filename <- paste0("pat_", monthstamp, "_", label, ".rda")
    filepath <- file.path(opt$outputDir, filename)
    
    # Try block so we keep chugging if one sensor fails
    result <- try({
      
      logger.debug("pat_loadLatest(pas, '%s', %s, %s)", 
                   label, startdate, enddate)
      pat <- pat_loadLatest(pas, label,
                            startdate = startdate,
                            enddate = enddate)
      logger.info('Writing "pat" data to %s', filename)
      save(list="pat", file=filepath)
      
    }, silent = TRUE)
    if ( "try-error" %in% class(result) ) {
      logger.warn(geterrmessage())
    }
    
  }  
  
  
  
}, silent=TRUE)

# Handle errors
if ( "try-error" %in% class(result) ) {
  msg <- paste("Error creating monthly PAT file: ", geterrmessage())
  logger.fatal(msg)
} else {
  # Guarantee that the errorLog exists
  if ( !file.exists(errorLog) ) dummy <- file.create(errorLog)
  logger.info("Completed successfully!")
}

