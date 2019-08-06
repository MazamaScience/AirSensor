#!/usr/local/bin/Rscript

# This Rscript will download the latest timeseries data from Purple Air. 
#
# Test this script from the command line with:
#
# ./createLatestPAT_exec.R
#
# Run it inside a docker continer with something like:
#
# docker run --rm -v /Users/jonathan/Projects/MazamaScience/AirSensor/local_executables:/app -w /app mazamascience/airsensor /app/createLatestPAT_exec.R --pattern=^SCNP_..$
#

#  --- . --- . AirSensor 0.3.14
VERSION = "0.1.0"

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
    pattern = "^SCNP_..$",
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
      c("-p","--pattern"), 
      default="^[Ss][Cc].._..$", 
      help="String patter passed to stringr::str_detect  [default=\"%default\"]"
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
  cat(paste0("createLatestPAT_exec.R ",VERSION,"\n"))
  quit()
}

# ----- Validate parameters ----------------------------------------------------

if ( !dir.exists(opt$outputDir) ) 
  stop(paste0("outputDir not found:  ",opt$outputDir))

if ( !dir.exists(opt$logDir) ) 
  stop(paste0("logDir not found:  ",opt$logDir))

# ----- Set up logging ---------------------------------------------------------

logger.setup(
  traceLog = file.path(opt$logDir, paste0("createLatestPAT_TRACE.log")),
  debugLog = file.path(opt$logDir, paste0("createLatestPAT_DEBUG.log")), 
  infoLog  = file.path(opt$logDir, paste0("createLatestPAT_INFO.log")), 
  errorLog = file.path(opt$logDir, paste0("createLatestPAT_ERROR.log"))
)

# For use at the very end
errorLog <- file.path(opt$logDir, paste0("createLatestPAT_ERROR.log"))

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

# Start logging
logger.info("Running createLatestPAT_exec.R version %s",VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse="\n")
logger.debug("R session:\n\n%s\n", sessionString)

# ------ Create PAT objects ----------------------------------------------------

result <- try({
  
  # Get times
  starttime <- lubridate::now(tzone = "UTC")
  endtime <- starttime - lubridate::ddays(8) # to get 7 full days
  
  # Get strings
  startdate <- strftime(starttime, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  enddate <- strftime(endtime, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  logger.info("Loading PA Synoptic data")
  pas <- pas_load()
  
  # Find the labels of interest, only one per sensor
  labels <-
    pas %>%
    pas_filter(is.na(parentID)) %>%
    pas_filter(stringr::str_detect(label, opt$pattern)) %>%
    dplyr::pull(label)
  
  logger.info("Loading PA Timeseries data for %d sensors", length(labels))
  
  for ( label in labels ) {
    
    # Try block so we keep chugging if one sensor fails
    result <- try({
      
      logger.debug("pat_createNew(pas, '%s', '%s', '%s')", 
                   label, startdate, enddate)

      pat <- pat_createNew(
        pas,
        label,
        startdate = startdate,
        enddate = enddate,
        baseURL = "https://api.thingspeak.com/channels/"
      )
      
      filename <- paste0("pat_", label, "_latest7.rda")
      filepath <- file.path(opt$outputDir, filename)
      
      logger.trace("Writing 'pat' data to %s", filename)
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

