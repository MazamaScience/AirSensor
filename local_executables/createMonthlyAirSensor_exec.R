#!/usr/local/bin/Rscript

# This Rscript will process archived 'pat' data files into a single 'airsensor'
# file containing hourly data for all sensors.
#
# Test this script from the command line with:
#
# ./createMonthlyAirSensor_exec.R
#
# Run it inside a docker continer with something like:
#
# docker run --rm -v /Users/jonathan/Projects/MazamaScience/AirSensor/local_executables:/app -w /app mazamascience/airsensor /app/createMonthlyAirSensor_exec.R --pattern=^SCNP_..$
#

#  --- . --- . AirSensor 0.4.1
VERSION = "0.3.7" 

library(optparse)      # to parse command line flags

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
      c("-d","--datestamp"), 
      default="", 
      help="Datestamp specifying the year and month as YYYYMM [default=current month]"
    ),
    make_option(
      c("-t","--timezone"), 
      default="America/Los_Angeles", 
      help="timezone used to interpret datestamp  [default=\"%default\"]"
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
  cat(paste0("createMonthlyAirSensor_exec.R ",VERSION,"\n"))
  quit()
}

# ----- Validate parameters ----------------------------------------------------

if ( !dir.exists(opt$outputDir) ) 
  stop(paste0("outputDir not found:  ",opt$outputDir))

if ( !dir.exists(opt$logDir) ) 
  stop(paste0("logDir not found:  ",opt$logDir))

# Default to the current month
now <- lubridate::now(opt$timezone)
if ( opt$datestamp == "" ) {
  opt$datestamp <- strftime(now, "%Y%m01", tz = opt$timezone)
}

# Handle the case where the day is already specified
datestamp <- stringr::str_sub(paste0(opt$datestamp,"01"), 1, 8)
monthstamp <- stringr::str_sub(datestamp, 1, 6)

# ----- Set up logging ---------------------------------------------------------

logger.setup(
  traceLog = file.path(opt$logDir, paste0("createMonthlyAirSensor_",monthstamp,"_TRACE.log")),
  debugLog = file.path(opt$logDir, paste0("createMonthlyAirSensor_",monthstamp,"_DEBUG.log")), 
  infoLog  = file.path(opt$logDir, paste0("createMonthlyAirSensor_",monthstamp,"_INFO.log")),
  errorLog = file.path(opt$logDir, paste0("createMonthlyAirSensor_",monthstamp,"_ERROR.log"))
)

# For use at the very end
errorLog <- file.path(opt$logDir, paste0("createMonthlyAirSensor_",monthstamp,"_ERROR.log"))

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

# Start logging
logger.info("Running createMonthlyAirSensor_exec.R version %s",VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse="\n")
logger.debug("R session:\n\n%s\n", sessionString)

# ------ Create AirSensor objects ----------------------------------------------

result <- try({
  
  # Get times
  starttime <- lubridate::ymd(datestamp, tz=opt$timezone)
  endtime <- lubridate::ceiling_date(starttime + lubridate::ddays(20), unit="month")
  
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
        pat_loadMonth(label, monthstamp, opt$timezone) %>%
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
  
  # Guarantee we don't end up with "2000" dates
  airsensor <- PWFSLSmoke::monitor_subset(airsensor, tlim=c(starttime, endtime))
  
  filename <- paste0("airsensor_scaqmd_", monthstamp, ".rda")
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

