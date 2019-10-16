#!/usr/local/bin/Rscript

# This Rscript will download the latest timeseries data from Purple Air. 
#
# Test this script from the command line with:
#
# ./createMonthlyPAT_exec.R
#
# Run it inside a docker continer with something like:
#
# docker run --rm -v /Users/jonathan/Projects/MazamaScience/AirSensor/local_executables:/app -w /app mazamascience/airsensor /app/createMonthlyPAT_exec.R --pattern=^SCNP_..$
#

#  --- . --- . fixed pas filtering
VERSION = "0.1.11"

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
    # timezone = "America/Los_Angeles",
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
      c("-d","--datestamp"), 
      default="", 
      help="Datestamp specifying the year and month as YYYYMM [default=current month]"
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
  cat(paste0("createMonthlyPAT_exec.R ",VERSION,"\n"))
  quit()
}

# ----- Validate parameters ----------------------------------------------------

# Create month files based on UTC months
timezone <- "UTC"

if ( !dir.exists(opt$outputDir) ) 
  stop(paste0("outputDir not found:  ",opt$outputDir))

if ( !dir.exists(opt$logDir) ) 
  stop(paste0("logDir not found:  ",opt$logDir))

# Default to the current month
now <- lubridate::now(tzone = timezone)
if ( opt$datestamp == "" ) {
  opt$datestamp <- strftime(now, "%Y%m01", tz = timezone)
}

# Handle the case where the day is already specified
datestamp <- stringr::str_sub(paste0(opt$datestamp,"01"), 1, 8)
monthstamp <- stringr::str_sub(datestamp, 1, 6)

# ----- Set up logging ---------------------------------------------------------

logger.setup(
  traceLog = file.path(opt$logDir, paste0("createMonthlyPAT_",monthstamp,"_TRACE.log")),
  debugLog = file.path(opt$logDir, paste0("createMonthlyPAT_",monthstamp,"_DEBUG.log")), 
  infoLog  = file.path(opt$logDir, paste0("createMonthlyPAT_",monthstamp,"_INFO.log")), 
  errorLog = file.path(opt$logDir, paste0("createMonthlyPAT_",monthstamp,"_ERROR.log"))
)

# For use at the very end
errorLog <- file.path(opt$logDir, paste0("createMonthlyPAT_",monthstamp,"_ERROR.log"))

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

# Start logging
logger.info("Running createMonthlyPAT_exec.R version %s",VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse="\n")
logger.debug("R session:\n\n%s\n", sessionString)

# ------ Create PAT objects ----------------------------------------------------

result <- try({
  
  # Get times that extend one day earlier and one day later to ensure we get
  # have a least a full month, regardless of timezone. This overlap is OK 
  # because the pat_join() function uses pat_distinct() to remove duplicate 
  # records.
  starttime <- lubridate::ymd(datestamp, tz=timezone)
  endtime <- lubridate::ceiling_date(starttime + lubridate::ddays(20), unit="month")
  
  starttime <- starttime - lubridate::ddays(1)
  endtime <- endtime + lubridate::ddays(1)
  
  # Get strings
  startdate <- strftime(starttime, "%Y%m%d", tz = timezone)
  enddate <- strftime(endtime, "%Y%m%d", tz = timezone)
  
  logger.trace("startdate = %s, enddate = %s", startdate, enddate)
  
  logger.info("Loading PAS data for %s ", opt$pattern)
  
  # Start with all sensors and filter based on date.
  pas <- pas_load(archival = TRUE) %>%
    pas_filter(lastSeenDate > starttime)
  
  # Find the labels of interest, only one per sensor
  labels <-
    pas %>%
    pas_filter(is.na(parentID)) %>%
    pas_filter(stringr::str_detect(label, opt$pattern)) %>%
    dplyr::pull(label)
  
  logger.info("Loading PAT data for %d sensors", length(labels))
  
  for ( label in labels ) {
    
    # Try block so we keep chugging if one sensor fails
    result <- try({
      
      logger.trace("pat_createNew(pas, '%s', '%s', '%s')", 
                   label, startdate, enddate)

      pat <- pat_createNew(
        pas,
        label,
        startdate = startdate,
        enddate = enddate,
        timezone = timezone,
        baseURL = "https://api.thingspeak.com/channels/"
      )
      
      filename <- paste0("pat_", label, "_", monthstamp, ".rda")
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

