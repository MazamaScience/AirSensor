#!/usr/local/bin/Rscript

# This Rscript will create calendar plots for a set of sensors. 
#
# Test this script from the command line with:
#
# ./createCalendarPlot_exec.R
#
# Run it inside a docker continer with something like:
#
# docker run --rm -v /Users/jonathan/Projects/MazamaScience/AirSensor/local_executables:/app -w /app mazamascience/airsensor /app/createCalendarPlot_exec.R --pattern=^SCNP_..$
#

#  --- . --- . AirSensor 0.4.2
VERSION = "0.1.0"

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
      help="Datestamp specifying the year [default=current year]"
    ),
    make_option(
      c("-t","--timezone"), 
      default="America/Los_Angeles", 
      help="timezone used to interpret datestamp  [default=\"%default\"]"
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
  cat(paste0("createCalendarPlot_exec.R ",VERSION,"\n"))
  quit()
}

# ----- Validate parameters ----------------------------------------------------

if ( !dir.exists(opt$outputDir) ) 
  stop(paste0("outputDir not found:  ",opt$outputDir))

if ( !dir.exists(opt$logDir) ) 
  stop(paste0("logDir not found:  ",opt$logDir))

# Default to the current year
now <- lubridate::now(opt$timezone)
if ( opt$datestamp == "" ) {
  opt$datestamp <- strftime(now, "%Y", tz = opt$timezone)
}

# Handle the case where month or day is already specified
yearstamp <- stringr::str_sub(opt$datestamp, 1, 4)
datestamp <- paste0(yearstamp, "0101")

# ----- Set up logging ---------------------------------------------------------

logger.setup(
  traceLog = file.path(opt$logDir, paste0("createCalendarPlot_",yearstamp,"_TRACE.log")),
  debugLog = file.path(opt$logDir, paste0("createCalendarPlot_",yearstamp,"_DEBUG.log")), 
  infoLog  = file.path(opt$logDir, paste0("createCalendarPlot_",yearstamp,"_INFO.log")), 
  errorLog = file.path(opt$logDir, paste0("createCalendarPlot_",yearstamp,"_ERROR.log"))
)

# For use at the very end
errorLog <- file.path(opt$logDir, paste0("createCalendarPlot_",yearstamp,"_ERROR.log"))

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

# Start logging
logger.info("Running createCalendarPlot_exec.R version %s",VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse="\n")
logger.debug("R session:\n\n%s\n", sessionString)

# ------ Create PAT objects ----------------------------------------------------

result <- try({
  
  # Get times
  starttime <- MazamaCoreUtils::parseDatetime(datestamp, timezone = opt$timezone)
  endtime <- lubridate::ceiling_date(starttime + lubridate::ddays(300), unit = "year")
  
  # Get strings
  startdate <- strftime(starttime, "%Y%m%d", tz = opt$timezone)
  enddate <- strftime(endtime, "%Y%m%d", tz = opt$timezone)
  
  logger.info("Loading 'airsensor' data")
  sensor <- sensor_load(startdate = startdate, enddate = enddate)
  
  # Find the monitorIDs of interest, only one per sensor
  monitorIDs <-
    sensor %>%
    sensor_filterMeta(stringr::str_detect(monitorID, opt$pattern)) %>%
    PWFSLSmoke::monitor_extractMeta() %>%
    dplyr::pull(monitorID)
  
  logger.info("Creating calendar plots for %d sensors", length(monitorIDs))
  
  for ( monitorID in monitorIDs ) {
    
    # Try block so we keep chugging if one sensor fails
    result <- try({
      
      filename <- paste0(monitorID, "_", yearstamp, "_calendarPlot.png")
      filepath <- file.path(opt$outputDir, filename)
      
      logger.trace("Working on %s", filename)
      
      gg <- 
        sensor %>% 
        PWFSLSmoke::monitor_subset(monitorID = monitorID) %>% 
        sensor_calendarPlot(ncol = 2, aspectRatio = 0.4)
      
      ggplot2::ggsave(filepath, gg, width = 6, height = 12, units = "in")
      
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

