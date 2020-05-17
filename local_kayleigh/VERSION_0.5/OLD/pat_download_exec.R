#!/usr/local/bin/Rscript

#
# ./B_exec.R
#

#  --- . --- . MazamaCoreUtils 0.3.5
VERSION = "0.1.6"

# The following packages are attached here so they show up in the sessionInfo
suppressPackageStartupMessages({
  library(futile.logger)
  library(MazamaCoreUtils)
  library(AirSensor)
  
  setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
})

# ----- Get command line arguments ---------------------------------------------

if ( interactive() ) {
  
  # RStudio session
  opt <- list(
    outputDir = getwd(),
    logDir = getwd(),
    label = "EDCAQMD Placerville Library",
    timezone = "America/Los_Angeles",
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
      c("-u", "--label"),
      default = "EDCAQMD Placerville Library",
      help = "label for the purple air sensor of interest"
    ),
    make_option(
      c("-p", "--timezone"),
      default = "America/Los_Angeles",
      help = "timezone you want the data to be in"
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
logger.info("Running B_exec.R version %s",VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse="\n")
logger.debug("R session:\n\n%s\n", sessionString)

# ------ Create PAS ------------------------------------------------------------

result <- try({
  
  # Get times
  endtime <- lubridate::now(tzone = "UTC")
  starttime <- lubridate::floor_date(endtime) - lubridate::ddays(1)
  
  # Get strings
  startdate <- strftime(starttime, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  enddate <- strftime(endtime, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  logger.trace("startdate = %s, enddate = %s", startdate, enddate)
  
  logger.info("Loading PAS data")
  
  # Start with all sensors and filter based on date.
  pas <- pas_load(archival = TRUE) %>%
    pas_filter(lastSeenDate > starttime)
  
  logger.info("PAS download completed")
  states <- unique(pas$stateCode)

  # Save it with the YYYYmmdd stamp
  timestamp <- strftime(lubridate::now(tzone = "UTC"), "%Y%m%d", tz="UTC")
  filename <- paste0("pas_", timestamp, ".rda")
  filepath <- file.path(opt$outputDir, filename)
  
  
  for (i in states){
    pas_state = pas_filter(pas = pas, stateCode == states[i])
    logger.info("Obtaining 'pat' data")
    pat <- pat_createNew(
      pas_state,
      label = opt$label,
      startdate = startdate,
      enddate = enddate,
      timezone = opt$timezone,
      #baseURL = "https://api.thingspeak.com/channels/"
  )

  }
  
  logger.info("pat download completed")
  # logger.info("creating plot")
  # png("testplot.png", width = 900, height = 900)
  # pat_multiPlot(pat)
  # dev.off 
  
  
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

