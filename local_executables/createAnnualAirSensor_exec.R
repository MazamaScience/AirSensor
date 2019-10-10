#!/usr/local/bin/Rscript

# This Rscript will ingest airsensor_~_latest7.rda files and use them to create
# airsensor files with extended time ranges: 45-day and monthly.
#
# Test this script from the command line with:
#
# ./createAnnualAirSensor_exec.R
#
# Run it inside a docker continer with something like:
#
# docker run --rm -v /Users/jonathan/Projects/MazamaScience/AirSensor/local_executables:/app -w /app mazamascience/airsensor /app/createAnnualAirSensor_exec.R --collectionName="MethowValley"
#

#  ----- . ----- . -----
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
    outputDir = file.path(getwd()),
    logDir = file.path(getwd()),
    datestamp = "2019",
    collectionName = "scaqmd",
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
      help="Datestamp specifying the year as YYYY [default=current year]"
    ),
    make_option(
      c("-n","--collectionName"), 
      default="COLLECTION_NAME", 
      help="Name associated with this collection of sensors [default=\"%default\"]"
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
  cat(paste0("createAnnualAirSensor_exec.R ",VERSION,"\n"))
  quit()
}

# ----- Validate parameters ----------------------------------------------------

# Create annual files based on UTC years
timezone <- "UTC"

if ( !dir.exists(opt$outputDir) ) 
  stop(paste0("outputDir not found:  ",opt$outputDir))

if ( !dir.exists(opt$logDir) ) 
  stop(paste0("logDir not found:  ",opt$logDir))

# ----- Create datestamps ------------------------------------------------------

# Default to the current year
now <- lubridate::now(tzone = timezone)
if ( opt$datestamp == "" ) {
  opt$datestamp <- strftime(now, "%Y", tz = timezone)
}

# Handle the case where month or day is already specified
yearstamp <- as.numeric(stringr::str_sub(opt$datestamp, 1, 4))
startstamp <- paste0(yearstamp, "0101")
endstamp <- paste0((yearstamp+1), "0101")

# ----- Set up logging ---------------------------------------------------------

logger.setup(
  traceLog = file.path(opt$logDir, paste0("createAnnualAirSensor_",opt$collectionName,"_TRACE.log")),
  debugLog = file.path(opt$logDir, paste0("createAnnualAirSensor_",opt$collectionName,"_DEBUG.log")), 
  infoLog  = file.path(opt$logDir, paste0("createAnnualAirSensor_",opt$collectionName,"_INFO.log")), 
  errorLog = file.path(opt$logDir, paste0("createAnnualAirSensor_",opt$collectionName,"_ERROR.log"))
)

# For use at the very end
errorLog <- file.path(opt$logDir, paste0("createAnnualAirSensor_",opt$collectionName,"_ERROR.log"))

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

# Start logging
logger.info("Running createAnnualAirSensor_exec.R version %s",VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse="\n")
logger.debug("R session:\n\n%s\n", sessionString)

# ------ Create annual airsensor object ----------------------------------------

result <- try({
  
  # NOTE:  Error messages are getting through even wilth "silent = TRUE" so we
  # NOTE:  just capture everything and log it.
  output <- capture.output({
    airsensor <- sensor_load(opt$collectionName, startstamp, endstamp)
  })
  
  logger.trace(paste0(output, collapse="\n"))
  
  # NOTE:  The resulting file will have a UTC stamp but will include an extra
  # NOTE:  day at the beginning and end so as to accomadate non-UTC timezones.
  # NOTE:  It is incumbent upon code ingesting these annual files to trim any 
  # NOTE:  excess.
  
  filename <- paste0("airsensor_scaqmd_", yearstamp, ".rda")
  filepath <- file.path(opt$outputDir, filename)
  
  logger.info("Writing 'airsensor' data to %s", filename)
  save(list="airsensor", file = filepath)
  
}, silent=TRUE)

# Handle errors
if ( "try-error" %in% class(result) ) {
  msg <- paste("Error creating annual airsensor file: ", geterrmessage())
  logger.fatal(msg)
} else {
  # Guarantee that the errorLog exists
  if ( !file.exists(errorLog) ) dummy <- file.create(errorLog)
  logger.info("Completed successfully!")
}

# ===== DEBUGGING ==============================================================

# if ( FALSE ) {
#   
#   library(AirMonitorPlots)
#   
#   sensor <- get(load("airsensor_scaqmd_2019.rda"))
#   
#   gg <- ggplot_pm25Timeseries(sensor,
#                               startdate = 20190101,
#                               enddate = 20191231) + 
#     geom_pm25Points(shape = "square", alpha = .1)
#  
#   
#   print(gg)
#   
# }
