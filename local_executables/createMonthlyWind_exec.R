#!/usr/local/bin/Rscript

# This Rscript will process archived wind data files into a single 'windData'
# file containing hourly data for all sensors.
#
# Test this script from the command line with:
#
# ./createMonthlyWind_exec.R
# 
# Run it inside a docker continer with something like:
#
# docker run --rm -v /Users/jonathan/Projects/MazamaScience/AirSensor/local_executables:/app -w /app mazamascience/pwfslsmoke /app/createMonthlyWind_exec.R
#

# ---- . ---- .  AirSensor 0.3.5
VERSION = "0.0.3" 

suppressPackageStartupMessages({
  library(MazamaCoreUtils)
  library(PWFSLSmoke)
})

# ----- Get command line arguments ---------------------------------------------

if ( interactive() ) {
  
  # RStudio session
  opt <- list(
    outputDir = getwd(),
    logDir = getwd(),
    spatialDataDir = "~/Data/Spatial",
    datestamp = "20190701",
    timezone = "America/Los_Angeles",
    version = FALSE
  )  
  
} else {
  
  # Set up OptionParser
  library(optparse)
  
  option_list <- list(
    optparse::make_option(
      c("-o","--outputDir"), 
      default=getwd(), 
      help="Output directory for generated .RData files [default=\"%default\"]"
    ),
    
    optparse::make_option(
      c("-l","--logDir"), 
      default=getwd(), 
      help="Output directory for generated .log file [default=\"%default\"]"
    ),
    
    make_option(
      c("-s","--spatialDataDir"), 
      default="/home/mazama/data/Spatial", 
      help="Directory containing spatial datasets used by MazamaSpatialUtils [default=\"%default\"]"
    ),

    optparse::make_option(
      c("-d","--datestamp"), 
      default="", 
      help="Datestamp of the year & month as YYYYMM [default=current month]"
    ),
    
    optparse::make_option(
      c("-t","--timezone"), 
      default="America/Los_Angeles", 
      help="timezone used to interpret datestamp [default=\"%default\"]"
    ),
    
    optparse::make_option(
      c("-V","--version"), 
      action="store_true", 
      default=FALSE, 
      help="Print out version number [default=\"%default\"]"
    )
    
  )
  
  # Parse arguments
  opt <- optparse::parse_args(OptionParser(option_list=option_list))
  
}

# Print out version and quit
if ( opt$version ) {
  
  cat(paste0("createMonthlyWind_exec.R ",VERSION,"\n"))
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
  traceLog = file.path(opt$logDir, "createMonthlyWind_TRACE.log"),
  debugLog = file.path(opt$logDir, "createMonthlyWind_DEBUG.log"), 
  infoLog  = file.path(opt$logDir, "createMonthlyWind_INFO.log"), 
  errorLog = file.path(opt$logDir, "createMonthlyWind_ERROR.log")
)

# For use at the very end
errorLog <- file.path(opt$logDir, "createMonthlyWind_ERROR.log")

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

# Start logging
logger.info("Running createMonthlyWind_exec.R version %s",VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse="\n")
logger.debug("R session:\n\n%s\n", sessionString)

# ----- Create Wind Data -------------------------------------------------------

result <- try({

  # Set up MazamaSpatialUtils
  MazamaSpatialUtils::setSpatialDataDir(opt$spatialDataDir)
  MazamaSpatialUtils::loadSpatialData("NaturalEarthAdm1")
  
  now <- lubridate::now(opt$timezone)
  
  # Default to the current month
  if ( opt$datestamp == "" )
    opt$datestamp <- strftime(now, "%Y%m01", tz = opt$timezone)

  # Handle the case where the day is already specified
  datestamp <- 
    stringr::str_sub(paste0(opt$datestamp,"01"), 1, 8)
  monthstamp <- 
    stringr::str_sub(datestamp, 1, 6)
  
  # Get times
  starttime <- 
    lubridate::ymd(datestamp, tz=opt$timezone)
  endtime <- 
    lubridate::ceiling_date(starttime + lubridate::ddays(20), unit="month")
  
  # Handle current month
  if ( endtime > now ) 
    endtime <- lubridate::floor_date(now, unit="hours")
  
  # Get hours
  hourCount <- as.numeric(difftime(endtime, starttime, units = "hours"))
  
  # === Monitors needed ===
  # SCAH: Chabot
  # SCAN: Vallejo
  # SCAP: Pasadena
  # SCBB: Crestline - Lake Gregory
  # SCEM: Pasadena
  # SCIV: El Centro – 9th Street
  # SCNP: NA
  # SCPR: NA
  # SCSB: South Long Beach
  # SCSC: (mislabeled SCSB sensor)
  # SCSG: Compton
  # SCSH: Riverside– Rubidoux
  # SCSJ: NA
  # SCTV: Lake Elsinore – W. Flint Street
  # SCUV: West Los Angeles – VA Hospital
  
  
  logger.info("Loading Wind data")
  
  # Load monitor objects
  airnow_monitorObjects <- 
    PWFSLSmoke::airnow_createMonitorObjects(
      parameters = c("WS", "WD"), 
      startdate = starttime, 
      hours = hourCount
    )
  
  airnow_WS <- airnow_monitorObjects$WS
  airnow_WD <- airnow_monitorObjects$WD
  airnow_monitorIDs <- airnow_WD$meta$monitorID
  
  # SCAQMD Provided monitors 
  siteNames <-
    c( #"Chabot", # Cannot find this one
       "Vallejo", 
       "Pasadena", 
       "Crestline - Lake Gregory",
       "Pasadena", 
       "El Centro - 9th Street", 
       #"NA", 
       #"NA", 
       "South Long Beach", 
       #"mislabeled", 
       "Compton", 
       "Riverside - Rubidoux", 
       #"NA", 
       "Lake Elsinore - W. Flint Street", 
       "West Los Angeles - VA Hospital"
    )
  
  # Gather monitorIDs using provided site names
  monitorIDs <- 
    airnow_monitorIDs[which(airnow_WD$meta$siteName %in% siteNames)]
  
  # Wind Direction
  WD <- 
    PWFSLSmoke::monitor_subset(
      ws_monitor = airnow_WD, 
      monitorIDs = monitorIDs
    ) 
  
  # Wind Speed
  WS <- 
    PWFSLSmoke::monitor_subset(
      ws_monitor = airnow_WS, 
      monitorIDs = monitorIDs
    )
  
  # Guarantee consistency of classes among package
  class(WD) <- c("airsensor", "ws_monitor", "list")
  class(WS) <- c("airsensor", "ws_monitor", "list")
  
  # Guarantee we don't end up with "2000" dates
  WD <- 
    PWFSLSmoke::monitor_subset(WD, tlim=c(starttime, endtime))
  WS <- 
    PWFSLSmoke::monitor_subset(WS, tlim=c(starttime, endtime))
  
  # Write file names and paths
  WD_filename <- paste0("airsensor_WD_", monthstamp, ".rda")
  WD_filepath <- file.path(opt$outputDir, WD_filename)
  
  WS_filename <- paste0("airsensor_WS_", monthstamp, ".rda")
  WS_filepath <- file.path(opt$outputDir, WS_filename)
  
  # Write wind wirection
  logger.info("Writing 'WD' data to %s", WD_filename)
  save(list="WD", file = WD_filepath)
  
  # Write wind speed
  logger.info("Writing 'WS' data to %s", WS_filename)
  save(list="WS", file = WS_filepath)
  
}, silent = TRUE)

# Handle errors
if ( "try-error" %in% class(result) ) {
  
  msg <- paste0("Error creating monthly Wind data file: ", geterrmessage())
  logger.fatal(msg)
  
} else {
  
  # Guarantee that the errorLog exists
  if ( !file.exists(errorLog) ) dummy <- file.create(errorLog)
  logger.info("Completed successfully!")
  
}

