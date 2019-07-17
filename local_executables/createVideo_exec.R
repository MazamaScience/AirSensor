#!/usr/local/bin/Rscript

# This Rscript will generate a video for a SC community for a 3-day period. 
#
# Test this script from the command line with:
#
# ./createVideo_exec.R -c "Seal Beach" -s 20190704

VERSION = "0.1.0"

# The following packages are attached here so they show up in the sessionInfo
suppressPackageStartupMessages({
  library(AirSensor)
  library(MazamaCoreUtils)
  library(MazamaSpatialUtils)
})

# ----- Get command line arguments ---------------------------------------------

if ( interactive() ) {
  
} else {
  
  # Set up OptionParser
  library(optparse)
  
  option_list <- list(
    make_option(
      c("-s","--startDate"), 
      default=20190704, 
      help="Start date for the 3-day period [default=\"%default\"]"
    ),
    make_option(
      c("-c","--community"), 
      default="Seal Beach", 
      help="Name of South Coast community [default=\"%default\"]"
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
  cat(paste0("createVideo_exec.R ", VERSION, "\n"))
  quit()
}

# ----- Validate parameters ----------------------------------------------------



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

# ------ Create video frames ---------------------------------------------------

result <- try({
  
  start <- lubridate::parse_date_time(opt$startDate, orders = "ymd", 
                                      tz = "America/Los_Angeles")
  end   <- start + lubridate::days(3)
  
  if (opt$community == "Seal Beach") {
    lon <- -118.083
    lat <- 33.767
    z <- 15
  } else {
    stop(paste0("Community '", opt$community, "' is not one of the 12 SC communities"))
  }
  
  sensor <- sensor_load()
  movieData <- sensor_filterDate(sensor, startdate = start, enddate = end)
  
  tickSkip <- 6
  tRange <- movieData$data$datetime
  tRange[(lubridate::hour(tRange) - 1) %% tickSkip == 0 & 
           lubridate::minute(tRange) == 0]
  tTicks <- tRange[(lubridate::hour(tRange) - 1) %% tickSkip == 0 & 
                     lubridate::minute(tRange) == 0]
  tLabels <- strftime(tTicks, "%l %P")
  tInfo <- PWFSLSmoke::timeInfo(tRange, longitude = lon, latitude = lat)
  
  communityRegion <- "Seal Beach"
  staticMap <- PWFSLSmoke::staticmap_getStamenmapBrick(centerLon = lon,
                                                       centerLat = lat,
                                                       zoom = z,
                                                       width = 770,
                                                       height = 495)
  
  folder <- "sealbeach_video_frames"
  dir.create(folder)
  
  # Generate individual frames
  for (i in 1:length(tRange)) {
    ft <- tRange[i]
    number <- stringr::str_pad(i, 3, 'left', '0')
    fileName <- paste0("sealbeach_video_", number, ".png")
    filePath <- file.path(folder, fileName)
    png(filePath, width = 1280, height = 720, units = "px")
    sensor_videoFrame(sensor,
                      communityRegion = community,
                      frameTime = ft,
                      timeInfo = tInfo,
                      timeRange = tRange,
                      timeTicks = tTicks,
                      timeLabels = tLabels,
                      map = staticMap)
    print(strftime(ft, "%b %d %H:%M"))
    dev.off()
  }
  
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
