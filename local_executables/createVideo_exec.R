#!/usr/local/bin/Rscript

# This Rscript generates a video for a South Coast community over a 3-day 
# period. If an endDate is given, then that day and the previous two are covered.
# If no endDate is given then the last 72 hours are covered. Resulting video
# is labeled by the communitiy's South Coast ID.
#
# Test this script from the command line with:
#
# ./createVideo_exec.R --communityName="Sycamore Canyon" -s 20190704 -r 4 -o ~/Desktop/ -v TRUE
# ./createVideo_exec.R -c SCSB -o ~/Desktop/

# ---- . ---- . AirSensor 0.4.1
VERSION = "0.1.2"

# The following packages are attached here so they show up in the sessionInfo
suppressPackageStartupMessages({
  library(futile.logger)
  library(AirSensor)
  library(MazamaCoreUtils)
  library(MazamaSpatialUtils)
  
  setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
})

# ----- Get command line arguments ---------------------------------------------

if ( interactive() ) {
  
  # RStudio session
  opt <- list(outputDir = getwd(),
              logDir = getwd(),
              version = FALSE)  
  
} else {
  
  # Set up OptionParser
  library(optparse)
  
  option_list <- list(
    make_option(
      c("-c","--communityID"), 
      default = "", 
      help = "ID of the South Coast community [default=\"%default\"]"
    ),
    make_option(
      c("-C","--communityName"), 
      default = "", 
      help = "Name of the South Coast community [default=\"%default\"]"
    ),
    make_option(
      c("-s","--endDate"), 
      default = NULL, 
      help = "End date for the 3-day (72 hr) period [default=\"%default\"]"
    ),
    make_option(
      c("-r", "--frameRate"),
      default = 6,
      help="Frames per second [default=\"%default\"]"
    ),
    make_option(
      c("-v","--verbose"), 
      default = FALSE, 
      help="Print out generated frame files [default=\"%default\"]"
    ),
    make_option(
      c("-o","--outputDir"), 
      default = getwd(), 
      help="Output directory for generated video file [default=\"%default\"]"
    ),
    make_option(
      c("-l","--logDir"), 
      default = getwd(), 
      help="Output directory for generated .log file [default=\"%default\"]"
    ),
    make_option(
      c("-V","--version"), 
      action = "store_true", 
      default = FALSE, 
      help = "Print out version number [default=\"%default\"]"
    )
  )
  
  # Parse arguments
  opt <- parse_args(OptionParser(option_list=option_list))
}

# Print out version and quit
if (opt$version) {
  cat(paste0("createVideo_exec.R ", VERSION, "\n"))
  quit()
}

# ----- Validate parameters ----------------------------------------------------

if (opt$frameRate < 0 || opt$frameRate != floor(opt$frameRate)) {
  stop("frameRate must be a positive integer")
}

if (opt$communityID == "" && opt$communityName == "") {
  stop("Must define either a community name or ID")
}

if (!dir.exists(opt$outputDir)) {
  stop(paste0("outputDir not found:  ", opt$outputDir))
}

if (!dir.exists(opt$logDir)) {
  stop(paste0("logDir not found:  ", opt$logDir))
}

# ----- Set up logging ---------------------------------------------------------

logger.setup(
  traceLog = file.path(opt$logDir, "createVideo_TRACE.log"),
  debugLog = file.path(opt$logDir, "createVideo_DEBUG.log"), 
  infoLog  = file.path(opt$logDir, "createVideo_INFO.log"), 
  errorLog = file.path(opt$logDir, "createVideo_ERROR.log")
)

# For use at the very end
errorLog <- file.path(opt$logDir, "createVideo_ERROR.log")

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

# Start logging
logger.info("Running createVideo_exec.R version %s", VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse="\n")
logger.debug("R session:\n\n%s\n", sessionString)

# ------ Create video frames ---------------------------------------------------

result <- try({
  
  # SCAP --- Alhambra/Monterey Park
  # SCBB --- Big Bear Lake
  # SCEM --- El Monte
  # SCIV --- Imperial Valley
  # SCNP --- Nipomo
  # SCPR --- Paso Robles
  # SCSJ --- San Jacinto
  # SCSB --- Seal Beach
  # SCAH --- SCAH
  # SCAN --- SCAN
  # SCUV --- SCUV
  # SCSG --- South Gate
  # SCHS --- Sycamore Canyon
  # SCTV --- Temescal Valley
  
  communityGeoMapInfo <- list(
    SCAP = list(lon = -118.132324, lat = 34.072205, zoom = 13),
    SCBB = list(lon = -116.898568, lat = 34.255736, zoom = 13),
    SCEM = list(lon = -118.034595, lat = 34.069292, zoom = 12),
    SCIV = list(lon = -115.551228, lat = 32.980878, zoom = 14),
    SCNP = list(lon = -120.555047, lat = 35.061590, zoom = 12),
    SCPR = list(lon = -120.668946, lat = 35.513530, zoom = 10),
    SCSJ = list(lon = -116.958228, lat = 33.765083, zoom = 14),
    SCSB = list(lon = -118.083084, lat = 33.767033, zoom = 15),
    SCAH = list(lon = -122.139473, lat = 37.662620, zoom = 10),
    SCAN = list(lon = -122.307492, lat = 37.964949, zoom = 12),
    SCUV = list(lon = -118.427781, lat = 34.023917, zoom = 15),
    SCSG = list(lon = -118.178104, lat = 33.934260, zoom = 13),
    SCHS = list(lon = -117.307598, lat = 33.947524, zoom = 15),
    SCTV = list(lon = -117.481278, lat = 33.753517, zoom = 12)
  )
  
  # Load all sensor data
  logger.info("Loading sensor data")
  sensor <- sensor_load()
  
  # Retrieve both the community name and ID. Prioritize name over ID if they are
  # different.
  if (opt$communityName != "") {
    communityMeta <- dplyr::filter(sensor$meta, communityRegion == opt$communityName)
    if (nrow(communityMeta) == 0) {
      stop(paste0("Community with name '", opt$communityName, "' has no monitors"))
    }
    opt$communityID <- toupper(sub("\\_.*", "", communityMeta[1, "monitorID"]))
  } else if (opt$communityID != "") {
    opt$communityID <- toupper(opt$communityID)
    communityMeta <- dplyr::filter(sensor$meta, stringr::str_detect(toupper(monitorID), paste0("^", opt$communityID, "_")))
    if (nrow(communityMeta) == 0) {
      stop(paste0("Community with ID '", opt$communityID, "' has no monitors"))
    }
    opt$communityName <- communityMeta[1, "communityRegion"]
  } else {
    stop("Must provide a South Coast community name or ID")
  }
  
  # Get the timezone
  timezone <-
    sensor %>%
    sensor_extractMeta() %>%
    dplyr::filter(communityRegion == opt$communityName) %>%
    dplyr::slice(1) %>%
    dplyr::pull(timezone)
  
  # Look back 71 hours from endDate entry, or back 71 hours from the most 
  # recent data entry if no endDate is given
  logger.info("Setting start and end times")
  if ( !is.null(opt$endDate) ) {
    endtime <- lubridate::parse_date_time(opt$endDate, orders = "ymd", tz = timezone)
    starttime   <- endtime - lubridate::hours(71)
  } else {
    endtime <- sensor$data[nrow(sensor$data), "datetime"]
    starttime <- endtime - lubridate::hours(71)
  }
  
  mapInfo <- communityGeoMapInfo[[opt$communityID]]
  
  # Time axis data
  logger.info("Preparing the time axis")
  tickSkip <- 6
  movieData <- sensor_filter(sensor, datetime >= starttime, datetime <= endtime)
  tAxis <- movieData$data$datetime
  tAxis[(lubridate::hour(tAxis) - 1) %% tickSkip == 0 & 
           lubridate::minute(tAxis) == 0]
  tTicks <- tAxis[(lubridate::hour(tAxis) - 1) %% tickSkip == 0 & 
                     lubridate::minute(tAxis) == 0]
  tLabels <- strftime(tTicks, "%l %P", tz = timezone)
  tInfo <- PWFSLSmoke::timeInfo(tAxis, longitude = mapInfo$lon, latitude = mapInfo$lat)
  
  # Load a static map image of the community
  logger.info("Loading static map of community '%s'", opt$communityName)
  staticMap   <- PWFSLSmoke::staticmap_getStamenmapBrick(centerLon = mapInfo$lon,
                                                         centerLat = mapInfo$lat,
                                                         zoom = mapInfo$zoom,
                                                         width = 770,
                                                         height = 495)
  
  # Generate individual frames
  logger.info("Generating %s video frames", length(tAxis))
  for (i in 1:length(tAxis)) {
    frameTime <- tAxis[i]
    number <- stringr::str_pad(i, 3, 'left', '0')
    fileName <- paste0(opt$communityID, number, ".png")
    filePath <- file.path(tempdir(), fileName)
    png(filePath, width = 1280, height = 720, units = "px")
    logger.trace("Generating frame %s: %s", number, strftime(frameTime, "%b %d %H:%M", tz = timezone))
    sensor_videoFrame(
      sensor,
      communityRegion = opt$communityName,
      frameTime = frameTime,
      timeInfo = tInfo,
      timeAxis = tAxis,
      timeTicks = tTicks,
      timeLabels = tLabels,
      map = staticMap
    )
    if (opt$verbose) {
      print(strftime(frameTime, "%b %d %H:%M", tz = timezone))
    }
    dev.off()
  }
  
  # Create a file name timestamped with the final frameTime in the
  # "America/Los_Angeles" timezone
  fileName <- paste0(
    opt$communityID, "_",
    strftime(frameTime, "%Y%m%d", tz = "America/Los_Angeles"), ".mp4"
  )

  # Define system calls to ffmpeg to create video from frames
  cmd_cd <- paste0("cd ", tempdir())
  cmd_ffmpeg <- paste0("ffmpeg -loglevel quiet -r ", 
                       opt$frameRate, " -f image2 -s 1280x720 -i ", 
                       opt$communityID, "%03d.png -vcodec libx264 -crf 25 ", 
                       opt$outputDir, "/", fileName)
  cmd_rm <- paste0("rm *.png")
  cmd <- paste0(cmd_cd, " && ", cmd_ffmpeg, " && ", cmd_rm)
  
  # Make system calls
  logger.info("Calling ffmpeg to make video from frames")
  logger.trace(cmd)

  ffmpegString <- paste(capture.output(system(cmd)), collapse="\n")

  logger.trace("ffmpeg output:\n\n%s\n", ffmpegString)
  
}, silent=TRUE)

if (opt$verbose) {
  print(result)
}

# Handle errors
if ( "try-error" %in% class(result) ) {
  msg <- paste("Error creating video: ", geterrmessage())
  logger.fatal(msg)
} else {
  # Guarantee that the errorLog exists
  if ( !file.exists(errorLog) ) dummy <- file.create(errorLog)
  logger.info("Completed successfully!")
  logger.error("No errors")
}
