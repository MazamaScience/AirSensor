#!/usr/local/bin/Rscript

# This Rscript generates a video for a South Coast community over a 3-day 
# period. If a startDate is given, then that day and the next two are covered.
# If no startDate is given then the last 72 hours are covered.
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
      c("-c","--community"), 
      default="", 
      help="Name of South Coast community [default=\"%default\"]"
    ),
    make_option(
      c("-s","--startDate"), 
      default=0, 
      help="Start date for the 3-day period [default=\"%default\"]"
    ),
    make_option(
      c("-d", "--directory"),
      default="./",
      help="Path of directory to save video to [default=\"%default\"]"
    ),
    make_option(
      c("-r", "--frameRate"),
      default=6,
      help="Frames per second [default=\"%default\"]"
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
if (opt$version) {
  cat(paste0("createVideo_exec.R ", VERSION, "\n"))
  quit()
}

if (opt$community == "") {
  cat(paste0("Community name must be given\n"))
  quit()
}

# ----- Validate parameters ----------------------------------------------------

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

if (opt$frameRate < 0 || opt$frameRate != floor(opt$frameRate)) {
  stop("frameRate must be a positive integer")
}

if (opt$community == "Alhambra/Monterey Park") {
  lon <- -118.132324 
  lat <- 34.072205
  z <- 13
} else if (opt$community == "Big Bear Lake") {
  lon <- -116.898568 
  lat <- 34.255736
  z <- 13
} else if (opt$community == "El Monte") {
  lon <- -118.034595
  lat <- 34.069292
  z <- 12
} else if (opt$community == "Imperial Valley") {
  lon <- -115.551228
  lat <- 32.980878
  z <- 14
} else if (opt$community == "Nipomo") {
  lon <- -120.555047
  lat <- 35.061590
  z <- 12
} else if (opt$community == "Paso Robles") {
  lon <- -120.668946
  lat <- 35.513530
  z <- 10
} else if (opt$community == "San Jacinto") {
  lon <- -116.958228
  lat <- 33.765083
  z <- 14
} else if (opt$community == "SCAH") {
  lon <- -122.139473
  lat <- 37.662620
  z <- 10
} else if (opt$community == "SCAN") {
  lon <- -122.307492
  lat <- 37.964949
  z <- 12
} else if (opt$community == "SCUV") {
  lon <- -118.427781
  lat <- 34.023917
  z <- 15
} else if (opt$community == "South Gate") {
  lon <- -118.178104
  lat <- 33.934260
  z <- 13
} else if (opt$community == "Sycamore Canyon") {
  lon <- -117.307598
  lat <- 33.947524
  z <- 15
} else if (opt$community == "Temescal Valley") {
  lon <- -117.481278
  lat <- 33.753517
  z <- 12
} else {
  stop(paste0("There is no SC community '", opt$community, "'"))
}

# ----- Set up logging ---------------------------------------------------------

# ------ Create video frames ---------------------------------------------------

result <- try({
  
  # Load data
  sensor <- sensor_load()
  timeZone <- dplyr::filter(sensor$meta, communityRegion == opt$community)[1, "timezone"]
  
  if (opt$startDate > 0) {
    start <- lubridate::parse_date_time(opt$startDate, orders = "ymd", tz = timeZone)
    end   <- start + lubridate::days(2)
  } else {
    # Is this timezone right?
    end   <- lubridate::now(tz = timeZone)
    start <- end - lubridate::hours(72)
  }
  
  # Can't filter by just the date
  #movieData <- sensor_filterDate(sensor, startdate = start, enddate = end)
  movieData <- sensor_filter(sensor, datetime >= start, datetime < end)
  
  # Time axis data
  tickSkip <- 6
  tAxis <- movieData$data$datetime
  tAxis[(lubridate::hour(tAxis) - 1) %% tickSkip == 0 & 
           lubridate::minute(tAxis) == 0]
  tTicks <- tAxis[(lubridate::hour(tAxis) - 1) %% tickSkip == 0 & 
                     lubridate::minute(tAxis) == 0]
  tLabels <- strftime(tTicks, "%l %P")
  tInfo <- PWFSLSmoke::timeInfo(tAxis, longitude = lon, latitude = lat)
  
  # Load static map image of community
  staticMap <- PWFSLSmoke::staticmap_getStamenmapBrick(centerLon = lon,
                                                       centerLat = lat,
                                                       zoom = z,
                                                       width = 770,
                                                       height = 495)
  
  communityID <- sub("\\_.*", "", dplyr::filter(sensor$meta, communityRegion == opt$community)[1, "monitorID"])
  communityID <- toupper(communityID)
  
  # Generate individual frames
  for (i in 1:length(tAxis)) {
    ft <- tAxis[i]
    number <- stringr::str_pad(i, 3, 'left', '0')
    fileName <- paste0(communityID, number, ".png")
    filePath <- file.path(tempdir(), fileName)
    png(filePath, width = 1280, height = 720, units = "px")
    sensor_videoFrame(sensor,
                      communityRegion = opt$community,
                      frameTime = ft,
                      timeInfo = tInfo,
                      timeAxis = tAxis,
                      timeTicks = tTicks,
                      timeLabels = tLabels,
                      map = staticMap)
    print(strftime(ft, "%b %d %H:%M"))
    dev.off()
  }
  
}, silent=TRUE)

# Handle errors
if ( "try-error" %in% class(result) ) {
  msg <- paste("Error creating video frames: ", geterrmessage())
  #logger.fatal(msg)
} else {
  # Guarantee that the errorLog exists
  #if ( !file.exists(errorLog) ) dummy <- file.create(errorLog)
  #logger.info("Completed successfully!")
}

# Define system calls to ffmpeg to create video from frames
cmd_cd <- paste0("cd ", tempdir())
cmd_ffmpeg <- paste0("ffmpeg -r ", opt$frameRate, " -f image2 -s 1280x720 -i ", 
                     communityID, "%03d.png -vcodec libx264 -crf 25 ", 
                     opt$directory, "/", communityID, ".mp4")
cmd_rm <- paste0("rm *.png")

# Make system calls
system(paste0(cmd_cd, " && ", cmd_ffmpeg, " && ", cmd_rm))
