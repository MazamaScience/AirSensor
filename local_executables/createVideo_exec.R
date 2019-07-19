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
      default="./frames",
      help="Path of directory to save frames to [default=\"%default\"]"
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

print(paste0("community: '", opt$community, "'"))
print(paste0("startDate: '", opt$startDate, "'"))
print(paste0("directory: '", opt$directory, "'"))

if (opt$community == "Seal Beach") {
  lon <- -118.083
  lat <- 33.767
  z <- 15
} else if (opt$community == "Big Bear Lake") {
  lon <- -116.898568 
  lat <- 34.255736
  z <- 13
} else if (opt$community == "Sycamore Canyon") {
  lon <- -117.306669
  lat <- 33.949935
  z <- 15
} else {
  stop(paste0("Community '", opt$community, "' is not one of the 12 SC communities"))
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
  movieData <- sensor_filter(sensor, datetime >= start, datetime <= end)
  
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
  
  # Create a directory for the frames (if it doesn't already exist)
  dirPath <- opt$directory
  if (!dir.exists(dirPath)) {
    dir.create(dirPath)
  }
  communityID <- sub("\\_.*", "", dplyr::filter(sensor$meta, communityRegion == opt$community)[1, "monitorID"])
  
  # Generate individual frames
  for (i in 1:length(tAxis)) {
    ft <- tAxis[i]
    number <- stringr::str_pad(i, 3, 'left', '0')
    fileName <- paste0(communityID, number, ".png")
    filePath <- file.path(dirPath, fileName)
    png(filePath, width = 1280, height = 720, units = "px")
    sensor_videoFrame(sensor,
                      communityRegion = opt$community,
                      frameTime = ft,
                      timeInfo = tInfo,
                      timeRange = tAxis,
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

#system(paste0("cd ", dirPath))
#system(paste0("ffmpeg -r 6 -f image2 -s 1280x720 -i ", communityID, "%03d.png -vcodec libx264 -crf 25 video.mp4"))
