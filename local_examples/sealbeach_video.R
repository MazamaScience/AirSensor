library(AirSensor)
library(MazamaSpatialUtils)

sensor <- sensor_load()

start <- lubridate::parse_date_time(20190703, orders = "ymd", 
                                    tz = "America/Los_Angeles")
end   <- lubridate::parse_date_time(20190705, orders = "ymd", 
                                    tz = "America/Los_Angeles")
movieData <- sensor_filterDate(sensor, startdate = 20190704, enddate = 20190705)

tickSkip <- 6
tAxis <- movieData$data$datetime
tAxis[(lubridate::hour(tAxis) - 1) %% tickSkip == 0 & 
            lubridate::minute(tAxis) == 0]
tTicks <- tAxis[(lubridate::hour(tAxis) - 1) %% tickSkip == 0 & 
                         lubridate::minute(tAxis) == 0]
tLabels <- strftime(tTicks, "%l %P")
tInfo <- PWFSLSmoke::timeInfo(tAxis, -118.082, 33.767)

communityRegion <- "Seal Beach"
staticMap <- PWFSLSmoke::staticmap_getStamenmapBrick(centerLon = -118.083,
                                                     centerLat = 33.767,
                                                     zoom = 15,
                                                     width = 770,
                                                     height = 495)

folder <- opt$folder
#dir.create(folder)

# Generate individual frames
for (i in 1:length(tAxis)) {
  fTime <- tAxis[i]
  number <- stringr::str_pad(i, 3, 'left', '0')
  fileName <- paste0("sealbeach_video_", number, ".png")
  filePath <- file.path(folder, fileName)
  png(filePath, width = 1280, height = 720, units = "px")
  sensor_videoFrame(sensor,
                    communityRegion = "Seal Beach",
                    frameTime = fTime,
                    timeInfo = tInfo,
                    timeRange = tAxis,
                    timeTicks = tTicks,
                    timeLabels = tLabels,
                    map = staticMap)
  print(strftime(fTime, "%b %d %H:%M"))
  dev.off()
}

# Generate an mp4 video with:
# ffmpeg -r 6 -f image2 -s 1280x720 -i sealbeach_video_%03d.png -vcodec libx264 -crf 25 video.mp4
