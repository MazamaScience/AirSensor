#' @export
#' 
#' @title Generate video frames
#' 
#' @param sensor An AirSensor object.
#' @param communityRegion The area to display.
#' @param frameTime POSIXct specifying the time to use.
#' @param timeInfo ???.
#' @param timeRange Movie time range.
#' @param timeTicks Movie time ticks.
#' @param timeLabels Movie time labels.
#' @param map A pre-generated basemap image.
#' 
#' @description desc
#' 
#' @return ret
#' 
#' @examples 
#' ft <- lubridate::ymd_h("2019-07-04 18")
#' #ti <- PWFSLSmoke::timeInfo(ft, -118.082, 33.767)
#' sMap <- PWFSLSmoke::staticmap_getStamenmapBrick(centerLon = -118.083,
#'                                                 centerLat = 33.767,
#'                                                 zoom = 15,
#'                                                 width = 770,
#'                                                 height = 495)
#' sensor_videoFrame(communityRegion = "Seal Beach", 
#'                   frameTime = ft,
#'                   map = sMap)

sensor_videoFrame <- function(sensor = sensor_load(),
                              communityRegion = NULL,
                              frameTime = NULL,
                              timeInfo = NULL,
                              timeRange = NULL,
                              timeTicks = NULL,
                              timeLabels = NULL,
                              map = NULL) {
  
  # Debug
  if (TRUE) {
    # Movie parameters (timeframe, community, map)
    boop <- sensor_filterDate(sensor, startdate = 20190704, enddate = 20190705)
    timeRange <- boop$data$datetime
    timeRange[(lubridate::hour(timeRange) - 1) %% 3 == 0 & 
                lubridate::minute(timeRange) == 0]
    timeTicks <- timeRange[(lubridate::hour(timeRange) - 1) %% 3 == 0 & 
                             lubridate::minute(timeRange) == 0]
    timeLabels <- strftime(timeTicks, "%l %P")
    communityRegion <- "Seal Beach"
    # timeInfo <- PWFSLSmoke::timeInfo(ft, -118.082, 33.767) # Needs "SimpleTimezones"
    map <- PWFSLSmoke::staticmap_getStamenmapBrick(centerLon = -118.083,
                                                   centerLat = 33.767,
                                                   zoom = 15,
                                                   width = 770,
                                                   height = 495)
    
    # For the individual frame
    frameTime <- lubridate::ymd_h("2019-07-04 18")
  }
  
  # ----- Validate parameters --------------------------------------------------
  
  if (is.null(frameTime))
    stop("Parameter 'frameTime' must be defined.")
  
  #if (is.null(timeInfo))
    #stop("Parameter 'timeInfo' must be defined.")
  
  if (is.null(map))
    stop("Parameter 'map' must be defined.")
  
  # Round frameTime to closest hour
  frameTime <- lubridate::round_date(frameTime, unit = "hour")
  
  # ----- Subset data ----------------------------------------------------------
  
  community <- dplyr::filter(sensor$meta,
                             .data$communityRegion == communityRegion)
  monitorId <- community$monitorID
  longitude <- community$longitude
  latitude <- community$latitude
  
  data <- sensor$data %>% 
    dplyr::select(.data$datetime, monitorId) %>% 
    dplyr::filter(.data$datetime == frameTime) %>%
    dplyr::select(-.data$datetime) %>%
    tidyr::gather(key = "monitorID", value = "pm25")
  pm25 <- data$pm25
  
  data <- data.frame(monitorId, longitude, latitude, pm25)
  
  # ----- Plot day/night shading -----------------------------------------------
  
  par(bg = "white")
  #par(bg = "gray60")
  # Need to have timeInfo working to do this bit
  
  # ----- Plot map -------------------------------------------------------------
  
  # Map
  layout(matrix(c(2, 1), 1, 2), heights = 1, widths = c(1, 7))
  par(mar = c(0, 0, 0, 0))
  PWFSLSmoke::staticmap_plotRasterBrick(map)
  
  # ----- Plot legend ----------------------------------------------------------
  
  colorPalette <- grDevices::colorRampPalette(c("#22f777", "yellow", "gold",
                                                "#f19100", "#f15800", 
                                                "#cc3702"))(30)
  colorBins <- c(seq(0, 60, length = 24), 300)
  usr <- par("usr")
  top    <- usr[4] - (usr[4] - usr[3]) / 3
  bottom <- usr[3] + (usr[4] - usr[3]) / 8
  left   <- usr[2] - (usr[2] - usr[1]) / 6
  right  <- usr[2] - (usr[2] - usr[1]) / 8
  
  # Draw gradient
  colorCount <- length(colorPalette)
  bottoms <- seq(bottom, top, length = colorCount + 1)[-(colorCount + 1)]
  tops    <- seq(bottom, top, length = colorCount + 1)[-1]
  rect(left, bottoms, right, tops, col = colorPalette, border = NA)
  rect(left, bottom,  right, top)
  
  # Draw labels
  text(left, bottom,  as.character(0),  pos = 2, cex = 1.5)
  text(left, top,     as.character(60), pos = 2, cex = 1.5)
  raster::text(right - (right - left) / 2, top + (usr[4] - usr[3]) / 8,
       labels = "PM 2.5", font = 2,  cex = 2.1)
  raster::text(right - (right - left) / 2, top + (usr[4] - usr[3]) / 16, 
       labels = "(\U03BCg/m\U00B3)", cex = 1.4)
  
  # ----- Plot points ----------------------------------------------------------
  
  colors = PWFSLSmoke::aqiColors(data$pm25, 
                                 palette = colorPalette, 
                                 domain = c(0, 250),
                                 bins = colorBins)
  
  points(x = data$longitude,
         y = data$latitude,
         col = colors,
         pch = 16, 
         cex = 4)
  
  # ----- Plot date ------------------------------------------------------------
  
  par(mar = c(0.5, 0.5, 6, 0.5))
  plot(rep(0, 10), -(as.numeric(1:10)), axes = F, col = 'red')
  
  mtext(strftime(frameTime, "%b %e", tz = "America/Los_Angeles") , line = 2.5, 
        cex = 2.1)
  mtext(strftime(frameTime, "%l %P", tz = "America/Los_Angeles") , line = 0.5, 
        cex = 1.5)
  

  # Fix: Axis is being drawn way under the date/time text
  if (!is.null(timeRange) && !is.null(timeTicks) && !is.null(timeLabels)) {
    labels <- timeLabels
    axis(2, labels = labels, line = -5.5, at = -as.numeric(timeTicks), 
         cex.axis = 1.9, las = 2, hadj = 1, cex= 2, lwd.ticks = 2.7,
         lwd = 2.7)
    axis(4, line = -4.25, at = -as.numeric(frameTime), col = 'transparent', 
         col.ticks = 2, lwd.ticks = 15, labels = "", tcl = -2)  
  }
  
}
