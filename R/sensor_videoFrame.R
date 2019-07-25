#' @export
#' 
#' @title Generate video frames
#' 
#' @param sensor An AirSensor object.
#' @param communityRegion The area to display.
#' @param frameTime POSIXct specifying the time to use.
#' @param timeInfo ???.
#' @param timeAxis Movie time axis.
#' @param timeTicks Movie time ticks.
#' @param timeLabels Movie time labels.
#' @param map A pre-generated basemap image.
#' 
#' @description desc
#' 
#' @return A plot is generated. Nothing is returned.
#' 
#' @examples 
#' \dontrun{
#' # Fourth of July in Seal Beach
#' library(MazamaSpatialUtils)
#' 
#' start <- lubridate::ymd_h("2019-07-04 00", tz = "America/Los_Angeles")
#' end   <- start + lubridate::days(3)
#' 
#' sensor <- sensor_load("scaqmd")
#' 
#' movieData <- 
#'   sensor %>%
#'   sensor_filterMeta(communityRegion == "Seal Beach") %>%
#'   sensor_filterDate(start, end)
#'
#' # Seal Beach map
#' lon <- -118.083
#' lat <- 33.767
#' zoom <- 15
#' 
#' map <- PWFSLSmoke::staticmap_getStamenmapBrick(
#'   centerLon = lon,
#'   centerLat = lat,
#'   zoom = zoom,
#'   width = 770,
#'   height = 495
#' )
#' 
#' tickSkip <- 6
#' timeAxis <- movieData$data$datetime
#' timeTicks <- timeAxis[(lubridate::hour(timeAxis) - 1) %% tickSkip == 0 & 
#'                  lubridate::minute(timeAxis) == 0]
#' timeLabels <- strftime(timeTicks, "%l %P")
#' timeInfo <- PWFSLSmoke::timeInfo(timeAxis, longitude = lon, latitude = lat)
#' 
#' frameTime <- lubridate::ymd_h("2019-07-04 21", tz="America/Los_Angeles")
#' timeInfo <- PWFSLSmoke::timeInfo(frameTime, lon, lat)
#' 
#' sensor_videoFrame(
#'   sensor = sensor,
#'   communityRegion = "Seal Beach", 
#'   frameTime = frameTime,
#'   timeInfo = timeInfo,
#'   timeAxis = timeAxis,
#'   timeTicks = timeTicks,
#'   timeLabels = timeLabels,
#'   map = map
#' )
#' }

sensor_videoFrame <- function(
  sensor = sensor_load(),
  communityRegion = NULL,
  frameTime = NULL,
  timeInfo = NULL,
  timeAxis = NULL,
  timeTicks = NULL,
  timeLabels = NULL,
  map = NULL,
  logo = png::readPNG("~/Desktop/ms_logo.png")
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !sensor_isSensor(sensor) ) 
    stop("Required parameter 'sensor' is not an airsensor object.")
  
  if ( sensor_isEmpty(sensor) )
    stop("Required parameter 'sensor' is empty.")
  
  if ( is.null(frameTime) )
    stop("Required parameter 'frameTime' is missing.")
  
  if ( is.null(timeInfo) )
    stop("Required parameter 'timeInfo' is missing.")
  
  if ( is.null(timeAxis) )
    stop("Required parameter 'timeAxis' is missing.")
  
  if ( is.null(timeTicks) )
    stop("Required parameter 'timeTicks' is missing.")
  
  if ( is.null(timeLabels) )
    stop("Required parameter 'timeLabels' is missing.")
  
  if ( is.null(map) )
    stop("Required parameter 'map' is missing.")
  
  # Round frameTime to closest hour
  frameTime <- lubridate::round_date(frameTime, unit = "hour")
  
  # ----- Subset data ----------------------------------------------------------
  
  community <- 
    dplyr::filter(sensor$meta, .data$communityRegion == communityRegion)
  monitorId <- community$monitorID
  longitude <- community$longitude
  latitude <- community$latitude
  
  sensorData <- sensor$data %>% 
    dplyr::select(.data$datetime, monitorId) %>% 
    dplyr::filter(.data$datetime == frameTime) %>%
    dplyr::select(-.data$datetime) %>%
    tidyr::gather(key = "monitorID", value = "pm25")
  pm25 <- sensorData$pm25
  
  data <- data.frame(monitorId, longitude, latitude, pm25)
  
  # ----- Day/night shading ----------------------------------------------------
  
  ti <- dplyr::filter(timeInfo, .data$localTime == frameTime)
  
  if(ti$night) {
    par(bg = "gray60")
  } else {
    par(bg = "white")
  }
  
  # add transitions
  # sunset
  if (ti$sunset - ti$localTime < lubridate::dminutes(40) & 
      ti$sunset - ti$localTime >= lubridate::dminutes(30)) {
    par(bg = "gray95")
  } else if (ti$sunset - ti$localTime < lubridate::dminutes(30) & 
             ti$sunset - ti$localTime >= lubridate::dminutes(20)) {
    par(bg = "gray90")
  } else if (ti$sunset - ti$localTime < lubridate::dminutes(20) & 
             ti$sunset - ti$localTime >= lubridate::dminutes(10)) {
    par(bg = "gray85")
  } else if (ti$sunset - ti$localTime < lubridate::dminutes(10) & 
             ti$sunset - ti$localTime >= lubridate::dminutes(0)) {
    par(bg = "gray80")
  } else if (ti$localTime - ti$sunset <= lubridate::dminutes(0) & 
             ti$localTime - ti$sunset > lubridate::dminutes(10)) {
    par(bg = "gray75")
  } else if (ti$localTime - ti$sunset <= lubridate::dminutes(10) & 
             ti$localTime - ti$sunset > lubridate::dminutes(20)) {
    par(bg = "gray70")
  } else if (ti$localTime - ti$sunset <= lubridate::dminutes(20) & 
             ti$localTime - ti$sunset > lubridate::dminutes(30)) {
    par(bg = "gray65")
  } 
  
  # sunrise
  if (ti$sunrise - ti$localTime < lubridate::dminutes(30) & 
      ti$sunrise - ti$localTime >= lubridate::dminutes(20)) {
    par(bg = "gray65")
  } else if (ti$sunrise - ti$localTime < lubridate::dminutes(20) & 
             ti$sunrise - ti$localTime >= lubridate::dminutes(10) ) {
    par(bg = "gray70")
  } else if (ti$sunrise - ti$localTime < lubridate::dminutes(10) & 
             ti$sunrise - ti$localTime >= lubridate::dminutes(0) ) {
    par(bg = "gray75")
  } else if (ti$localTime - ti$sunrise <= lubridate::dminutes(10) & 
             ti$localTime - ti$sunrise > lubridate::dminutes(0) ) {
    par(bg = "gray80")
  } else if (ti$localTime - ti$sunrise <= lubridate::dminutes(20) & 
             ti$localTime - ti$sunrise > lubridate::dminutes(10) ) {
    par(bg = "gray85")
  } else if (ti$localTime - ti$sunrise <= lubridate::dminutes(30) & 
             ti$localTime - ti$sunrise > lubridate::dminutes(20) ) {
    par(bg = "gray90")
  } else if (ti$localTime - ti$sunrise <= lubridate::dminutes(40) & 
             ti$localTime - ti$sunrise > lubridate::dminutes(30) ) {
    par(bg = "gray95")
  }
  
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
  raster::text(right - (right - left) / 2, top + (usr[4] - usr[3]) / 16,
               labels = "PM 2.5", font = 2,  cex = 2.8)
  # Disabled to keep units ambiguous
  #raster::text(right - (right - left) / 2, top + (usr[4] - usr[3]) / 16, 
  #     labels = "(\U03BCg/m\U00B3)", cex = 1.4)
  #text(left, bottom,  as.character(0),  pos = 2, cex = 1.5)
  #text(left, top,     as.character(60), pos = 2, cex = 1.5)
  
  # ----- Plot points ----------------------------------------------------------
  
  colors = PWFSLSmoke::aqiColors(data$pm25, 
                                 palette = colorPalette, 
                                 domain = c(0, 250),
                                 bins = colorBins)
  
  points(x = data$longitude,
         y = data$latitude,
         col = colors,
         pch = 16, 
         cex = 7)
  
  # ----- Plot time axis -------------------------------------------------------
  
  if (!is.null(timeAxis) && !is.null(timeTicks) && !is.null(timeLabels)) {
    par(mar = c(0.5, 0.5, 6, 0.5))
    plot(rep(0, length(timeAxis)), -(as.numeric(timeAxis)), axes = FALSE, 
         col = 'transparent')
    
    mtext(strftime(frameTime, "%b %e", tz = "America/Los_Angeles") , line = 2.6, 
          cex = 2.7)
    mtext(strftime(frameTime, "%l %P", tz = "America/Los_Angeles") , line = 0.5, 
          cex = 2.0)
    
    axis(side = 2, labels = timeLabels, line = -5.5, at = -as.numeric(timeTicks), 
         cex.axis = 1.6, las = 2, hadj = 1, cex= 2, lwd.ticks = 2.7,
         lwd = 2.7)
    axis(side = 4, line = -3.9, at = -as.numeric(frameTime), col = 'red', 
         col.ticks = 2, lwd.ticks = 12, labels = "", tcl = -2)
  }
  
  # ----- Plot logo ------------------------------------------------------------
  
  if ( !is.null(logo) ) {
    # Center position 
    x = right - (right - left) * 1.25
    y = top + (usr[4] - usr[3]) * 0.3
    
    # Width and height
    degreesPerInchEW <- (usr[2] - usr[1]) %% 360 / par("pin")[1]
    degreesPerInchNS <- (usr[4] - usr[3]) %% 360 / par("pin")[2]
    
    # Estimate conversion from inch to pixel (assuming dpi ~ 96)
    degreesPerPixelEW <- degreesPerInchEW / 96
    degreesPerPixelNS <- degreesPerInchNS / 96
    
    width <- degreesPerPixelEW * dim(logo)[2]*1
    height <- degreesPerPixelNS * dim(logo)[1]*1
    
    l <- x - width / 2
    r <- x + width / 2
    b <- y - height / 2
    t <- y + height / 2
    
    graphics::rasterImage(logo, l, b, r, t)
  }
  
}
