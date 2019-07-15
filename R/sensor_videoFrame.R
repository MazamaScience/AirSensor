#' @export
#' 
#' @title Generate video frames
#' 
#' @param sensor An AirSensor object.
#' @param communityRegion The area to display.
#' @param frameTime POSIXct specifying the time to use.
#' @param timeInfo ???.
#' @param map A pre-generated basemap image.
#' 
#' @description desc
#' 
#' @return ret


sensor_videoFrame <- function(sensor = sensor_load(),
                              communityRegion = NULL,
                              frameTime = NULL,
                              timeInfo = NULL,
                              map = NULL) {
  
  if (TRUE) {
    commReg <- "Seal Beach"
    frameTime <- lubridate::ymd_hms("2019-07-04 18:00:00")
    
    centerLon = -118.082
    centerLat = 33.767
    width =  770
    height = 495
    zoom <- 15
    degreesPerPixelEW <- 360/(256*2^zoom)
    degreesPerPixelNS <- degreesPerPixelEW*cos(pi/180*centerLat) # R does trigonometry in radians
    lonLo <- centerLon - degreesPerPixelEW*(width/2-.5)
    lonHi <- centerLon + degreesPerPixelEW*(width/2-.5)
    latLo <- centerLat-degreesPerPixelNS*(height/2-.5)
    latHi <- centerLat+degreesPerPixelNS*(height/2-.5)
    bbox <- c(lonLo, latLo, lonHi, latHi)
    bboxString <- paste0(bbox, collapse=",")
    url <- paste0("http://server.arcgisonline.com/arcgis/rest/services/Canvas/World_Light_Gray_Base/MapServer/export?bbox=", bboxString,"&bboxSR=4326&size=770,495")
    jsonUrl <- paste0(url, "&f=json")
    pngUrl <- paste0(url, "&f=image")
    mapInfo <- jsonlite::fromJSON(jsonUrl)
    response <- httr::GET(pngUrl)
    imageArray <- httr::content(response, type="image/png")
    mapRaster <- raster::brick(ncol=mapInfo$width, nrow=mapInfo$height, nl = 3)
    mapRaster <- raster::setValues(mapRaster, imageArray*255)
    names(mapRaster) <- c("red", "green", "blue")
    raster::extent(mapRaster) <- c(mapInfo$extent$xmin, mapInfo$extent$xmax, mapInfo$extent$ymin, mapInfo$extent$ymax)
    raster::crs(mapRaster) <- sp::CRS(paste0("+init=epsg:",mapInfo$extent$spatialReference$latestWkid))
    map <- mapRaster
    
    # Ideally just use this...
    # map <- PWFSLSmoke::esriMap_getMap(centerLon = -118.082, centerLat = 33.767,
    #                                  maptype = "natGeo",
    #                                  zoom = 15,
    #                                  width = 770, height = 495)
  }
  
  layout(matrix(c(2,1), 1, 2), heights = 1, widths = c(1, 7))
  
  # Round frameTime to closest hour
  frameTime <- lubridate::round_date(frameTime, unit = "hour")
  
  # Subset and format data
  community <- dplyr::filter(sensor$meta, .data$communityRegion == commReg)
  monitorId <- community$monitorID
  longitude <- community$longitude
  latitude <- community$latitude
  
  data <- sensor$data %>% 
    dplyr::select(.data$datetime, monitorId) %>% 
    dplyr::filter(.data$datetime == frameTime) %>%
    dplyr::select(-datetime) %>%
    tidyr::gather(key = "monitorID", value = "pm25")
  pm25 <- data$pm25
  
  data <- data.frame(monitorId, longitude, latitude, pm25)
  
  # Map
  par(mar = c(0, 0, 0, 0))
  PWFSLSmoke::esriMap_plotOnStaticMap(map)
  
  # Dots
  colorPalette <- grDevices::colorRampPalette(c("#22f777", "yellow", "gold",  
                                                "#f19100", "#f15800", "#cc3702"))(30)
  colorBins <-  c(seq(0, 60, length = 24), 300)
  colors = PWFSLSmoke::aqiColors(data$pm25, 
                                 palette = colorPalette, 
                                 domain = c(0, 250),
                                 bins = colorBins)
  
  points(x = data$longitude,
         y = data$latitude,
         col = colors,
         pch = 16, 
         cex = 16)
  
}
