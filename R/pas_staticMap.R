#' @keywords pa_synoptic
#' 
#' @export
#' 
#' @title Static map of Purple Air sensors
#' 
#' @param pas \emph{pa_synoptic} object
#' @param theme base color or palette to be used. 
#' @param mapTheme default is "terrain", see description for additional options
#' @param mapShape default is "square", can also be "natural"
#' @param direction legend color direction
#' @param minScale default is 0, minimum value to set scale for color gradient
#' @param maxScale default is 150, maximum value to set scale for color gradient
#' @param shape symbol to use for points
#' @param size size of points
#' @param alpha opacity of points
#' @param bbuff bounding box buffer, default is 0.1
#' @param zoom map zoom level
#' @param ... additional options: the legend can disabled \code{guide = FALSE}
#'   
#' @return ggplot object 
#' 
#' @description Creates a static map of a \emph{pa_synoptic} object.
#' 
#' 
#' Available \code{theme} options include an \code{"AQI"} color palette, 
#' Additional sequential and diverging palettes are avaliable as well.
#' 
#' The sequential palettes names are
#'  
#' \code{"Blues" "BuGn" "BuPu" "GnBu" "Greens" "Greys" "Oranges" "OrRd" "PuBu" 
#'  "PuBuGn" "PuRd" "Purples" "RdPu" "Reds" "YlGn" "YlGnBu" "YlOrBr" "YlOrRd"}
#' 
#' The diverging palettes are 
#' 
#' \code{"BrBG" "PiYG" "PRGn" "PuOr" "RdBu" "RdGy" "RdYlBu" "RdYlGn" "Spectral"}
#' 
#'
#' 
#' Additional map tile info found at:
#' \url{http://maps.stamen.com/}
#'
#' @examples
#' \dontrun{
#' wa <- pas_load() %>% filter(stateCode == "WA")
#' pas_staticMap(wa, theme = "Spectral", direction = -1)
#' }
#' \dontrun{
#' pas %>%
#'   pas_filter(stateCode == "CA" & pm25 > 75) %>%
#'   pas_esriMap(theme = "Purples", maxScale = 250)
#' }
#' 

pas_staticMap <- function(
  pas, 
  param = "pm25",
  theme = "Purples", 
  mapTheme = "terrain",
  mapShape = "sq",
  direction = 1,
  minScale = 0,
  maxScale = 150, 
  shape = 15, 
  size = 2.0, 
  alpha = 0.8, 
  bbuff = 0.1, 
  zoom = 8, 
  ...
) {
  
  if ( pas_isEmpty(pas) ) {
    stop("Required parameter 'pas' is empty.")
  }
  
  # ----- Color Scale Theme ----------------------------------------------------
  
  if ( tolower(theme) == "aqi" ) { 
    
    colorFunc <- 
      leaflet::colorBin( 
        PWFSLSmoke::AQI$colors, 
        bins = PWFSLSmoke::AQI$breaks_24, 
        na.color = "grey50" 
      )
    
    AQI_color <- c(PWFSLSmoke::AQI$colors, "grey50")
    AQI_label <- c(PWFSLSmoke::AQI$names, "Missing")
    
    sensorColor <- colorFunc(pas[[param]])
    
    colorPalette <- 
      ggplot2::scale_color_identity(
        "AQI",
        labels = AQI_label,
        breaks = AQI_color,
        guide = "legend"
      )
    
  } else { 
    
    if ( param == "pm25" ) {
      name <- "\u03bcg / m\u00b3"
    } else if ( param == "temperature") { 
      name <- "\u00b0F"
    } else if ( param == "humidity" ) { 
      name <- "RH%"
    } else { 
      name <- ""
    }
    colorBrew <- 
      grDevices::colorRampPalette(
        RColorBrewer::brewer.pal(
          n = 8, 
          name = theme
        )
      )
    
    colors <- colorBrew(8)
    if ( direction == -1 ) ( rev(colors) -> colors )
    
    sensorColor <- pas[[param]]
    
    colorPalette <- 
      ggplot2::scale_color_gradientn(
        name = name,
        colors = colors, 
        limits = c(minScale,maxScale), 
        oob = scales::squish, 
        na.value = "grey50", 
        ...
      )
    
  }
  
  # ----- Determine Coordinate Bounding Box ------------------------------------ 
  
  maxLat <- max(pas$latitude)
  minLat <- min(pas$latitude)
  maxLong <- max(pas$longitude)
  minLong <- min(pas$longitude)
  
  height <- maxLat - minLat
  width <- maxLong - minLong
  
  if ( tolower(mapShape) == "sq" || tolower(mapShape) == "square" ) {
    
    centerLon <- minLong + 0.5 * width
    centerLat <- minLat + 0.5 * height 
    
    coordDomain <- cbind(pas$longitude, pas$latitude)
    
    centroid <- cbind(centerLon , centerLat)
    
    distance <- geosphere::distHaversine(centroid, coordDomain)
    
    b <- geosphere::destPoint(centroid, 180, d = max(distance))
    t <- geosphere::destPoint(centroid, 0, d = max(distance))
    l <- geosphere::destPoint(centroid, 270, d = max(distance))
    r <- geosphere::destPoint(centroid, 90, d = max(distance))
    
    bbox <- 
      c(
        bottom = b[2] - bbuff * height, 
        top = t[2] + bbuff * height, 
        left = l[1] - bbuff * width, 
        right = r[1] + bbuff * width
      )
    
  } else if ( tolower(mapShape) == "nt" || tolower(mapShape) == "natural" ) { 
    
    height <- maxLat - minLat
    width <- maxLong - minLong
    
    bbox <- 
      c(
        bottom = minLat - bbuff  * height, 
        top = maxLat + bbuff * height, 
        left = minLong - bbuff * width, 
        right = maxLong + bbuff * width
      )
    
  } 
  
  # ---- PA Sensor Points ------------------------------------------------------
  
  paSensors <- 
    ggplot2::geom_point(
      data = pas,
      mapping = ggplot2::aes(
        x = .data$longitude, 
        y = .data$latitude, 
        color = sensorColor
      ), 
      size = size, 
      shape = shape, 
      alpha = alpha
    )
  
  # ----- Construct Stamen Maps ------------------------------------------------
  
  stamenMap <- 
    ggmap::get_stamenmap(
      bbox = bbox, 
      zoom = zoom, 
      maptype = mapTheme,
      ...
    ) %>% 
    ggmap::ggmap()
  
  # ----- Construct Static Map -------------------------------------------------
  
  staticMap <- 
    stamenMap + 
    paSensors + 
    colorPalette + 
    ggplot2::theme_void()
  
  return(staticMap)
  
}
