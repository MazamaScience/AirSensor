#' @keywords pa_synoptic
#' 
#' @export
#' 
#' @title Static map of Purple Air sensors
#' 
#' @param pas \emph{pa_synoptic} object
#' @param parameter Value to plot, e.g. \code{pm25_1hr}.
#' @param palette base color or palette to be used. 
#' @param mapTheme default is "terrain", see description for additional options
#' @param mapShape default is "square", can also be "natural"
#' @param direction legend color direction
#' @param minScale default is 0, minimum value to set scale for color gradient
#' @param maxScale default is 150, maximum value to set scale for color gradient
#' @param shape symbol to use for points
#' @param size size of points
#' @param alpha opacity of points
#' @param bbuff bounding box buffer, default is 0.1
#' @param zoomAdjust adjustment to map zoom level (-1:3)
#' @param ... additional options: the legend can disabled \code{guide = FALSE}, 
#' and renamed with \code{name= "Example name"}. 
#'   
#' @return ggplot object 
#' 
#' @description Creates a static map of a \emph{pa_synoptic} object.
#' 
#' Users can create a map using any numeric data column within the
#' \emph{pa_synoptic} object:
#' 
#' \code{"pm25" "temperature" "humidity" "pressure" 
#'  "pm25_current" "pm25_10min" "pm25_30min" "pm25_1hr" "pm25_6hr" 
#'  "pm25_1day" "pm25_1week" "pwfsl_closestDistance"}
#' 
#' Available \code{palette} options include an \code{"AQI"} color palette, 
#' as well as a suite of sequential and diverging palettes from the 
#' \pkg{RColorBrewer} R package.
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
#' Additional map tile info found at:
#' \url{http://maps.stamen.com/}
#'
#' @examples
#' \dontrun{
#' LA_basin <- pas_load() %>% pas_filterArea(-118.5, -117.5, 33.5, 34.5)
#' pas_staticMap(LA_basin, palette = "AQI", zoomAdjust = 1)
#' }
#' 

pas_staticMap <- function(
  pas, 
  parameter = "pm25_1hr",
  palette = "Purples", 
  mapTheme = "terrain",
  mapShape = "sq",
  direction = 1,
  minScale = 0,
  maxScale = 150, 
  shape = 15, 
  size = 2.0, 
  alpha = 0.8, 
  bbuff = 0.1, 
  zoomAdjust = 0, 
  ...
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  if ( pas_isEmpty(pas) ) {
    stop("Required parameter 'pas' is empty.")
  }
  
  # ----- Color Scale Theme ----------------------------------------------------
  
  if ( tolower(palette) == "aqi" ) { # AQI COLORS
    
    options(warn = -1)
    
    colorInfo <- pas_palette(pas, palette)
    
    colors <- colorInfo$colors
    breaks <- c(colorInfo$key[,2], "grey50")
    labels <- c(colorInfo$key[,1], "Missing")
    sensorColor <- colorInfo$colors
    
    colorPalette <- 
      ggplot2::scale_color_identity(
        "AQI",
        labels = labels,
        breaks = breaks,
        guide = "legend"
      )

  } else if ( tolower(palette) == "humidity"  ) { # HUMIDITY 
    
    colorInfo <- pas_palette(pas, "humidity")
    
    colors <- colorInfo$colors
    breaks <- c(colorInfo$key[,2], "grey50")
    labels <- c(colorInfo$key[,1], "Missing")
    sensorColor <- colorInfo$colors
    
    colorPalette <- 
      ggplot2::scale_color_identity(
        "Humidity",
        labels = labels,
        breaks = breaks,
        guide = "legend"
      )
    
  }  else if ( tolower(palette) == "temperature" ) { # TEMPERATURE
    
    colorInfo <- pas_palette(pas, "temperature")
    
    colors <- colorInfo$colors
    breaks <- c(colorInfo$key[,2], "grey50")
    labels <- c(colorInfo$key[,1], "Missing")
    sensorColor <- colorInfo$colors
    
    colorPalette <- 
      ggplot2::scale_color_identity(
        "Temperature",
        labels = labels,
        breaks = breaks,
        guide = "legend"
      )
    
    options(warn = 0)
  } else if ( tolower(palette) == "distance" ) { # PWFLS distance
    
    colorInfo <- pas_palette(pas, "distance")
    
    colors <- colorInfo$colors
    breaks <- c(colorInfo$key[,2], "grey50")
    labels <- c(colorInfo$key[,1], "Missing")
    sensorColor <- colorInfo$colors
    
    colorPalette <- 
      ggplot2::scale_color_identity(
        "Distance",
        labels = labels,
        breaks = breaks,
        guide = "legend"
      )
    
    options(warn = 0)
    
  } else { # OTHER PALETTE HANDLING
    
    colorBrew <- 
      grDevices::colorRampPalette(
        RColorBrewer::brewer.pal(
          n = 8, 
          name = palette
        )
      )
    
    colors <- colorBrew(8)
    if ( direction == -1 ) ( rev(colors) -> colors )
    
    sensorColor <- pas[[parameter]]
    
    colorPalette <- 
      ggplot2::scale_color_gradientn(
        colors = colors, 
        limits = c(minScale, maxScale), 
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
  
  # ----- Determine map zoom ---------------------------------------------------
  
  maxRange <- max(
    diff(range(pas$longitude, na.rm = TRUE)),
    diff(range(pas$latitude, na.rm = TRUE)) 
  )
  
  if ( maxRange > 50 ) {
    zoom <- 3
  } else if ( maxRange > 20 ) {
    zoom <- 4
  } else if ( maxRange > 10 ) {
    zoom <- 5
  } else if ( maxRange > 5 ) {
    zoom <- 6
  } else if ( maxRange > 2 ) {
    zoom <- 7
  } else if ( maxRange > 1 ) {
    zoom <- 8
  } else if ( maxRange > 0.5 ) {
    zoom <- 9
  } else if ( maxRange > 0.2 ) {
    zoom <- 10
  } else if ( maxRange > 0.1 ) {
    zoom <- 11
  } else if ( maxRange > 0.05 ) {
    zoom <- 12
  } else {
    zoom <- 13
  }
  
  zoom <- zoom + zoomAdjust
  
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
  
  # TODO: Find a way to show the complete legend.
  
  staticMap <- 
    stamenMap + 
    paSensors + 
    colorPalette + 
    ggplot2::theme_void()
  
  return(staticMap)
  
}