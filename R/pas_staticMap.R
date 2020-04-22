#' @keywords pa_synoptic
#' 
#' @export
#' 
#' @title Static map of PurpleAir sensors
#' 
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param parameter Value to plot, e.g. \code{pm25_1hr}.
#' @param paletteName Base color or palette name to be used. 
#' @param mapTheme Default is "terrain", see description for additional options.
#' @param mapShape Default is "square", can also be "natural".
#' @param direction Legend color direction.
#' @param minScale Minimum value to set scale for color gradient. Default is 0.
#' @param maxScale Maximum value to set scale for color gradient. Default is 150.
#' @param shape Symbol to use for points.
#' @param size Size of points.
#' @param alpha Opacity of points.
#' @param bbuff Bounding box buffer. Default is 0.1.
#' @param zoomAdjust Adjustment to map zoom level (-1:3).
#' @param ... Additional options: the legend can disabled \code{guide = FALSE}, 
#'   and renamed with \code{name= "Example name"}. 
#'   
#' @return A ggplot object.
#' 
#' @description Creates a static map of a \emph{pas} object
#' 
#' Users can create a map using any numeric data column within the
#' \emph{pas} object:
#' 
#' \code{"pm25" "temperature" "humidity" "pressure" 
#'  "pm25_current" "pm25_10min" "pm25_30min" "pm25_1hr" "pm25_6hr" 
#'  "pm25_1day" "pm25_1week" "pwfsl_closestDistance"}
#' 
#' Available \code{paletteName} options include an \code{"AQI"} color palette, 
#' as well as a suite of sequential and diverging palettes from the 
#' \pkg{RColorBrewer} R package.
#' 
#' The sequential palette names are
#'  
#' \code{"Blues" "BuGn" "BuPu" "GnBu" "Greens" "Greys" "Oranges" "OrRd" "PuBu" 
#'  "PuBuGn" "PuRd" "Purples" "RdPu" "Reds" "YlGn" "YlGnBu" "YlOrBr" "YlOrRd"}
#' 
#' The diverging palette names are 
#' 
#' \code{"BrBG" "PiYG" "PRGn" "PuOr" "RdBu" "RdGy" "RdYlBu" "RdYlGn" "Spectral"}
#' 
#' Additional map tile info found at:
#' \url{http://maps.stamen.com/}
#'
#' @examples
#' LA_basin <- 
#'   example_pas %>% 
#'   pas_filterArea(-118.5, -117.5, 33.5, 34.5)
#' pas_staticMap(LA_basin, paletteName = "AQI", zoomAdjust = 1)

pas_staticMap <- function(
  pas = NULL, 
  parameter = "pm25_1hr",
  paletteName = "Purples", 
  mapTheme = "terrain",
  mapShape = "sq",
  direction = 1,
  minScale = 0,
  maxScale = 150,
  shape = 15, 
  size = 2.0, 
  alpha = 0.8, 
  bbuff = 0.5, 
  zoomAdjust = 0, 
  ...
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pas)
  MazamaCoreUtils::stopIfNull(parameter)
  
  parameter <- tolower(parameter)
  
  if ( pas_isEmpty(pas) ) {
    stop("Required parameter 'pas' is empty.")
  }
  
  if ( !pas_isPas(pas) ) {
    stop("Required parameter 'pas' is not a valid 'pa_synoptic' object.")
  }
  
  # ----- Color scale ----------------------------------------------------------
  
  if ( tolower(paletteName) == "aqi" ) { # AQI COLORS
    
    options(warn = -1)
    
    colorInfo <- pas_palette(pas, paletteName)
    
    colors <- colorInfo$colors
    breaks <- c(colorInfo$key[,2], "grey50")
    labels <- c(colorInfo$key[,1], "Missing")
    sensorColor <- colorInfo$colors
    
    colorScale <- 
      ggplot2::scale_color_identity(
        "AQI",
        labels = labels,
        breaks = breaks,
        guide = "legend"
      )
    
  } else if ( tolower(paletteName) == "humidity"  ) { # HUMIDITY 
    
    colorInfo <- pas_palette(pas, "humidity")
    
    colors <- colorInfo$colors
    breaks <- c(colorInfo$key[,2], "grey50")
    labels <- c(colorInfo$key[,1], "Missing")
    sensorColor <- colorInfo$colors
    
    colorScale <- 
      ggplot2::scale_color_identity(
        "Humidity",
        labels = labels,
        breaks = breaks,
        guide = "legend"
      )
    
  }  else if ( tolower(paletteName) == "temperature" ) { # TEMPERATURE
    
    colorInfo <- pas_palette(pas, "temperature")
    
    colors <- colorInfo$colors
    breaks <- c(colorInfo$key[,2], "grey50")
    labels <- c(colorInfo$key[,1], "Missing")
    sensorColor <- colorInfo$colors
    
    colorScale <- 
      ggplot2::scale_color_identity(
        "Temperature",
        labels = labels,
        breaks = breaks,
        guide = "legend"
      )
    
    options(warn = 0)
  } else if ( tolower(paletteName) == "distance" ) { # PWFLS distance
    
    colorInfo <- pas_palette(pas, "distance")
    
    colors <- colorInfo$colors
    breaks <- c(colorInfo$key[,2], "grey50")
    labels <- c(colorInfo$key[,1], "Missing")
    sensorColor <- colorInfo$colors
    
    colorScale <- 
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
          name = paletteName
        )
      )
    
    colors <- colorBrew(8)
    if ( direction == -1 ) ( rev(colors) -> colors )
    
    sensorColor <- pas[[parameter]]
    
    colorScale <- 
      ggplot2::scale_color_gradientn(
        colors = colors, 
        limits = c(minScale, maxScale), 
        oob = scales::squish, 
        na.value = "grey50", 
        ...
      )
    
  }
  
  # ----- Determine bounding box ----------------------------------------------- 
  
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
    
    colnames(coordDomain) <- colnames(centroid) <- c("x", "y")
    
    distance <- diag(geodist::geodist(centroid, coordDomain))
    
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
  
  # ---- PA sensor points ------------------------------------------------------
  
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
  
  # ----- Construct Stamen basemap ---------------------------------------------
  
  suppressMessages({
    
    stamenMap <- 
      ggmap::get_stamenmap(
        bbox = bbox, 
        zoom = zoom, 
        maptype = mapTheme,
        ...
      ) %>% 
      ggmap::ggmap()
    
  })
  
  # ----- Construct static map -------------------------------------------------
  
  # TODO: Find a way to show the complete legend.
  
  staticMap <- 
    stamenMap + 
    paSensors + 
    colorScale + 
    ggplot2::theme_void()
  
  # ----- Return ---------------------------------------------------------------
  
  return(staticMap)
  
}
