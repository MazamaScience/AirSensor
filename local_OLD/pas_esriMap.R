#' @keywords pa_synoptic
#' 
#' @export
#' 
#' @title ESRI static map of Purple Air sensors
#' 
#' @param pas \emph{pa_synoptic} object
#' @param centerLon map center longitude
#' @param centerLat map center latitude
#' @param maptype map type
#' @param mapRaster optional RGB Raster object to be used as the map layer
#' @param width width of image in pixels
#' @param height height of image in pixels
#' @param zoom map zoom level
#' @param shape symbol to use for points
#' @param size size of points
#' @param alpha opacity of points
#' @param theme base color or palette to be used. 
#' @param scaleMin default is 0, minimum value to set scale for color gradient
#' @param scaleMax default is 150, maximum value to set scale for color gradient
#'   
#' @return Plots a map loaded from arcGIS REST with points for each monitor.
#' 
#' @description Creates an ESRI map of a \emph{pa_synoptic} object.
#' 
#' Available \code{maptype} options include:
#' \itemize{
#' \item{natGeo}
#' \item{worldStreetMap}
#' \item{worldTopoMap}
#' \item{satellite}
#' \item{deLorme}
#' }
#' 
#' Available \code{theme} options include sequential and diverging palettes:
#' 
#' The sequential palettes names are
#'  
#' \code{"Blues" "BuGn" "BuPu" "GnBu" "Greens" "Greys" "Oranges" "OrRd" "PuBu" 
#' "PuBuGn" "PuRd" "Purples" "RdPu" "Reds" "YlGn" "YlGnBu" "YlOrBr" "YlOrRd"}
#' 
#' The diverging palettes are 
#' 
#' \code{"BrBG" "PiYG" "PRGn" "PuOr" "RdBu" "RdGy" "RdYlBu" "RdYlGn" "Spectral"}
#' 
#' 
#' Additional base maps are found at:
#' \url{https://developers.arcgis.com/}
#'
#' If \code{centerLon}, \code{centerMap} or \code{zoom} are not specified,
#' appropriate values will be calcualted using data from the
#' \code{pa_synoptic} dataframe.
#' @examples
#' \dontrun{
#' ca <- pas_load() %>% filter(stateCode == "CA")
#' pas_esriMap(ca)
#' }
#' \dontrun{
#' pas %>%
#'   pas_filter(stateCode == "CA" & pm25 > 75) %>%
#'   pas_esriMap(theme = "Purples", scaleMax = 250)
#' }
#' 
#' @seealso \code{\link{esriMap_plotOnStaticMap}}

pas_esriMap <- function(
  pas,
  centerLon = NULL,
  centerLat = NULL,
  maptype = "worldStreetMap",
  mapRaster = NULL, 
  width = 800,
  height = 800,
  zoom = NULL,
  shape = 15, 
  size = 4.0, 
  alpha = 0.8, 
  theme = "Purples", 
  scaleMin = 0,
  scaleMax = 150
) {
  
  if ( nrow(pas) == 0 ) {
    stop("Required parameter 'pas' is empty.")
  }
  
  # ----- Determine coordinate centeroid ---------------------------------------
  
  if ( is.null(centerLon) ) {
    xlim <- range(pas$longitude, na.rm = TRUE)
    centerLon <- xlim[1] + 0.5 * diff(xlim)
  }
  if ( is.null(centerLat) ) {
    ylim <- range(pas$latitude, na.rm = TRUE)
    centerLat <- ylim[1] + 0.5 * diff(ylim)
  }
  
  # ----- Determine map zoom ---------------------------------------------------
  
  if ( is.null(zoom) ) { 
    maxRange <- max(
      diff(range(pas$longitude, na.rm = TRUE)),
      diff(range(pas$latitude, na.rm = TRUE)) 
    )
    
    if ( maxRange > 50 ) {
      zoom <- 4
    } else if ( maxRange > 20 ) {
      zoom <- 5
    } else if ( maxRange > 10 ) {
      zoom <- 6
    } else if ( maxRange > 5 ) {
      zoom <- 7
    } else if ( maxRange > 2 ) {
      zoom <- 8
    } else if ( maxRange > 1 ) {
      zoom <- 9
    } else if ( maxRange > 0.5 ) {
      zoom <- 10
    } else if ( maxRange > 0.2 ) {
      zoom <- 11
    } else if ( maxRange > 0.1 ) {
      zoom <- 12
    } else if ( maxRange > 0.05 ) {
      zoom <- 13
    } else {
      zoom <- 14
    }
    
  } else {
    zoom <- round(zoom)
  }
  
  # ----- Generate RGB Raster --------------------------------------------------
  
  if ( is.null(mapRaster) ) {
    mapRaster <- PWFSLSmoke::esriMap_getMap(
      centerLon, 
      centerLat, 
      width = width, 
      height = height,
      zoom = zoom, 
      maptype = maptype, 
      crs = sp::CRS("+init=epsg:4326") )
  }
  
  # ----- Generate Color Palette -----------------------------------------------
  
  if ( tolower(theme) == "aqi" ) { 
    
    colorFunc <- 
      leaflet::colorBin( 
        PWFSLSmoke::AQI$colors, 
        bins = PWFSLSmoke::AQI$breaks_24, 
        na.color = "#bbbbbb" 
      )
    
    AQI_color <- c(PWFSLSmoke::AQI$colors, "#bbbbbb")
    AQI_label <- c(PWFSLSmoke::AQI$names, "Missing")

    colors <- colorFunc(pas$pm25_1hr)
    
    colorGradient <- 
      ggplot2::scale_color_identity(
        "AQI",
        labels = AQI_label,
        breaks = AQI_color,
        guide = "legend"
      )
    
  
  } else { 
    
    colors <- pas$pm25
    palette <- 
      grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 8, name = theme))

    colorGradient <- 
      ggplot2::scale_colour_gradientn(
        "\u03bcg / m\u00b3",
        colors = palette(8), 
        limits = c(scaleMin, scaleMax), 
        oob = scales::squish, 
        na.value = "grey50"
      )
    
  }

  
  #----- Generate ggobject with mapRaster --------------------------------------

  
  # Suppress "Coordinate system already present." message
  suppressMessages({
    
    ggRasterPlot <- 
      RStoolbox::ggRGB(mapRaster, r = 1, g = 2, b = 3, maxpixels = 5e+06) + 
      ggplot2::geom_point(
        ggplot2::aes( 
          x = pas$longitude, 
          y = pas$latitude, 
          color = colors),
        shape = shape, 
        size = size, 
        alpha = alpha) +   
        ggplot2::coord_fixed(ratio = 4/3) +   # TODO:  comment on purpose
        ggplot2::theme_void() + 
        colorGradient 
        
  })
  
  return(ggRasterPlot)
  
}
