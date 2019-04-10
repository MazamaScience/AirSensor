#' @keywords pa_synoptic
#' @export
#' @title Create an ESRI Map of ws_monitor Object
#' @param pas \emph{pa_synoptic} object
#' @param centerLon map center longitude
#' @param centerLat map center latitude
#' @param zoom map zoom level
#' @param mapRaster optional RGB Raster* object returned from
#' @param showMap option to show the ggplot if true
#' @return Plots a map loaded from arcGIS REST with points for each monitor
#' @description Creates an ESRI map of a \emph{pa_synoptic} object.
#'
#' #' Available \code{maptypes} include:
#' \itemize{
#' \item{natGeo}
#' \item{worldStreetMap}
#' \item{worldTopoMap}
#' \item{satellite}
#' \item{deLorme}
#' }
#'
#' Additional base maps are found at:
#' \url{http://resources.arcgis.com/en/help/arcgis-rest-api/index.html#/Basemaps/02r3000001mt000000/}
#'
#' If \code{centerLon}, \code{centerMap} or \code{zoom} are not specified,
#' appropriate values will be calcualted using data from the
#' \code{pa_synoptic} dataframe.
#' @examples
#' \dontrun{
#' pas <- pas_load()
#' CA <- pas %>% filter(stateCode == "CA")
#' pas_esriMap(CA, showMap=TRUE)
#' }
#' @seealso \code{\link{esriMap_plotOnStaticMap}}

pas_esriMap <- function(
  pas, 
  centerLon=NULL,
  centerLat=NULL,
  maptype="worldStreetMap",
  mapRaster=NULL, 
  zoom=NULL,
  showMap=FALSE,
  ...) {
  
  if( nrow(pas) == 0 ) {
    stop("Empty pa_synoptic")
  }
  
  # ----- Determine coordinate centeroid ---------------------------------------
  
  if ( is.null(centerLon) ) {
    centerLon <- base::mean(pas$longitude)
  }
  if ( is.null(centerLat) ) {
    centerLat <- base::mean(pas$latitude)
  }
  
  # ----- Determine map zoom ---------------------------------------------------
  
  if ( is.null(zoom) ) { 
    maxRange <- max(
      diff(range(pas$longitude, na.rm = TRUE)),
      diff(range(pas$latitude, na.rm = TRUE)) )
    
    zoom <- (0.02974 * log10(maxRange) + 0.1355)**(-1)
  }
  
  # ----- Generate RGB Raster --------------------------------------------------
  
  if ( is.null(mapRaster) ) {
    mapRaster <- PWFSLSmoke::esriMap_getMap(
      centerLon, 
      centerLat, 
      width = 800, 
      height = 800,
      zoom = zoom, 
      maptype = maptype, 
      crs = sp::CRS("+init=epsg:4326") )
  }
  
  #----- Generate ggobject with mapRaster --------------------------------------
  colorFunc <- leaflet::colorBin( PWFSLSmoke::AQI$colors, 
                                  bins = PWFSLSmoke::AQI$breaks_24, 
                                  na.color = "#bbbbbb" )
  colors <- PWFSLSmoke::AQI$colors
  labels <- PWFSLSmoke::AQI$names
  ggRasterPlot <- 
    RStoolbox::ggRGB(mapRaster, r=1, g=2, b=3, maxpixels = 2e+06) + 
    ggplot2::geom_point(ggplot2::aes( x = pas$longitude, 
                                      y = pas$latitude, 
                                      color = colorFunc(pas$pm25_1hr) ),
                        shape = 15, 
                        size = 2., 
                        alpha = 0.75) + 
    ggplot2::scale_color_manual( name = "AQI", 
                                 values = c(colors, "#bbbbbb"), 
                                 labels = c(labels, "Missing") ) +
    ggplot2::coord_fixed(ratio = 4/3) +
    ggplot2::theme_void()
  
  if ( showMap )( show(ggRasterPlot) )
  
  return(ggRasterPlot)
  
}
