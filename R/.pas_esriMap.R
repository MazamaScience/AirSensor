#' @keywords pa_synoptic
#' @title Create an ESRI Map of ws_monitor Object
#' @param pas \emph{pa_synoptic} object
#' @param centerLon map center longitude
#' @param centerLat map center latitude
#' @param zoom map zoom level
#' @param mapRaster optional RGB Raster* object returned from
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
#' pas_esriMap(CA)
#' }
#' @seealso \code{\link{esriMap_plotOnStaticMap}}

pas_esriMap <- function(
  pas, 
  centerLon=NULL,
  centerLat=NULL,
  maptype="worldStreetMap",
  mapRaster=NULL, 
  zoom=NULL,
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
  
  # ----- Generate RGB Raster --------------------------------------------------------
  
  if ( is.null(mapRaster) ) {
    mapRaster <- PWFSLSmoke::esriMap_getMap(
      centerLon, 
      centerLat, 
      width = 640, 
      height = 640,
      zoom = zoom, 
      maptype = maptype, 
      crs = sp::CRS("+init=epsg:4326") )
  }

  img <- PWFSLSmoke::esriMap_plotOnStaticMap(mapRaster)
  graphics::points(pas$longitude, pas$latitude )
  # NOTE: THIS WILL BE IMPLEMENTED IN GGPLOT
}