#' @export
#' 
#' @title Find PurpleAir sensors within radial distance
#' 
#' @param pas PurpleAir \emph{pas} object.
#' @param longitude a Target longitude.
#' @param latitude a Target latitude. 
#' @param radius Distance from target with unit (i.e "15 km").
#' 
#' @description Filter for PurpleAir sensors within a specified distance from 
#' specified target coordinates. 
#' 
#' @details \code{radius} Should be a numeric string with a metric unit 
#' separated by a space, such as \code{"250 m"}.
#'
#' @return A subset of the given \emph{pas} object.
#' 
#' @seealso \link{pas_filter}
#' @seealso \link{pas_filterArea}
#'
#' @examples 
#' library(AirSensor)
#' 
#' # Near Diamond Bar, CA
#' pas <- example_pas
#' diamond_bar <-
#'   pas %>%
#'   pas_filterNear(
#'     longitude = -117.820833,
#'     latitude = 34.001667, 
#'     radius = "20 km"
#'   )
#'                  
#' if ( interactive() ) {
#'   pas_leaflet(diamond_bar)
#' }
#'

pas_filterNear <- function(
  pas = NULL,
  longitude = NULL, 
  latitude = NULL, 
  radius = "1 km"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pas)
  MazamaCoreUtils::stopIfNull(longitude)
  MazamaCoreUtils::stopIfNull(latitude)
  MazamaCoreUtils::stopIfNull(radius)
  
  if ( !pas_isPas(pas) )
    stop("Required parameter 'pas' is not a valid 'pa_synoptic' object.")
  
  if ( pas_isEmpty(pas) )
    stop("Required parameter 'pas' has no data.") 
  
  if ( !stringr::str_ends(radius, "[ km]") )
    stop("Radius requires a unit and format (i.e '1 m' or '1 km')")
  
  parts <- 
    stringr::str_split(
      string = radius, 
      pattern = " ", 
      simplify = TRUE
    ) 
  
  if ( tolower(parts[,2]) == "km" ) {
    radius_m <- as.numeric(parts[,1])*1000
  } else if ( tolower(parts[,2]) == "m" ) {
    radius_m <- as.numeric(parts[,1])
  } else {
    stop(sprintf("Unit \"%s\" is not understood. Use 'm' or 'km'.", parts[,2]))
  }
  
  # ----- Calculate distances --------------------------------------------------
  
  distance <- 
    geodist::geodist(
      x = cbind(
        "x" = longitude, 
        "y" = latitude
      ),
      y = cbind(
        "x" = pas$longitude, 
        "y" = pas$latitude
      )
    )
  
  
  # ----- Return ---------------------------------------------------------------
  
  return(pas[which(distance <= radius_m),])
  
}