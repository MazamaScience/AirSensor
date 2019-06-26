#' @export
#' 
#' @title Find Purple Air sensors within radial distance
#' 
#' @param pas a \code{pa_synoptic} object.
#' @param latitude a target latitude. 
#' @param longitude a target longitude.
#' @param radius a distance with unit (i.e "15 km").
#' 
#' @description Filter for Purple Air sensors within a specified distance from 
#' specified target coordinates. 
#' 
#' @details \code{radius} Should be a numeric string with provided metric unit 
#' separated by a space, such as \code{"250 m"}.
#'
#' @return A subset of the given \code{pa_synoptic} object.
#' 
#' @seealso \link{pas_filter}, \link{pas_filterArea}
#'
#' @examples 
#' \dontrun{
#' pas_filterNear(
#'     pas = example_pas, 
#'     label = "SaratogaMiddleHighSchool", 
#'     radius = "1024 km"
#'     )
#'}
#'\dontrun{
#' pas_filterNear(
#'     pas = example_pas,
#'     latitude = 47.61702, 
#'     longitude = -122.34376
#'     )
#'}
#'

pas_filterNear <- function(
  pas,
  latitude = NULL, 
  longitude = NULL, 
  radius = "1 km"
  ) {
  
  # Validate parameters --------------------------------------------------------
  
  if ( !pas_isPas(pas) )
    stop("Required parameter 'pas' is not a valid 'pa_synoptic' object.")
  
  if ( pas_isEmpty(pas) )
    stop("Required parameter 'pas' has no data.") 
  
  if ( is.null(latitude) || is.null(longitude) )
    stop("Required target coordinate(s) is missing") 
  
  if ( !stringr::str_ends(radius, "[ km]") )
    stop("Raidus requires a unit and format (i.e '1 m' or '1 km')")
   
  r_split <- 
    stringr::str_split(
      string = radius, 
      pattern = " ", 
      simplify = TRUE
    ) 
  
  if ( tolower(r_split[,2]) == "km" ) radius_m <- as.numeric(r_split[,1])*1000
  if ( tolower(r_split[,2]) == "m" ) radius_m <- as.numeric(r_split[,1])
  
  distance <- 
    geosphere::distHaversine(
      p1 =cbind(
        longitude, 
        latitude
      ),
      p2 = cbind(
        pas$longitude, 
        pas$latitude
      )
    )
  
  return(pas[which(distance <= radius_m),])
  
  }