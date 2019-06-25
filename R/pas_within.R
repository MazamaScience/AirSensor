#' @export
#' 
#' @title Find Purple Air sensors within radial distance
#' 
#' @param pas A \code{pa_synoptic} object.
#' @param label A target (centered) Purple Air sensor label.
#' @param radius A distance with unit (i.e "15 km").
#' 
#' @description Filter for Purple Air sensors within a specified distance from 
#' a target Purple Air sensor.  
#' 
#' @details \code{radius} Should be a numeric string with provided metric unit separated by a space,
#' such as \code{"250 m"}.
#'
#' @return A subset of the given \code{pa_synoptic} object.
#' 
#' @seealso \link{pas_filter}, \link{pas_filterArea}
#'
#' @examples 
#' \dontrun{
#' pas_within(example_pas, "SaratogaMiddleHighSchool", "1024 km")
#' }

pas_within <- function(
  pas,
  label = NULL, 
  radius = "1 km"
  ) {
  
  # Validate parameters --------------------------------------------------------
  
  if ( !pas_isPas(pas) )
    stop("Required parameter 'pas' is not a valid 'pa_synoptic' object.")
  
  if ( pas_isEmpty(pas) )
    stop("Required parameter 'pas' has no data.") 
  
  if ( is.null(label) ) 
    stop("Required parameter 'label' is missing")
  
  r_split <- 
    stringr::str_split(
      string = radius, 
      pattern = " ", 
      simplify = TRUE
    ) 
  
  if ( tolower(r_split[,2]) == "km" ) radius_m <- as.numeric(r_split[,1])*1000
  if ( tolower(r_split[,2]) == "m" ) radius_m <- as.numeric(r_split[,1])
  
  target <- 
    pas[which(stringr::str_detect(label, pas$label)),]
  
  distance <- 
    geosphere::distHaversine(
      p1 =cbind(
        target$longitude, 
        target$latitude
      ),
      p2 = cbind(
        pas$longitude, 
        pas$latitude
      )
    )
  
  return(pas[which(distance <= radius_m),])
  
  }