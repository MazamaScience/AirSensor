#' @keywords pas
#' @export
#' 
#' @title Test for correct structure in a \emph{pa_synoptic} object
#' 
#' @param pas A \emph{pa_synoptic} object.
#' 
#' @return \code{TRUE} if \code{pas} has the correct structure, \code{FALSE} otherwise.
#' 
#' @description The \code{pas} is checked for the "pas" class name
#' and presence of core metadata columns:
#' \itemize{
#'   \item{ID -- Purple Air ID}
#'   \item{label -- location label}
#'   \item{sensorType -- PurpleAir sensor type}
#'   \item{longitude -- decimal degrees E}
#'   \item{latitude -- decimal degrees N}
#'   \item{timezone -- Olson timezone}
#'   \item{countryCode -- ISO 3166-1 alpha-2}
#'   \item{stateCode -- ISO 3166-2 alpha-2}
#'   \item{pm25_1hr -- hourly PM2.5}
#'   \item{pm25_1day -- daily PM2.5}
#'   \item{temperature -- deg F}
#'   \item{humidity -- \%}
#'   \item{pressure -- mb}
#'   \item{deviceID -- unique device identifier}
#'   \item{locationID -- unique location identifier}
#'   \item{deviceDeploymentID -- unique time series identifier}
#' }
#' 
#' @seealso \link{pas_enhanceRawData}
#' 
#' @examples
#' pas_isPas(example_pas)
#' pas_isPas(1:10)

pas_isPas <- function(
    pas = NULL
) {
  
  # Test a variety of things that could go wrong
  if ( is.null(pas) ) return(FALSE)
  if ( !"pa_synoptic" %in% class(pas) ) return(FALSE)
  
  if ( "pm2.5_60minute" %in% names(pas) ) {
    # New pas created with version >= 1.1
    parameters <- c(
      "ID", # for backwards compatibility, same as deviceID
      "label", # for backwards compatibility, same as locationName
      "locationName",
      "sensorType", # for backwards compatibility, same as model
      "longitude", "latitude", 
      "timezone", "countryCode", "stateCode",
      "pm2.5_60minute", "pm2.5_24hour", "temperature", "humidity", "pressure",
      "deviceID", "locationID", "deviceDeploymentID"
    )
  } else {
    # Old pas created with version < 1.1
    parameters <- c(
      "ID", "label", "sensorType",
      "longitude", "latitude", 
      "timezone", "countryCode", "stateCode",
      "pm25_1hr", "pm25_1day", "temperature", "humidity", "pressure",
      "deviceID", "locationID", "deviceDeploymentID"
    )
  }
  
  if ( !all(parameters %in% names(pas)) ) {
    
    message('Invalid pa_synoptic format. See `pas_upgrade()` to upgrade the format.')
    
    return(FALSE)
    
  } else {
    
    # Nothing failed so return TRUE
    return(TRUE)
    
  }
  
}


#' @export
#' 
#' @title Test for an empty \emph{pa_synoptic} object
#' 
#' @param pas A \emph{pa_synoptic} object.
#' 
#' @return \code{TRUE} if no data exist in \code{pas}, \code{FALSE} otherwise.
#' 
#' @description Convenience function for \code{nrow(pas) == 0}.
#' This makes for more readable code in functions that need to test for this.
#' 
#' @examples
#' pas <- example_pas
#' pas_isEmpty(pas)
#' pas <- pas %>% pas_filter(ID < 0)
#' pas_isEmpty(pas)

pas_isEmpty <- function(pas) {
  if (!pas_isPas(pas)) stop("Not a valid 'pas' object.")
  return( nrow(pas) == 0 )
}

#' @export
#' 
#' @title Test for spatial metadata in \emph{pa_synoptic} object
#' 
#' @param pas A \emph{pa_synoptic} object.
#' 
#' @return \code{TRUE} if \code{pas} contains core spatial metadata, 
#' \code{FALSE} otherwise.
#' 
#' @description Tests for the existence of the following core spatial metadata 
#' columns:
#' 
#' \itemize{
#'   \item{longitude -- decimal degrees E}
#'   \item{latitude -- decimal degrees N}
#'   \item{timezone -- Olson timezone}
#'   \item{countryCode -- ISO 3166-1 alpha-2}
#'   \item{stateCode -- ISO 3166-2 alpha-2}
#' }
#' 
#' @examples
#' pas <- example_pas
#' pas_hasSpatial(pas)

pas_hasSpatial <- function(pas) {
  
  if ( is.null(pas) ) return(FALSE)
  
  # Test the following
  parameters <- c(
    "longitude", "latitude", "timezone", "countryCode", "stateCode"
  )
  
  if ( all(parameters %in% names(pas)) ) {
    
    return(TRUE)
    
  } else {
    
    return(FALSE)
    
  }
  
}
