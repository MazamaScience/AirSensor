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
#'   \item{pwfsl_closestDistance -- distance in meters from an official monitor}
#'   \item{pwfsl_closestMonitorID -- identifer for the nearest official monitor}
#' }
#' 
#' The "pwfsl", official, monitors are obtained from the USFS AirFire site 
#' using the \pkg{PWFSLSmoke} R package.
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
  
  parameters <- c(
    "ID", "label", "sensorType",
    "longitude", "latitude", 
    "timezone", "countryCode", "stateCode",
    "pm25_1hr", "pm25_1day", "temperature", "humidity", "pressure",
    "deviceID", "locationID", "deviceDeploymentID",
    "pwfsl_closestDistance",
    "pwfsl_closestMonitorID"
  )
  
  if ( !all(parameters %in% names(pas)) ) return(FALSE)
  
  # Nothing failed so return TRUE
  return(TRUE)
  
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
