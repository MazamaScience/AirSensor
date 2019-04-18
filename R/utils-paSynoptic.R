#' @keywords pas
#' @export
#' 
#' @title Test for correct structure in a \emph{pas} object
#' 
#' @param pas \emph{pas} object
#' 
#' @return \code{TRUE} if \code{pas} has the correct structure, \code{FALSE} otherwise.
#' 
#' @description The \code{pas} is checked for the "pas" class name
#' and presence of core metadata columns:
#' \itemize{
#'   \item{ID -- Purple Air ID}
#'   \item{label -- location label}
#'   \item{sensorType -- Purple Air sensor type}
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
#'   \item{pwfsl_closestDistance -- distance in meters from an official monitor}
#'   \item{pwfsl_closestMonitorID -- identifer for the nearest official monitor}
#' }
#' 
#' The "pwfsl", official, monitors are obtained from the USFS AirFire site 
#' using the \pkg{PWFSLSmoke} R package.
#' 
pas_isPas <- function(pas) {
  
  # Test a variety of things that could go wrong
  if ( !"pa_synoptic" %in% class(pas) ) return(FALSE)
  
  parameters <- c(
    'ID', 'label', 'sensorType',
    'longitude', 'latitude', 
    'timezone', 'countryCode', 'stateCode',
    'pm25_1hr', 'pm25_1day', 'temperature', 'humidity', 'pressure',
    'pwfsl_closestDistance',
    'pwfsl_closestMonitorID'
  )
  
  if ( !all(parameters %in% names(pas)) ) return(FALSE)
  
  # Nothing failed so return TRUE
  return(TRUE)
  
}


#' @export
#' 
#' @title Test for an empty \emph{pas} object
#' 
#' @param pas \emph{pas} object
#' @return \code{TRUE} if no data exist in \code{pas}, \code{FALSE} otherwise.
#' @description Convenience function for \code{nrow(pas) == 0}.
#' This makes for more readable code in functions that need to test for this.
#'
pas_isEmpty <- function(pas) {
  if (!pas_isPas(pas)) stop("Not a valid 'pas' object.")
  return( nrow(pas) == 0 )
}