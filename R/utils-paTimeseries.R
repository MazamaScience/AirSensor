#' @keywords pat
#' @export
#' 
#' @title Test for correct structure in a \emph{pat} object
#' 
#' @param pat \emph{pat} object
#' 
#' @return \code{TRUE} if \code{pat} has the correct structure, \code{FALSE} otherwise.
#' 
#' @description The \code{pat} is checked for the 'pat' class name
#' and presence of core \code{meta} and \code{data} columns.
#' 
#' Core \code{meta} columns include:
#' 
#' \itemize{
#'   \item{\code{ID} -- Purple Air ID}
#'   \item{\code{label} -- location label}
#'   \item{\code{sensorType} -- PurpleAir sensor type}
#'   \item{\code{longitude} -- decimal degrees E}
#'   \item{\code{latitude} -- decimal degrees N}
#'   \item{\code{timezone} -- Olson timezone}
#'   \item{\code{countryCode} -- ISO 3166-1 alpha-2}
#'   \item{\code{stateCode} -- ISO 3166-2 alpha-2}
#'   \item{\code{pwfsl_closestDistance} -- distance in meters from an official monitor}
#'   \item{\code{pwfsl_closestMonitorID} -- identifer for the nearest official monitor}
#' }
#' 
#' The "pwfsl", official, monitors are obtained from the USFS AirFire site 
#' using the \pkg{PWFSLSmoke} R package.
#' 
#' Core \code{data} columns include:
#' 
#' \itemize{
#' \item{\code{datetime} -- measurement time (UTC)}
#' \item{\code{pm25_A} -- A channel PM 2.5 concentration (ug/m3)}
#' \item{\code{pm25_B} -- B channel PM 2.5 concentration (ug/m3)}
#' \item{\code{temperature} -- temperature (F)}
#' \item{\code{humidity} -- relative humidity (\%)}
#' }
#' 
#' The "pwfsl", official, monitors are obtained from the USFS AirFire site 
#' using the \pkg{PWFSLSmoke} R package.
#' 
#' @examples
#' pat_isPat(example_pat)
#'
pat_isPat <- function(
  pat = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  # Test a variety of things that could go wrong
  if ( is.null(pat) ) return(FALSE)
  if ( !"pa_timeseries" %in% class(pat) ) return(FALSE)
  
  if ( !"meta" %in% names(pat) ) return(FALSE)
  if ( !"data" %in% names(pat) ) return(FALSE)
  
  metaParameters <- c(
    'ID', 'label', 'sensorType',
    'longitude', 'latitude', 
    'timezone', 'countryCode', 'stateCode',
    'pwfsl_closestDistance',
    'pwfsl_closestMonitorID'
  )
  
  if ( !all(metaParameters %in% names(pat$meta)) ) return(FALSE)
  
  # NOTE:  This set of columns must match those defined in
  # NOTE:    pat_createPATimeseriesObject.R
  patData_columnNames <- c(
    "datetime", 
    "pm25_A", "pm25_B", 
    "temperature", "humidity", "pressure",
    "pm1_atm_A", "pm25_atm_A", "pm10_atm_A",
    "pm1_atm_B", "pm25_atm_B", "pm10_atm_B",
    "uptime", "rssi", "memory", "adc0", "bsec_iaq",
    "datetime_A", "datetime_B"
  )
  
  if ( !all(patData_columnNames %in% names(pat$data)) ) return(FALSE)
  
  # Nothing failed so return TRUE
  return(TRUE)
  
}


#' @export
#' 
#' @title Test for an empty \emph{pat} object
#' 
#' @param pat \emph{pat} object
#' @return \code{TRUE} if no data exist in \code{pat}, \code{FALSE} otherwise.
#' @description Convenience function for \code{nrow(pat$data) == 0}.
#' This makes for more readable code in functions that need to test for this.
#' @examples
#' pat_isEmpty(example_pat)
#'
pat_isEmpty <- function(pat) {
  if (!pat_isPat(pat)) stop("Not a valid 'pat' object.")
  return( nrow(pat$data) == 0 )
}


#' @export
#' 
#' @title Retain only distinct data records in pat$data
#' 
#' @param pat \emph{pat} object
#' 
#' @return A \emph{pat} object with no duplicated data records.
#' 
#' @description Convenience wrapper for 
#' \code{pat$data <- dplyr::distinct(pat$data)}.
#' 
pat_distinct <- function(pat) {
  if (!pat_isPat(pat)) stop("Not a valid 'pat' object.")
  pat$data <- dplyr::distinct(pat$data)
  return( pat )
}


#' @title Extract dataframes from \emph{pat} objects
#'
#' @description
#' These functions are convenient wrappers for extracting the dataframes that
#' comprise a \emph{pat} object. These functions are designed to be useful when
#' manipulating data in a pipeline chain using \code{\%>\%}.
#'
#' Below is a table showing equivalent operations for each function.
#'
#' \tabular{ll}{
#'   \strong{Function} \tab \strong{Equivalent Operation}\cr
#'   \code{pat_extractData(pat)} \tab \code{pat[["data"]]}\cr
#'   \code{pat_extractMeta(pat)} \tab \code{pat[["meta"]]}
#' }
#'
#' @param pat \emph{pat} object to extract dataframe from.
#'
#' @return A dataframe from the given \emph{pat} object
#'
#' @name pat_extractDataFrame
#' @aliases pat_extractData pat_extractMeta
#'
NULL


#' @export
#' @rdname pat_extractDataFrame
#'
pat_extractData <- function(pat) {
  if (!pat_isPat(pat)) stop("Not a valid 'pat' object.")
  return(pat[["data"]])
}


#' @export
#' @rdname pat_extractDataFrame
#'
pat_extractMeta <- function(pat) {
  if (!pat_isPat(pat)) stop("Not a valid 'pat' object.")
  return(pat[["meta"]])
}
