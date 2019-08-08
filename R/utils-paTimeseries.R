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
#'   \item{pwfsl_closestDistance -- distance in meters from an official monitor}
#'   \item{pwfsl_closestMonitorID -- identifer for the nearest official monitor}
#' }
#' 
#' The "pwfsl", official, monitors are obtained from the USFS AirFire site 
#' using the \pkg{PWFSLSmoke} R package.
#' 
#' @examples
#' pat_isPat(example_pat)
#'
pat_isPat <- function(pat = NULL) {
  
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
  
  dataParameters <- c(
    "datetime", "pm25_A", "pm25_B",
    "temperature", "humidity",
    "uptime", "adc0", "rssi",
    "datetime_A", "datetime_B" 
  )
  
  if ( !all(dataParameters %in% names(pat$data)) ) return(FALSE)
  
  # Nothing failed so return TRUE
  return(TRUE)
  
}


#' @export
#' 
#' @title Test for an empty \emph{pat} object
#' 
#' @param pat \emph{pat} object
#' @return \code{TRUE} if no data exist in \code{pat}, \code{FALSE} otherwise.
#' @description Convenience function for \code{nrow(pat$meta) == 0}.
#' This makes for more readable code in functions that need to test for this.
#' @examples
#' pat_isEmpty(example_pat)
#'
pat_isEmpty <- function(pat) {
  if (!pat_isPat(pat)) stop("Not a valid 'pat' object.")
  return( nrow(pat$meta) == 0 )
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
