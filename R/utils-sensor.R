#' @keywords sensor
#' @export
#' 
#' @title Test for correct structure in a \emph{sensor} object
#' 
#' @param sensor \emph{sensor} object
#' 
#' @return \code{TRUE} if \code{sensor} has the correct structure, \code{FALSE} otherwise.
#' 
#' @description The \code{sensor} is checked for the 'sensor' class name
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
#'   \item{pwfsl_closestDistance -- distance in meters from an official monitor}
#'   \item{pwfsl_closestMonitorID -- identifer for the nearest official monitor}
#' }
#' 
#' The "pwfsl", official, monitors are obtained from the USFS AirFire site 
#' using the \pkg{PWFSLSmoke} R package.
#' 
#' @examples
#' example_sensor <- pat_createAirSensor(example_pat)
#' sensor_isSensor(example_sensor)
#'
sensor_isSensor <- function(sensor = NULL) {
  
  # Test a variety of things that could go wrong
  if ( is.null(sensor) ) return(FALSE)
  
  # NOTE:  "airsensor" objects also have class "ws_monitor" and can be 
  # NOTE:  manipulated with PWFSLSmoke::monitor_~() functions. These may
  # NOTE:  return objects that no longer have the "airsensor" class but they
  # NOTE:  should always have the "ws_monitor" class.
  
  if ( !"ws_monitor" %in% class(sensor) ) return(FALSE)
  
  if ( !"meta" %in% names(sensor) ) return(FALSE)
  if ( !"data" %in% names(sensor) ) return(FALSE)
  
  # Most important metadata columns
  metaParameters <- c(
    'ID', 'sensorType',
    'longitude', 'latitude', 
    'timezone', 'countryCode', 'stateCode',
    'pwfsl_closestDistance',
    'pwfsl_closestMonitorID'
  )
  
  if ( !all(metaParameters %in% names(sensor$meta)) ) return(FALSE)
  
  if ( any(duplicated(sensor$data$datetime)) )
    warning("Duplicate timesteps found in 'sensor' object.")
  
  # Nothing failed so return TRUE
  return(TRUE)
  
}


#' @export
#' 
#' @title Test for an empty \emph{sensor} object
#' 
#' @param sensor \emph{sensor} object
#' @return \code{TRUE} if no data exist in \code{sensor}, \code{FALSE} otherwise.
#' @description Convenience function for \code{nrow(sensor$meta) == 0}.
#' This makes for more readable code in functions that need to test for this.
#' @examples
#' example_sensor <- pat_createAirSensor(example_pat)
#' sensor_isEmpty(example_sensor)
#'
sensor_isEmpty <- function(sensor) {
  if (!sensor_isSensor(sensor)) stop("Not a valid 'sensor' object.")
  return( nrow(sensor$meta) == 0 )
}



#' @title Extract dataframes from \emph{airsensor} objects
#'
#' @description
#' These functions are convenient wrappers for extracting the dataframes that
#' comprise a \emph{airsensor} object. These functions are designed to be useful
#' when manipulating data in a pipeline chain using \code{\%>\%}.
#'
#' Below is a table showing equivalent operations for each function.
#'
#' \tabular{ll}{
#'   \strong{Function} \tab \strong{Equivalent Operation}\cr
#'   \code{sensor_extractData(sensor)} \tab \code{sensor[["data"]]}\cr
#'   \code{sensor_extractMeta(sensor)} \tab \code{sensor[["meta"]]}
#' }
#'
#' @param sensor \emph{sensor} object to extract dataframe from.
#'
#' @return A dataframe from the given \emph{sensor} object
#'
#' @name sensor_extractDataFrame
#' @aliases sensor_extractData sensor_extractMeta
#'
NULL


#' @export
#' @rdname sensor_extractDataFrame
#'
sensor_extractData <- function(sensor) {
  if (!sensor_isSensor(sensor)) stop("Not a valid 'sensor' object.")
  return(sensor[["data"]])
}


#' @export
#' @rdname sensor_extractDataFrame
#'
sensor_extractMeta <- function(sensor) {
  if (!sensor_isSensor(sensor)) stop("Not a valid 'sensor' object.")
  return(sensor[["meta"]])
}
