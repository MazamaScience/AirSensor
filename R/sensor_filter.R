#' @export
#' @importFrom rlang .data
#' 
#' @title Data filtering for AirSensor objects
#' 
#' @param sensor An AirSensor object.
#' @param ... Logical predicates defined in terms of the variables in 
#' \code{sensor$data}.
#' 
#' @description A generalized data filter for \emph{sensor} objects to 
#' choose rows/cases where conditions are true.  Multiple conditions are 
#' combined with \code{&} or seperated by a comma. Only rows where the condition 
#' evaluates to TRUE are kept.Rows where the condition evaluates to \code{NA}
#' are dropped.
#' 
#' @note Filtering predicates are applied to the \code{data} dataframe within
#' the \emph{sensor} object.
#' 
#' @return A subset of the incoming \emph{sensor}.
#' 
#' @seealso \link{sensor_filterDate}
#' @seealso \link{sensor_filterMeta}
#' 
#' @examples
#' twenties <- sensor_filter(example_sensor, SCAN_14 >= 20, SCAN_14 < 30)
#' head(twenties$data)
#' 

sensor_filter <- function(
  sensor = NULL, 
  ...
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !PWFSLSmoke::monitor_isMonitor(sensor) )
    stop("Parameter 'sensor' is not a valid 'airsensor' object.") 
  
  if ( PWFSLSmoke::monitor_isEmpty(sensor) ) 
    stop("Parameter 'sensor' has no data.")
  
  # ----- Filter meta ----------------------------------------------------------
  
  sensor$data <-
    dplyr::filter(sensor$data, ...)
  
  return(sensor)
  
}
