#' @export
#' @importFrom rlang .data
#' 
#' @title General purpose data filtering for AirSensor objects
#' 
#' @param sensor An AirSensor object.
#' @param ... Logical predicates defined in terms of the variables in the 
#' \emph{sensor}.
#' 
#' @description A generalized data filter for \emph{sensor} objects to 
#' choose rows/cases where conditions are true.  Multiple conditions are 
#' combined with \code{&} or seperated by a comma. Only rows where the condition 
#' evaluates to TRUE are kept.Rows where the condition evaluates to \code{NA}
#' are dropped.
#' 
#' @return A subset of the incoming \emph{sensor}.
#' 
#' @seealso \link{sensor_filterDate}
#' 
#' @examples
#' \dontrun{
#' monitorID <- example_sensor$meta$monitorId
#' twenties <- sensor_filter(example_sensor, SCAN_14 >= 20, SCAN_14 < 30)
#' head(twenties$data)
#' }
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
  
  # ----- Filter data ----------------------------------------------------------
  
  sensor$data <-
    dplyr::filter(sensor$data,...)
  
  return(sensor)
  
}
