#' @export
#' @importFrom rlang .data
#' 
#' @title Metadata filtering for AirSensor objects
#' 
#' @param sensor An AirSensor object.
#' @param ... Logical predicates defined in terms of the variables in 
#' \code{sensor$meta}.
#' 
#' @description A generalized data filter for \emph{sensor} objects to 
#' choose rows/cases where conditions are true.  Multiple conditions are 
#' combined with \code{&} or seperated by a comma. Only rows where the condition 
#' evaluates to TRUE are kept.Rows where the condition evaluates to \code{NA}
#' are dropped.
#' 
#' @note Filtering predicates are applied to the \code{meta} dataframe within
#' the \emph{sensor} object.
#' 
#' @return A subset of the incoming \emph{sensor}.
#' 
#' @seealso \link{sensor_filter}
#' @seealso \link{sensor_filterDate}
#' 
#' @examples \donttest{
#' library(AirSensor)
#' 
#' example_sensor %>%
#'   sensor_filterMeta(stateCode == "CA") %>%
#'   PWFSLSmoke::monitor_stamenmap(zoom = 11)
#' }

sensor_filterMeta <- function(
  sensor = NULL, 
  ...
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(sensor)
  
  if ( !sensor_isSensor(sensor) )
    stop("Parameter 'sensor' is not a valid 'airsensor' object.") 
  
  if ( sensor_isEmpty(sensor) ) 
    stop("Parameter 'sensor' has no data.")
  
  # ----- Filter based on metadata ---------------------------------------------
  
  # FILTER meta rows
  sensor$meta <-
    sensor$meta %>%
    dplyr::filter(...)
  
  # SELECT data columns
  sensor$data <-
    sensor$data %>%
    dplyr::select(c('datetime', sensor$meta$monitorID))
  
  return(sensor)
  
}
