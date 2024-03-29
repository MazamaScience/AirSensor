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
#' library(AirSensor)
#' 
#' teens <- sensor_filter(example_sensor, 
#'                           example_sensor$data$`6f71a0c5f076deda_9392` <  20, 
#'                           example_sensor$data$`6f71a0c5f076deda_9392` >= 10)
#' head(teens$data)
#' 

sensor_filter <- function(
  sensor = NULL, 
  ...
) {
  
  # ----- Validate parameters --------------------------------------------------

  # A little involved to catch the case where the user forgets to pass in 'sensor'
  
  result <- try({
    if ( !sensor_isSensor(sensor) )
      stop("First argument is not of class 'airsensor' or 'ws_monitor'.")
  }, silent = TRUE)
  
  if ( class(result) %in% "try-error" ) {
    err_msg <- geterrmessage()
    if ( stringr::str_detect(err_msg, "object .* not found") ) {
      stop(paste0(err_msg, "\n(Did you forget to pass in the 'sensor' object?)"))
    }
  }
  
  if ( sensor_isEmpty(sensor) ) 
    stop("Parameter 'sensor' has no data.")
  
  # ----- Filter meta ----------------------------------------------------------
  
  sensor$data <-
    dplyr::filter(sensor$data, ...)
  
  return(sensor)
  
}
