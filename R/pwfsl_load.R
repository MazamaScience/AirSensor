#' @export
#' @importFrom MazamaCoreUtils logger.debug
#' 
#' @title Get PWFSLSmoke monitoring data
#' 
#' @description Loads recent PM2.5 monitoring data from the US Forest Service
#' Pacific Wildland Fire Sciences Lab. This function performs the same data 
#' loading step as \code{pwfsl_loadLatest()}, but has a shorter name for 
#' consistency with other data loading functions in the \code{AirSensor} 
#' package. By default, this function loads data from all 50 states for the past 
#' 10 days. 
#'
#' By default, this function is a wrapper around 
#' \code{PWFSLSmoke::monitor_loadLatest}. But it can also be used as a wrapper
#' around \code{PWFSLSmoke::monitor_load} by passing in arguments.
#' 
#' If you pass in arguments, \emph{e.g.} \code{starttime} and \code{endtime},
#' \code{PWFSLSmoke::monitor_load()} will be invoked. Otherwise,
#' \code{PWFSLSmoke::monitor_loadLatest()} will be invoked.
#' 
#' @param ... Arguments passed on to \code{PWFSLSmoke::monitor_load()}.
#' 
#' @return List with \code{meta} and \code{data} elements, a \emph{ws_monitor} 
#' object.
#' 
#' @examples
#' \donttest{
#' library(AirSensor)
#' 
#' pwfsl <- pwfsl_load()
#' dim(pwfsl$meta)
#' dim(pwfsl$data)
#' }

pwfsl_load <- function(...) {
  
  # ----- Validate parameters --------------------------------------------------
  
  argsList <- list(...)
  
  # ----- Load data ------------------------------------------------------------

  if ( length(argsList) == 0 ) {
    pwfsl <- PWFSLSmoke::monitor_loadLatest()
  }  else {
    pwfsl <- PWFSLSmoke::monitor_load(...)
  }
  
  return(pwfsl)
  
}
