#' @export
#' @importFrom PWFSLSmoke monitor_loadLatest
#' 
#' @title Get PWFSLSmoke Monitoring Data
#' 
#' @description Loads recent PM2.5 monitoring data from the US Forest Service
#' Pacific Wildland Fire Sciences Lab. By default, this loads data from all 50 states
#' for the past 10 days. 
#'
#' This function is a wrapper around \code{PWFSLSmoke::monitor_loadLatest}.
#' 
#' Data for the most recent 45 days can be downloaded using \code{monitor_load}
#' in the PWFSLSmoke package, whose value will be a /code{ws_monitor} object with 
#' the same structuer as the value returned by \code{pwfsl_loadLatest}. 
#' 
#' @inheritParams PWFSLSmoke::monitor_loadLatest
#' 
#' @return List with \code{meta} and \code{data} elements
#' 
#' @examples
#' \dontrun{
#' pwfsl <- pwfsl_load()
#' }

pwfsl_loadLatest <- function() {
  
  logger.debug("----- pwfsl_load() -----")
  
  # Download PWFSL data
  pwfsl <- PWFSLSmoke::monitor_loadLatest()
  
  return(pwfsl)
  
}
