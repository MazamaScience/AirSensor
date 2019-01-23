#' @export
#' @importFrom PWFSLSmoke monitor_loadLatest
#' 
#' @title Get PWFSLSmoke Monitoring Data
#' 
#' @description Loads recent PM2.5 monitoring data from the US Forest Service
#' Pacific Wildland Fire Sciences Lab. This function performs the same data loading 
#' step as \code{pwfsl_load}, but has a different name for naming consistency with the 
#' \code{PWFSLSmoke} package. By default, this loads data from all 50 states
#' for the past 10 days. 
#'
#' This function is a wrapper around \code{PWFSLSmoke::monitor_loadLatest}.
#' 
#' Data for the most recent 45 days can be downloaded using \code{monitor_load}
#' in the PWFSLSmoke package, whose value will be a \emph{ws_monitor} object with 
#' the same structure as the value returned by \code{pwfsl_loadLatest}. 
#' 
#' @inheritParams PWFSLSmoke::monitor_loadLatest
#' 
#' @return List with \code{meta} and \code{data} elements
#' 
#' @examples
#' \dontrun{
#' pwfsl <- pwfsl_loadLatest()
#' }

pwfsl_loadLatest <- function() {
  # TODO: change the logger.debug quoted statement to pwfsl_loadLatest as well?
  logger.debug("----- pwfsl_load() -----")
  
  # Download PWFSL data
  pwfsl <- PWFSLSmoke::monitor_loadLatest()
  
  return(pwfsl)
  
}
