#' @export
#' @importFrom MazamaCoreUtils logger.debug
#' 
#' @title Get PWFSLSmoke monitoring data
#' 
#' @description Loads recent PM2.5 monitoring data from the US Forest Service
#' Pacific Wildland Fire Sciences Lab. This function performs the same data 
#' loadingstep as \code{pwfsl_load()}, but has a longer name for consistency 
#' with other data loading functions in the \code{PWFSLSmoke} package. By 
#' default, this function loads data from all 50 states for the past 10 days. 
#'
#' This function is a wrapper around \code{PWFSLSmoke::monitor_loadLatest}.
#' 
#' Data for the most recent 45 days can be downloaded using 
#' \code{PWFSLSmoke::monitor_loadDaily()}. See the \code{PWFSLSmoke package} for
#' additional data loading functions.
#' 
#' @inheritParams PWFSLSmoke::monitor_loadLatest
#' 
#' @return List with \code{meta} and \code{data} elements, a \emph{ws_monitor}
#' object. 
#' 
#' @examples
#' \dontrun{
#' pwfsl <- pwfsl_loadLatest()
#' }

pwfsl_loadLatest <- function() {

  logger.debug("----- pwfsl_loadLatest() -----")
  
  # Download PWFSL data
  pwfsl <- PWFSLSmoke::monitor_loadLatest()
  
  return(pwfsl)
  
}
