#' @export
#' @importFrom PWFSLSmoke monitor_loadLatest
#' 
#' @title Get PWFSLSmoke Monitoring Data
#' 
#' @description Loads recent PM2.5 monitoring data from the US Forest Service
#' Pacific Wildland Fire Sciences Lab. This function performs the same data loading 
#' step as \code{pwfsl_loadLatest()}, but has a shorter name for consistency with other
#' data loading functions in the \code{MazamaPurpleAir} package. By default,
#' this function loads data from all 50 states for the past 10 days. 
#'
#' This function is a wrapper around \code{PWFSLSmoke::monitor_loadLatest}.
#' 
#' Data for the most recent 45 days can be downloaded using 
#' \code{PWFSLSmoke::monitor_loadDaily()}. See the \code{PWFSLSmoke package} for
#' additional data loading functions.
#' 
#' @inheritParams PWFSLSmoke::monitor_loadLatest
#' 
#' @return List with \code{meta} and \code{data} elements.
#' 
#' @examples
#' \dontrun{
#' pwfsl <- pwfsl_load()
#' }

pwfsl_load <- function() {
  
  logger.debug("----- pwfsl_load() -----")
  
  # Download PWFSL data
  pwfsl <- PWFSLSmoke::monitor_loadLatest()
  
  return(pwfsl)
  
}
