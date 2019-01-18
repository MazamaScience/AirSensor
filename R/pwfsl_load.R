#' @export
#' @importFrom PWFSLSmoke monitor_loadLatest
#' 
#' @title Get PWFSLSmoke Monitoring Data
#' 
#' @description Loads recent PM2.5 monitoring data from the US Forest Service
#' Pacific Wildland Fire Sciences Lab.
#'
#' This function is a wrapper around \code{PWFSLSmoke::monitor_loadLatest}.
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
