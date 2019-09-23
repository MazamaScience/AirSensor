#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized logger.error
#' 
#' @title Load hourly-aggregated Purple Air data for a month
#' 
#' @description A pre-generated \code{airsensor} object will be loaded for
#' the given month. Archived data for SCAQMD sensors go back to January, 2018.
#' 
#' The \code{datestamp} can must be in the following format:
#' 
#' \itemize{
#' \item{\code{"YYYYmm"}}
#' }
#' 
#' By default, the current month is loaded.
#'
#' Each \code{airsensor} object contains data from a named collection of 
#' Purple Air sensors.
#' 
#' @param collection Name associated with the collection.
#' @param datestamp A date string in ymd order.
#' @param timezone Timezone used to interpret \code{datestamp}.
#' 
#' @return An object of class "pa_timeseries".
#' 
#' @seealso \link{pat_createNew}
#' 
#' @examples
#' \donttest{
#' setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
#' sensor_loadMonth("scaqmd", 201905) %>%
#'   PWFSLSmoke::monitor_timeseriesPlot(style = 'gnats')
#' }

sensor_loadMonth <- function(
  collection = "scaqmd",
  datestamp = NULL,
  timezone = "America/Los_Angeles"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(collection)
  
  # TODO: Work with lubridate to support all formats
  
  # Default to the current month
  if ( is.null(datestamp) || datestamp == "" ) {
    now <- lubridate::now(tzone = timezone)
    datestamp <- strftime(now, "%Y%m%d", tz = timezone)
  }
  
  # Handle the case where the day is already specified
  datestamp <- stringr::str_sub(paste0(datestamp,"01"), 1, 8)
  monthstamp <- stringr::str_sub(datestamp, 1, 6)
  yearstamp <- stringr::str_sub(datestamp, 1, 4)
  
  # ----- Load data from URL or directory --------------------------------------
  
  # Use package internal URL
  baseDir <- getArchiveBaseDir()
  baseUrl <- getArchiveBaseUrl()
  
  filename <- paste0("airsensor_", collection, "_", monthstamp, ".rda")
  dataUrl <- paste0(baseUrl, '/airsensor/', yearstamp)
  
  # dataDir should be NULL if baseDir is NULL
  if ( is.null(baseDir) ) {
    dataDir <- NULL
  } else {
    dataDir <- paste0(baseDir, '/airsensor/', yearstamp)
  }
  
  # Get data from URL or directory
  result <- try({
    suppressWarnings( airsensor <- loadDataFile(filename, dataUrl, dataDir) )
  }, silent = TRUE)
  
  # NOTE:  We used suppressWarnings() above so that we can have a more
  # NOTE:  uniform error response for the large variety of reasons that
  # NOTE:  loading might fail.
  
  if ( "try-error" %in% class(result) ) {
    if ( logger.isInitialized() ) {
      logger.error("%s", geterrmessage())
    }
    if ( is.null(baseDir) ) {
      stop(paste0("Data file could not be loaded from: ", baseUrl), call.=FALSE)
    } else {
      stop(paste0("Data file could not be loaded from: ", baseDir), call.=FALSE)
    }
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(invisible(airsensor))
  
}
