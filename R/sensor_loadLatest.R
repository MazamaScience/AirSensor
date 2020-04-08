#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized
#' 
#' @title Load hourly-aggregated PurpleAir data for a week
#' 
#' @description A pre-generated \code{airsensor} object will be loaded 
#' containing data for the most recent 7 or 45-day interval.
#' 
#' Each \code{airsensor} object contains data from a named collection of 
#' PurpleAir sensors.
#' 
#' @param collection Name associated with the collection.
#' @param days Number of days of data to include (7 or 45).
#' 
#' @return An object of class "pa_timeseries".
#' 
#' @seealso \link{sensor_load}
#' @seealso \link{sensor_loadMonth}
#' @seealso \link{pat_createAirSensor}
#' 
#' @examples
#' \donttest{
#' # TODO:  This needs to be updated to use USFS data
#' setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
#' sensor_loadLatest("scaqmd") %>%
#'   PWFSLSmoke::monitor_timeseriesPlot(style = 'gnats')
#' }

sensor_loadLatest <- function(
  collection = "scaqmd",
  days = 7
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(collection)
  MazamaCoreUtils::stopIfNull(days)
  
  days <- as.numeric(days)
  if ( !days %in% c(7, 45) )
    stop("Parameter 'days' must be either 7 or 45")
  
  # ----- Load data from URL or directory --------------------------------------
  
  # Use package internal URL
  baseDir <- getArchiveBaseDir()
  baseUrl <- getArchiveBaseUrl()
  
  filename <- paste0("airsensor_", collection, "_latest", days, ".rda")
  dataUrl <- paste0(baseUrl, '/airsensor/latest')
  
  # dataDir should be NULL if baseDir is NULL
  if ( is.null(baseDir) ) {
    dataDir <- NULL
  } else {
    dataDir <- paste0(baseDir, '/airsensor/latest')
  }
  
  # Get data from URL or directory
  result <- try({
    suppressWarnings({
      sensor <- MazamaCoreUtils::loadDataFile(filename, dataUrl, dataDir) 
    })
  }, silent = TRUE)
  
  # NOTE:  We used suppressWarnings() above so that we can have a more
  # NOTE:  uniform error response for the large variety of reasons that
  # NOTE:  loading might fail.
  
  if ( "try-error" %in% class(result) ) {
    if ( is.null(baseDir) ) {
      stop(paste0("Data file could not be loaded from: ", baseUrl), call. = FALSE)
    } else {
      stop(paste0("Data file could not be loaded from: ", baseDir), call. = FALSE)
    }
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(sensor)
  
}
