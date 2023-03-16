#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized
#' 
#' @title Load hourly-aggregated PurpleAir data for a month
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
#' PurpleAir sensors.
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
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor)
#' 
#' setArchiveBaseUrl("https://airsensor.aqmd.gov/PurpleAir/v1")
#' 
#' sensor_loadMonth("scaqmd", 202005) %>%
#'   PWFSLSmoke::monitor_timeseriesPlot(style = 'gnats')
#' 
#' }, silent = FALSE)
#' }

sensor_loadMonth <- function(
  collection = "scaqmd",
  datestamp = NULL,
  timezone = "America/Los_Angeles"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(collection)
  
  # ----- Create year and month stamps -----------------------------------------
  
  # NOTE:  Incoming datestamps are interpreted in the local timezone.
  
  # Default to the current month
  if ( is.null(datestamp) || is.na(datestamp) || datestamp == "" ) {
    datetime <- lubridate::now(tzone = timezone)
  } else {
    datetime <- MazamaCoreUtils::parseDatetime(datestamp, timezone = timezone)
  }
  
  # Filename timestamps are always in UTC
  datestamp <- strftime(datetime, "%Y%m%d", tz = "UTC")
  monthstamp <- strftime(datetime, "%Y%m", tz = "UTC")
  yearstamp <- strftime(datetime, "%Y", tz = "UTC")

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
    suppressWarnings({ 
      airsensor <- MazamaCoreUtils::loadDataFile(filename, dataUrl, dataDir) 
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
  
  # Guarantee that 'ID' and 'deviceID' fields are <character> as opposed to <int>
  airsensor$meta$ID <- as.character(airsensor$meta$ID)
  airsensor$meta$deviceID <- as.character(airsensor$meta$deviceID)
  airsensor$meta$instrumentID <- as.character(airsensor$meta$instrumentID)
  
  return(invisible(airsensor))
  
}
