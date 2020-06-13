#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized
#' 
#' @title Load PurpleAir time series data for a month
#' 
#' @description A pre-generated PurpleAir Timeseries \emph{pat} object will be 
#' loaded for the month requested with \code{datestamp} if available. Data are 
#' loaded from the archive set with either \code{setArchiveBaseUrl()} or 
#' \code{setArchiveBaseDir()} for locally archived files.
#' 
#' The \code{datestamp} must be in the following format:
#' 
#' \itemize{
#' \item{\code{"YYYYmm"}}
#' }
#' 
#' By default, the current month is loaded.
#'
#' @note Starting with \pkg{AirSensor} version 0.6, archive file names are 
#' generated with a unique "device-deployment" identifier by combining a unique 
#' location ID with a unique device ID. These "device-deployment" identifiers 
#' guarantee that movement of a sensor will result in the creation of a new
#' time series.
#' 
#' Users may request a \emph{pat} object in one of two ways:
#' 
#' 1) Pass in \code{id} with a valid a \code{deviceDeploymentID}
#' 
#' 2) Pass in both \code{label} and \code{pas} so that the 
#' \code{deviceDeploymentID} can be looked up.
#' 
#' @param id PurpleAir sensor 'deviceDeploymentID'.
#' @param label PurpleAir sensor 'label'.
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param datestamp Date string in ymd order.
#' @param timezone Timezone used to interpret \code{datestamp}.
#' 
#' @return A PurpleAir Timeseries \emph{pat} object.
#' 
#' @seealso \link{pat_load}
#' @seealso \link{pat_loadLatest}
#' @seealso \link{pat_createNew}
#' 
#' @examples
#' \donttest{
#' library(AirSensor)
#' 
#' setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
#' 
#' pas <- pas_load(archival = TRUE)
#' may <- pat_loadMonth(label = "SCNP_20", pas = pas, datestamp = 201905)
#' pat_multiPlot(may)
#' }

pat_loadMonth <- function(
  id = NULL,
  label = NULL,
  pas = NULL,
  datestamp = NULL,
  timezone = "America/Los_Angeles"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  # Get the deviceDeploymentID
  if ( is.null(id) && is.null(label) ) {
    
    stop(paste0("label or id must be provided"))
    
  } else if ( is.null(id) && !is.null(label) ) {
    
    if ( is.null(pas) )
      stop(paste0("pas must be provided when loading by label"))
    
    if ( !label %in% pas$label )
      stop(sprintf("label '%s' is not found in the 'pas' object", label))
    
    # Get the deviceDeploymentID from the label
    deviceDeploymentID <- pas_getDeviceDeploymentIDs(pas, pattern = label)
    
    if ( length(deviceDeploymentID) > 1 )
      stop(sprintf("label '%s' matches more than one sensor", label))
    
  } else {
    
    # Use id whenever it is defined, potentially ignoring label
    deviceDeploymentID <- id
    
  }
  
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
  mmstamp <- strftime(datetime, "%m", tz = "UTC")
  
  # ----- Load data from URL or directory --------------------------------------
  
  # Create filename
  filename <- paste0("pat_", deviceDeploymentID, "_", monthstamp, ".rda")
  
  # Use package internal URL
  baseDir <- getArchiveBaseDir()
  baseUrl <- getArchiveBaseUrl()
  
  dataUrl <- paste0(baseUrl, '/pat/', yearstamp, '/', mmstamp)
  
  # dataDir should be NULL if baseDir is NULL
  if ( is.null(baseDir) ) {
    dataDir <- NULL
  } else {
    dataDir <- paste0(baseDir, '/pat/', yearstamp, '/', mmstamp)
  }
  
  # Get data from URL or directory
  result <- try({
    suppressWarnings( pat <- MazamaCoreUtils::loadDataFile(filename, dataUrl, dataDir) )
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
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  return(pat)
  
}
