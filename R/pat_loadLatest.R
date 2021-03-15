#' @export
#' @importFrom rlang .data
#' 
#' @title Load PurpleAir time series data for a week
#' 
#' @description A pre-generated PurpleAir Timeseries \emph{pat} object will be 
#' loaded containing data for the most recent 7- or 45-day interval. Data are
#' loaded from the archive set with either \code{setArchiveBaseUrl()} or 
#' \code{setArchiveBaseDir()} for locally archived files.
#' 
#' @note Archive file names are 
#' generated with a unique "device-deployment" identifier by combining a unique 
#' location ID with a unique device ID. These \code{deviceDeploymentID} 
#' identifiers guarantee that movement of a sensor will result in the creation 
#' of a new time series.
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
#' @param days Number of days of data to include (7 or 45).
#' 
#' @return A PurpleAir Timeseries \emph{pat} object.
#' 
#' @seealso \link{pat_load}
#' @seealso \link{pat_loadMonth}
#' @seealso \link{pat_createNew}
#' 
#' @examples
#' \donttest{
#' library(AirSensor)
#' 
#' setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
#' 
#' pas <- pas_load()
#' pat <- pat_loadLatest(label = "SCSB_07", pas = pas)
#' pat_multiPlot(pat)
#' }

pat_loadLatest <- function(
  id = NULL,
  label = NULL,
  pas = NULL,
  days = 7
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(days)
  days <- as.numeric(days)
  
  if ( !days %in% c(7, 45) )
    stop("Parameter 'days' must be either 7 or 45")

  
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
  
  # ----- Load data from URL or directory --------------------------------------
  
  # Create filename
  filename <- paste0("pat_", deviceDeploymentID, "_latest", days, ".rda")
  
  # Use package internal URL
  baseDir <- getArchiveBaseDir()
  baseUrl <- getArchiveBaseUrl()
  
  dataUrl <- paste0(baseUrl, '/pat/latest')
  
  # dataDir should be NULL if baseDir is NULL
  if ( is.null(baseDir) ) {
    dataDir <- NULL
  } else {
    dataDir <- paste0(baseDir, '/pat/latest')
  }
  
  # Get data from URL or directory
  result <- try({
    suppressWarnings({ 
      pat <- MazamaCoreUtils::loadDataFile(filename, dataUrl, dataDir)
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
  pat$meta$ID <- as.character(pat$meta$ID)
  pat$meta$deviceID <- as.character(pat$meta$deviceID)
  
  # Guarantee that 'uptime' and 'memory' are <dbl> as opposed to <int> as they 
  # were in an earlier version
  pat$data$uptime <- as.double(pat$data$uptime)
  pat$data$memory <- as.double(pat$data$memory)
  
  # Guarantee that times are arranged properly
  pat$data <- 
    pat$data %>%
    dplyr::arrange(.data$datetime)
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  return(pat)
  
}

