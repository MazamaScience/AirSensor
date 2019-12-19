#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized
#' 
#' @title Load PurpleAir time series data for a week
#' 
#' @description A pre-generated PurpleAir Timeseries \emph{pat} object will be 
#' loaded containing data for the most recent 7-day interval. Data are loaded
#' from the archive set with either \code{setArchiveBaseUrl()} or 
#' \code{setArchiveBaseDir()} for locally archived files.
#' 
#' @note Starting with \pkg{AirSensor} version 0.6, archive file names are 
#' generated with a "sensor-deployment" identifier by combining a unique 
#' location ID with a unique device ID. These "sensor-deployment" identifiers 
#' guarantee that movement of a sensor will result in the creation of a new
#' time series.
#' 
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param label PurpleAir sensor 'label'.
#' @param id PurpleAir sensor 'ID'.
#' 
#' @return A PurpleAir Timeseries \emph{pat} object.
#' 
#' @seealso \link{pat_load}
#' @seealso \link{pat_loadMonth}
#' @seealso \link{pat_createNew}
#' 
#' @examples
#' \donttest{
#' # TODO:  This needs to be updated to use USFS data
#' setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
#' pat <- pat_loadLatest("SCNP_20")
#' pat_multiplot(pat)
#' }

pat_loadLatest <- function(
  pas = NULL,
  label = NULL,
  id = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pas)
  
  if ( !pas_isPas(pas) )
    stop("Required parameter 'pas' is not a valid 'pa_synoptic' object.")
  
  if ( pas_isEmpty(pas) )
    stop("Required parameter 'pas' has no data.") 
  
  # Get the sensorID
  if ( is.null(id) && is.null(label) ) {
    
    stop(paste0("label or id must be provided"))
    
  } else if ( is.null(id) && !is.null(label) ) {
    
    if ( ! label %in% pas$label )
      stop(sprintf("label '%s' is not found in the 'pas' object", label))
    
    # Get the sensorID from the label
    sensorID <- pas_getIDs(pas, pattern = label)
    
    if ( length(sensorID) > 1 )
      stop(sprintf("label '%s' matches more than one sensor", label))
    
  } else {
    
    if ( ! id %in% pas$ID )
      stop(sprintf("id '%s' is not found in the 'pas' object", id))
    
    sensorID <- id
    
  }
  
  # ----- Load data from URL or directory --------------------------------------
  
  # Create dfilewname
  sensorDeploymentID <- pas_sensorDeploymentID(pas, sensorID)
  filename <- paste0("pat_", sensorDeploymentID, "_latest7.rda")
  
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
    suppressWarnings( pat <- loadDataFile(filename, dataUrl, dataDir) )
  }, silent = TRUE)
  
  # NOTE:  We used suppressWarnings() above so that we can have a more
  # NOTE:  uniform error response for the large variety of reasons that
  # NOTE:  loading might fail.
  
  if ( "try-error" %in% class(result) ) {
    if ( is.null(baseDir) ) {
      stop(paste0("Data file could not be loaded from: ", baseUrl), call.=FALSE)
    } else {
      stop(paste0("Data file could not be loaded from: ", baseDir), call.=FALSE)
    }
  }

  # ----- Return ---------------------------------------------------------------
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  return(pat)
  
}

