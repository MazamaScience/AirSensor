#' @keywords internal
#' @export
#' @importFrom MazamaCoreUtils logger.isInitialized logger.error
#'
#' @title Load data from URL or local file
#'
#' @param filename Name of the data file to be loaded.
#' @param dataUrl Remote URL directory for data files.
#' @param dataDir Local directory containing data files.
#' @return A data object.
#' 
#' @description Loads pre-generated .rda files from a URL or a local
#' directory. This function is intended to be called by other \code{~_load()}
#' functions and can remove internet latencies when local versions of daata are
#' available.
#' 
#' For this reason, specification of \code{dataDir} always takes precedence over 
#' \code{dataUrl}.

loadDataFile <- function(
  filename = NULL,
  dataUrl = NULL,
  dataDir = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(filename)
  
  if ( is.null(dataUrl) && is.null(dataDir) ) {
    stop("Either 'dataUrl' or 'dataDir' must be specified.")
  }

  # ----- Load the data --------------------------------------------------------

  result <- try({
    
    # Always check for dataDir first
    if ( 
      !is.null(dataDir) && 
      !is.na(dataDir) && 
      dir.exists(path.expand(dataDir)) 
    ) {
      
      # Load from a file
      filepath <- file.path(path.expand(dataDir), filename)
      
      result <- try({
        objectName <- load(filepath)
      }, silent = TRUE)
      
      if ( "try-error" %in% class(result) ) {
        stop(paste0("Data file could not be loaded from: ", filepath), call.=FALSE)
      } else {
        loadedData <- get(objectName)
      }
      
    } else {
      
      # Load from a URL
      filepath <- paste0(dataUrl, '/', filename)
      
      # Define a 'connection' object so we can close it no matter what happens
      conn <- url(filepath)
      result <- try({
        objectName <- load(conn)
      }, silent=TRUE )
      close(conn)
      
      if ( "try-error" %in% class(result) ) {
        stop(paste0("Data file could not be loaded from: ", filepath), call.=FALSE)
      } else {
        loadedData <- get(objectName)
      }
      
    }
    
  })

  # ----- Handle errors --------------------------------------------------------
  
  # NOTE:  Failures should be handled above but just in case.

  if ( "try-error" %in% class(result) ) {
    stop(paste0("Data file could not be loaded from: ", filepath), call.=FALSE)
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(loadedData)
  
}
