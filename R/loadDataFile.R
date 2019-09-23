#' @keywords internal
#' @export
#' @importFrom MazamaCoreUtils logger.isInitialized logger.error
#'
#' @title Load data from URL or local file
#'
#' @param filename Name of the data file to be loaded.
#' @param baseUrl Base URL for data files.
#' @param dataDir Local directory containing data files.
#' @return A data object.
#' 
#' @description Loads pre-generated .rda RData files from a URL or a local
#' directory. This function is intended to be called by other \code{~_load()}
#' functions and can remove internet latencies when local versions of daata are
#' available.
#' 
#' For this reason, specification of \code{dataDir} always takes precedence over 
#' \code{baseUrl}.

loadDataFile <- function(
  filename = NULL,
  baseUrl = NULL,
  dataDir = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(filename)
  
  if ( is.null(baseUrl) && is.null(dataDir) ) {
    stop("Either 'baseUrl' or 'dataDir' must be specified.")
  }

  # ----- Load the data --------------------------------------------------------

  # Always check for dataDir first
  if ( !is.null(dataDir) ) {

    # Load from a file
    filepath <- file.path(path.expand(dataDir), filename)
    result <- try({
      suppressWarnings(loadedData <- get(load(filepath)))
    }, silent = TRUE)
    
  } else {

    # Load from a URL
    filepath <- paste0(baseUrl, filename)
    # Define a 'connection' object so we can close it no matter what happens
    conn <- url(filepath)
    result <- try({
      suppressWarnings(loadedData <- get(load(conn)))
    }, silent=TRUE )
    close(conn)
    
  }

  # ----- Handle errors --------------------------------------------------------
  
  # NOTE:  We used suppressWarnings() above so that we can have a more
  # NOTE:  uniform error response for the large variety of reasons that
  # NOTE:  loading might fail.

  if ( "try-error" %in% class(result) ) {
    if ( logger.isInitialized() ) {
      logger.error("%s", geterrmessage())
    }
    stop(paste0("Data file could not be loaded from: ", filepath), call.=FALSE)
  }

  # ----- Return ---------------------------------------------------------------
  
  return(loadedData)
  
}
