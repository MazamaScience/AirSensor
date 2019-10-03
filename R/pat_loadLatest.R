#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized logger.error
#' 
#' @title Load PurpleAir time series data for a week
#' 
#' @description A pre-generated PurpleAir Timeseries \emph{pat} object will be 
#' loaded containing data for the most recent 7-day interval.
#' 
#' @note Many Purple Air sensor labels have spaces and other special characters
#' which make for awkward file names. Best practices would suggest creating
#' file names without special characters by running the \code{label} string
#' through \code{make.names()} first.
#'  
#' This function defaults to generating file names in this manner but allows
#' users to override this in case some users have a compelling reason to create 
#' filenames that exactly match the \code{pat$label} of the \emph{pat} object
#' they contain.
#' 
#' @param label PurpleAir sensor 'label'.
#' @param make.names Logical specifying whether to run 
#' \code{make.names(label)} when assembilng the file path.
#' 
#' @return A PurpleAir Timeseries \emph{pat} object.
#' 
#' @seealso \link{pat_load}
#' @seealso \link{pat_loadMonth}
#' @seealso \link{pat_createNew}
#' 
#' @examples
#' \donttest{
#' setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
#' pat <- pat_loadLatest("SCNP_20")
#' pat_multiplot(pat)
#' }

pat_loadLatest <- function(
  label = NULL,
  make.names = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(label)
  
  if ( make.names ) 
    label <- make.names(label)
  
  # ----- Load data from URL or directory --------------------------------------
  
  # Use package internal URL
  baseDir <- getArchiveBaseDir()
  baseUrl <- getArchiveBaseUrl()

  filename <- paste0("pat_", label, "_latest7.rda")
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
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  return(pat)
  
}

