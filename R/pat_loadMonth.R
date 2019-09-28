#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.isInitialized logger.error
#' 
#' @title Load PurpleAir time series data for a month
#' 
#' @description A pre-generated PurpleAir Timeseries \emph{pat} object will be 
#' loaded for the given month. Archived data for SCAQMD sensors go back to 
#' January, 2018.
#' 
#' The \code{datestamp} must be in the following format:
#' 
#' \itemize{
#' \item{\code{"YYYYmm"}}
#' }
#' 
#' By default, the current month is loaded.
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
#' @param label Purple Air sensor 'label'
#' @param datestamp Date string in ymd order.
#' @param timezone Timezone used to interpret \code{datestamp}.
#' @param make.names Logical specifying whether to run 
#' \code{make.names(label)} when assembilng the file path.
#' 
#' @return A PurpleAir Timeseries \emph{pat} object.
#' 
#' @seealso \link{pat_load}
#' @seealso \link{pat_loadLatest}
#' @seealso \link{pat_createNew}
#' 
#' @examples
#' \donttest{
#' setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
#' may <- pat_loadMonth("SCNP_20", 201905)
#' pat_multiplot(may)
#' }

pat_loadMonth <- function(
  label = NULL,
  datestamp = NULL,
  timezone = "America/Los_Angeles",
  make.names = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(label)
  
  if ( make.names ) 
    label <- make.names(label)
  
  # ----- Load data from URL or directory --------------------------------------
  
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
  
  filename <- paste0("pat_", label, "_", monthstamp, ".rda")
  dataUrl <- paste0(baseUrl, '/pat/', yearstamp)
  
  # dataDir should be NULL if baseDir is NULL
  if ( is.null(baseDir) ) {
    dataDir <- NULL
  } else {
    dataDir <- paste0(baseDir, '/pat/', yearstamp)
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
