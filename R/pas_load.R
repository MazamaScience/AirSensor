#' @export
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @importFrom MazamaCoreUtils logger.isInitialized
#' @importFrom utils capture.output
#' 
#' @title Load PurpleAir synoptic data
#' 
#' @description A pre-generated \emph{pa_synoptic} object will be loaded for
#'   the given date. These files are generated each day and provide
#'   a record of all currently installed PurpleAir sensors for the day of 
#'   interest. With default arguments, this function will always load data 
#'   associated with the most recent pre-generated file -- typically less than 
#'   one hour old.
#' 
#'   The \code{datestamp} can be anything that is understood by 
#'   \code{lubrdiate::ymd()} including either of the following recommended 
#'   formats:
#' 
#' \itemize{
#' \item{\code{"YYYYmmdd"}}
#' \item{\code{"YYYY-mm-dd"}}
#' }
#' 
#' By default, the host computer's date is used.
#' 
#' The \emph{pas} object for a specific hour may be loaded by specifying 
#' \code{datestamp = "YYYYmmddHH"}.
#'
#' @param datestamp Local date string in valid YYYY-mm-dd format. See description. 
#' @param retries Max number of days to go back and try to load if requested 
#' date cannot be retrieved.
#' @param timezone Timezone used to interpret \code{datestamp}.
#' @param archival Logical specifying whether a version should be loaded that
#' includes sensors that have stopped reporting.
#' @param verbose Logical to specify warning and error message display.
#' 
#' @return A PurpleAir Synoptic \emph{pas} object.
#' 
#' @seealso \link{pas_createNew}
#' 
#' @examples
#' \dontrun{
#' setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
#' pas <- pas_load()
#' pas %>% 
#'   pas_filter(stateCode == "CA") %>%
#'   pas_leaflet()
#' }

pas_load <- function(
  datestamp = NULL,
  retries = 30,
  timezone = "America/Los_Angeles",
  archival = FALSE,
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(retries)
  MazamaCoreUtils::stopIfNull(timezone)
  MazamaCoreUtils::stopIfNull(archival)
  
  if ( !timezone %in% OlsonNames() ) {
    stop(paste0("Parameter 'timezone' is not recognized: ", timezone))
  }
  
  if ( is.null(datestamp) ) {
    # All .rda archive files are saved with UTC datestamp filenames
    datestamp <- 
      lubridate::now(tzone = timezone) %>%
      strftime("%Y%m%d", tz = "UTC")
  } else {
    datestamp <- as.character(datestamp)
  }
  
  # Check for all numbers
  if ( !stringr::str_detect(datestamp, "^[0-9]{8}$") ) {
    stop("Parameter 'datestamp' must bee in 'YYYYmmdd' format.")
  }
  
  if ( datestamp <= "20190404" ) {
    # NOTE: The inequality operator coerces a string into numeric
    stop("No 'pas' data available prior to April 5, 2019.") 
  }
  
  # Allow datestamp to be one day past today to handle timezone differences
  tomorrowStamp <- 
    { lubridate::now(tzone = timezone) + lubridate::ddays(1) } %>%
    strftime("%Y%m%d", tz = timezone)
  
  if ( datestamp > tomorrowStamp ) {
    stop("No data available for future dates.")
  }
  
  # ----- Load data from URL or directory --------------------------------------
  
  # Use baseDir if it is set
  baseDir <- getArchiveBaseDir()
  
  if ( !is.null(baseDir) ) {
    if ( !dir.exists(baseDir) ) {
      stop(sprintf("Directory baseDir = '%s' does not exist.", baseDir))
    } else {
      baseUrl <- NULL
    }
  } else {
    baseUrl <- getArchiveBaseUrl()
  }
  
  result <- NULL
  successful <- FALSE
  tries <- 0
  localDate <- lubridate::ymd(datestamp, tz = timezone)
  
  # NOTE:  We let people specify their date of interest in local time but 'pas'
  # NOTE:  files are created with UTC timestamps.
  
  # Keep looking back for a valid data file until all tries are used
  while ( !successful && tries < retries ) {
    
    # Create directory structure and filename
    yearstamp <- strftime(localDate, "%Y", tz = "UTC")
    datestamp <- strftime(localDate, "%Y%m%d", tz = "UTC")
    if ( archival ) {
      filename <- paste0("pas_", datestamp, "_archival.rda")
    } else {
      filename <- paste0("pas_", datestamp, ".rda")
    }
    
    # Use baseDir if it is set
    if ( is.null(baseDir) ) {
      dataDir <- NULL
      dataUrl <- paste0(baseUrl, '/pas/', yearstamp)
    } else {
      dataDir <- paste0(baseDir, '/pas/', yearstamp)
      dataUrl <- NULL
    }
    
    # Get data from URL or directory
    result <- try({
      suppressWarnings({
        if ( verbose ) {
          pas <- MazamaCoreUtils::loadDataFile(filename, dataUrl, dataDir)
        } else {
          # Suppress any messages from `loadDataFile` such as `Error : Data file could not be loaded from: `, etc.
          invisible(capture.output(pas <- MazamaCoreUtils::loadDataFile(filename, dataUrl, dataDir)))
        }
      })
    }, silent = TRUE)

    successful <- !("try-error" %in% class(result))
    localDate <- localDate - lubridate::days(1)
    tries <- tries + 1
    
  }
  
  # NOTE:  We used suppressWarnings() above so that we can have a more
  # NOTE:  uniform error response for the large variety of reasons that
  # NOTE:  loading might fail.

  if ( !successful ) {
    stop(paste-1("Data file could not be loaded after ", retries, " tries"),
         call.=FALSE)
  }

  
  # ----- Return ---------------------------------------------------------------
  
  return(pas)
  
}
