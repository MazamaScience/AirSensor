#' @export
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @importFrom MazamaCoreUtils logger.debug logger.error
#' 
#' @title Load PurpleAir synoptic data
#' 
#' @description A pre-generated \emph{pa_synoptic} object will be loaded for
#'   the given date. These files are generated throughout each day and provide
#'   a record of all currently installed Purple Air sensors for the day of 
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
#' By default, today's date is used.
#' 
#' The \emph{pas} object for a specific hour may be loaded by specifying 
#' \code{datestamp = "YYYYmmddHH"}.
#'
#' @param datestamp Local date string in ymd order.
#' @param retries Max number of days to go back and try to load if requested 
#'   date cannot be retrieved.
#' @param timezone Timezone used to interpret \code{datestamp}.
#' @param archival Logical specifying whether a version should be loaded that
#' includes sensors that have stopped reporting.
#' 
#' @return A PurpleAir Synoptic \emph{pas} object.
#' 
#' @seealso \link{pas_createNew}
#' 
#' @examples
#' \dontrun{
#' setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
#' pas <- pas_load()
#' pas %>% 
#'   pas_filter(stateCode == "CA") %>%
#'   pas_leaflet()
#' }

pas_load <- function(
  datestamp = strftime(lubridate::today("America/Los_Angeles"),
                       "%Y%m%d",
                       tz = "America/Los_Angeles"),
  retries = 30,
  timezone = "America/Los_Angeles",
  archival = FALSE
) {
  
  logger.debug("----- pas_load() -----")
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( datestamp <= "20190404" ) 
    stop("No 'pas' data is available prior to April 5, 2019.")
  
  # Allow datestamp to be one day past today to handle timezone differences
  tomorrow <- lubridate::today(timezone) + lubridate::ddays(1)
  
  if ( datestamp > strftime(tomorrow, "%Y%m%d", tz = timezone) )
    stop("No data is available for future dates.")
  
  # ----- Load data from URL ---------------------------------------------------
  
  # Use package internal URL
  baseUrl <- getArchiveBaseUrl()
  
  result <- NULL
  successful <- FALSE
  tries <- 0
  localDate <- lubridate::ymd(datestamp, tz = timezone)
  
  # NOTE:  We let people specify their date of interest in local time but 'pas'
  # NOTE:  files are created with UTC timestamps.
  
  # Keep looking back for a valid data file until all tries are used
  while (!successful && tries < retries) {
    yearstamp <- strftime(localDate, "%Y", tz = "UTC")
    datestamp <- strftime(localDate, "%Y%m%d", tz = "UTC")
    if ( archival ) {
      filename <- paste0("pas_", datestamp, "_archival.rda")
    } else {
      filename <- paste0("pas_", datestamp, ".rda")
    }
    filepath <- paste0(baseUrl, '/pas/', yearstamp, '/', filename)
    
    # Define a 'connection' object so we can close it no matter what happens
    conn <- url(filepath)
    result <- try({
      suppressWarnings(pas <- get(load(conn)))
    }, silent=TRUE )
    close(conn)
    
    successful <- !("try-error" %in% class(result))
    localDate <- localDate - lubridate::days(1)
    tries <- tries + 1
  }
  
  # NOTE:  We used suppressWarnings() above so that we can have a more
  # NOTE:  uniform error response for the large variety of reasons that
  # NOTE:  loading might fail.
  
  if ( "try-error" %in% class(result) ) {
    # TODO:  Restore logging when we stop generating "futile.logger" errors
    # TODO:  when logging has not been initialized.
    # # Log the error if logging is enabled. Fail silently otherwise.
    # try({ logger.error("%s", geterrmessage()) }, silent = TRUE)
    stop(paste0("Data file could not be loaded after ", retries, " tries"), 
         call.=FALSE)
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(invisible(pas))
  
}
