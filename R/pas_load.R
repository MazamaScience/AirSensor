#' @export
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @importFrom MazamaCoreUtils logger.debug logger.error
#' 
#' @title Load PurpleAir synoptic data
#' 
#' @description A pre-generated \emph{pa_synoptic} object will be loaded for
#'   the given date. These files are generated each day at 4am California time
#'   and provide a record of all currently installed Purple Air sensors for the
#'   day of interest.
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
#' @param datestamp Date string in ymd order.
#' @param baseUrl Base URL for synoptic data.
#' @param retries Max number of days to go back and try to load if requested 
#'   date cannot be retrieved.
#' 
#' @return A PurpleAir Synoptic \emph{pas} object.
#' 
#' @seealso \link{pas_loadLatest}
#' 
#' @examples
#' \dontrun{
#' pas <- pas_load()
#' pas %>% 
#'   pas_filter(stateCode == "CA") %>%
#'   pas_leaflet()
#' }

pas_load <- function(
  datestamp = strftime(lubridate::today("America/Los_Angeles"), "%Y%m%d"),
  baseUrl = "http://smoke.mazamascience.com/data/PurpleAir/pas/2019",
  retries = 10
) {
  
  logger.debug("----- pas_load() -----")
  
  # Validate parameters --------------------------------------------------------
  
  if ( datestamp <= "20190404" ) 
    stop("No data is available prior to April 5, 2019.")
  
  # Allow datestamp to be one day past today to handle timezone differences
  tomorrow <- lubridate::today("America/Los_Angeles") + lubridate::ddays(1)
  if ( datestamp > strftime(tomorrow, "%Y%m%d") )
    stop("No data is available for future dates.")
  
  # Load data from URL ---------------------------------------------------------
  
  result <- NULL
  loaded <- FALSE
  tries <- 0
  date <- lubridate::ymd(datestamp)
  
  # Keep looking back for a valid data file until all tries are used
  while (!loaded & tries < retries) {
    datestamp <- strftime(date, "%Y%m%d")
    filename <- paste0("pas_", datestamp, ".rda")
    filepath <- paste0(baseUrl, '/', filename)
    
    # Define a 'connection' object so we can close it no matter what happens
    conn <- url(filepath)
    result <- try({
      suppressWarnings(pas <- get(load(conn)))
    }, silent=TRUE )
    close(conn)
    
    loaded <- !("try-error" %in% class(result))
    date <- date - lubridate::days(1)
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
  
  return(invisible(pas))
  
}
