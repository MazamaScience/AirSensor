#' @export
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' 
#' @title Load Purple Air Timeseries data
#' 
#' @description A pre-generated \code{pa_timeseries} object will be loaded for
#' the given month. Archived data for SCAQMD sensors go back to January, 2018.
#' 
#' The \code{datestamp} can must be in the following format:
#' 
#' \itemize{
#' \item{\code{"YYYYmm"}}
#' }
#' 
#' By default, the current month is loaded.
#'
#' @param label Purple Air 'label'
#' @param datestamp A date string in ymd order.
#' @param timezone Timezone used to interpret datestamp.
#' @param baseUrl Base URL for synoptic data.
#' 
#' @return An object of class "pa_timeseries".
#' 
#' @seealso \link{pat_loadLatest}
#' 
#' @examples
#' \dontrun{
#' pat_loadMonth("SCNP_20", 201905)
#'   pat_multiplot()
#' }

pat_loadMonth <- function(
  label = NULL,
  datestamp = NULL,
  timezone = "America/Los_Angeles",
  baseUrl = "http://smoke.mazamascience.com/data/PurpleAir/pat"
) {
  
  logger.debug("----- pat_loadMonth() -----")
  
  # Validate parameters --------------------------------------------------------
  
  # TODO: Work with lubridate to support all formats
  
  if ( is.null(label) )
    stop("Required parameter 'label' is missing.")

  # Default to the current month
  if ( is.null(datestamp) || datestamp == "" ) {
    now <- lubridate::now(timezone)
    datestamp <- strftime(now, "%Y%m%d")
  }
  
  # Handle the case where the day is already specified
  datestamp <- stringr::str_sub(paste0(datestamp,"01"), 1, 8)
  monthstamp <- stringr::str_sub(datestamp, 1, 6)
  yearstamp <- stringr::str_sub(datestamp, 1, 4)
  
  # Load data from URL ---------------------------------------------------------
  
  filename <- paste0("pat_", monthstamp, "_", label, ".rda")
  filepath <- paste0(baseUrl, '/', yearstamp, '/', filename)
  # Define a 'connection' object so we can close it no matter what happens
  conn <- url(filepath)
  result <- try({
    suppressWarnings(pat <- get(load(conn)))
  }, silent=TRUE )
  close(conn)
    
  # NOTE:  We used suppressWarnings() above so that we can have a more
  # NOTE:  uniform error response for the large variety of reasons that
  # NOTE:  loading might fail.
  
  if ( "try-error" %in% class(result) ) {
    # TODO:  Restore logging when we stop generating "futile.logger" errors
    # TODO:  when logging has not been initialized.
    # # Log the error if logging is enabled. Fail silently otherwise.
    # try({ logger.error("%s", geterrmessage()) }, silent = TRUE)
    stop(paste0("Data file could not be loaded: ", filepath), call.=FALSE)
  }
  
  return(invisible(pat))
  
}