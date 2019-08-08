#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.debug
#' 
#' @title Load PurpleAir time series data for a week
#' 
#' @description A pre-generated PurpleAir Timeseries \emph{pat} object will be 
#' loaded containing data for the most recent 7-day interval.
#' 
#' @param label PurpleAir sensor 'label'.
#' @param baseUrl Base URL for \emph{pat} data.
#' 
#' @return A PurpleAir Timeseries \emph{pat} object.
#' 
#' @seealso \link{pat_load}
#' @seealso \link{pat_loadMonth}
#' @seealso \link{pat_createNew}
#' 
#' @examples
#' \donttest{
#' pat <- pat_loadLatest("SCNP_20")
#' pat_multiplot(pat)
#' }

pat_loadLatest <- function(
  label = NULL, 
  baseUrl = "http://smoke.mazamascience.com/data/PurpleAir/pat"
) {
  
  logger.debug("----- pat_loadLatest() -----")
  
  # Validate parameters --------------------------------------------------------
  
  if ( is.null(label) ) 
    stop("Required parameter 'label' is missing.")
  
  # Load data from URL ---------------------------------------------------------
  
  filename <- paste0("pat_", label, "_latest7.rda")
  filepath <- paste0(baseUrl, '/latest/', filename)
  
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
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  return(pat)
  
}

