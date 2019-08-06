#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.debug
#' 
#' @title Load hourly-aggregated Purple Air data for a week
#' 
#' @description A pre-generated \code{airsensor} object will be loaded 
#' containing data for the most recent 7-day interval.
#' 
#' Each \code{airsensor} object contains data from a named collection of 
#' Purple Air sensors.
#' 
#' @param collection Name associated with the collection.
#' @param baseUrl Base URL for \emph{airsensor} data.
#' 
#' @return An object of class "pa_timeseries".
#' 
#' @seealso \link{sensor_load}
#' @seealso \link{sensor_loadMonth}
#' @seealso \link{pat_createAirSensor}
#' 
#' @examples
#' \donttest{
#' sensor_loadLatest("scaqmd", 201905) %>%
#'   PWFSLSmoke::monitor_timeseriesPlot(style = 'gnats')
#' }

sensor_loadLatest <- function(
  collection = "scaqmd",
  baseUrl = "http://smoke.mazamascience.com/data/PurpleAir/airsensor"
) {
  
  # Validate parameters --------------------------------------------------------
  
  if ( is.null(collection) )
    stop("Required parameter 'collection' is missing.")

  # Load data from URL ---------------------------------------------------------
  
  filename <- paste0("airsensor_", collection, "_latest7.rda")
  filepath <- paste0(baseUrl, '/latest/', filename)
  
  # Define a 'connection' object so we can close it no matter what happens
  conn <- url(filepath)
  result <- try({
    suppressWarnings(airsensor <- get(load(conn)))
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
  
  return(invisible(airsensor))
  
}
