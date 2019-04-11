#' @export
#' @importFrom rlang .data
#' 
#' @title Subset PurpleAir time series intervals
#' 
#' @param pat Purple Air Timeseries "pat" object from \code{createPATimeseriesObject()}
#' @param startdate desired start datetime (ISO 8601)
#' @param enddate desired end datetime (ISO 8601)
#' @param days Number of days to include in the filterDate interval
#' @param weeks Number of weeks to include in the filterDate interval
#' @param timezone Olson timezone used to interpret dates
#' 
#' @description Subsets a Purple Air Timeseries object by date. 
#' 
#' Dates can be anything that is understood by \code{lubrdiate::ymd()}
#' including either of the following recommended formats:
#' 
#' \itemize{
#' \item{\code{"YYYYmmdd"}}
#' \item{\code{"YYYY-mm-dd"}}
#' }
#' 
#' @return A subset of the original \emph{pat} object.
#' 

pat_filterDate <- function(
  pat = NULL, 
  startdate = NULL, 
  enddate = NULL, 
  days = NULL, 
  weeks = NULL,
  timezone = "America/Los_Angeles"
  ) {
  
  # Validate parameters --------------------------------------------------------
  
  if ( is.null(pat) )
    stop("Required parameter 'pat' is missing.")
  
  if ( nrow(pat$data) == 0 )
    stop("'pat' object has no data")
  
  if ( is.null(startdate) && !is.null(enddate) )
    stop("At least one of 'startdate' or 'enddate' must be specified")

  # Get the start end end times ------------------------------------------------
  
  if ( !is.null(days) ) {
    days = days
  } else if ( !is.null(weeks) ) {
    days = weeks * 7
  } else {
    days = 7 # default
  }
  
  tlim <- .dateRange(startdate, enddate, days, timezone = timezone)
  
  # Subset the "pat" object ----------------------------------------------------
  
  data <- 
    pat$data %>%
    filter(.data$datetime >= tlim[1]) %>%
    filter(.data$datetime <= tlim[2])
  
  pat$data <- data

  return(pat)
  
}
