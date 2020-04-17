#' @export
#' @importFrom rlang .data
#' 
#' @title Date filtering for PurpleAir Timeseries objects
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param days Number of days to include in the filterDate interval.
#' @param weeks Number of weeks to include in the filterDate interval.
#' @param timezone Olson timezone used to interpret dates.
#' 
#' @description Subsets a PurpleAir Timeseries object by date. 
#' 
#' Dates can be anything that is understood by \code{lubridate::ymd()}
#' including either of the following recommended formats:
#' 
#' \itemize{
#' \item{\code{"YYYYmmdd"}}
#' \item{\code{"YYYY-mm-dd"}}
#' }
#' 
#' @note The returned data will run from the beginning of \code{startdate} until
#' the \strong{beginning} of \code{enddate} -- \emph{i.e.} no values associated
#' with \code{enddate} will be returned. The exception being when
#' \code{enddate} is less than 24 hours after \code{startdate}. In that case, a
#' single day is returned.
#' 
#' @return A subset of the given \emph{pat} object.
#' 
#' @examples
#' library(AirSensor)
#' 
#' august08_15 <- pat_filterDate(example_pat, 
#'                               startdate = 20180808, 
#'                               enddate = 20180815)
#' pat_multiplot(pat = august08_15)
#'

pat_filterDate <- function(
  pat = NULL, 
  startdate = NULL, 
  enddate = NULL, 
  days = NULL, 
  weeks = NULL,
  timezone = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  if ( is.null(startdate) && !is.null(enddate) )
    stop("At least one of 'startdate' or 'enddate' must be specified")
  
  # Timezone determination precedence assumes that if you are passing in
  # POSIXct times then you know what you are doing.
  #   1) get timezone from startdate if it is POSIXct
  #   2) use passed in timezone
  #   3) get timezone from pat
  
  if ( lubridate::is.POSIXt(startdate) ) {
    timezone <- lubridate::tz(startdate)
  } else {
    if ( is.null(timezone) ) {
      timezone <- pat$meta$timezone
    }
  }
  
  # ----- Get the start and end times ------------------------------------------
  
  if ( !is.null(days) ) {
    days <- days
  } else if ( !is.null(weeks) ) {
    days <- weeks * 7
  } else {
    days <- 7 # default
  }
  
  dateRange <- MazamaCoreUtils::dateRange(
    startdate = startdate, 
    enddate = enddate, 
    timezone = timezone,
    unit = "sec",
    ceilingEnd = FALSE,
    days = days
  )
  
  if (dateRange[1] > pat$data$datetime[length(pat$data$datetime)] |
      dateRange[2] < pat$data$datetime[1])
    stop("pat does not contain requested date range")
  
  
  # ----- Subset the "pat" object ----------------------------------------------
  
  data <- 
    pat$data %>%
    dplyr::filter(.data$datetime >= dateRange[1]) %>%
    dplyr::filter(.data$datetime < dateRange[2])
  
  pat$data <- data
  
  # ----- Return ---------------------------------------------------------------
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  return(pat)
  
}
