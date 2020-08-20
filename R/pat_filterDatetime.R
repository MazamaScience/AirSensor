#' @export
#' @importFrom rlang .data
#' 
#' @title Datetime filtering for PurpleAir Timeseries objects
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param startdate Desired start datetime (ISO 8601) or \code{POSIXct}.
#' @param enddate Desired end datetime (ISO 8601) or \code{POSIXct}.
#' @param timezone Olson timezone used to interpret dates.
#' 
#' @description Subsets a PurpleAir Timeseries object by datetime. This function
#' allows for sub-day filtering as opposed to \code{pat_filterDate()} which
#' always filters to day-boundaries.
#' 
#' Datetimes can be anything that is understood by 
#' \code{MazamaCoreUtils::parseDatetime()}. For non-\code{POSIXct} values,
#' the recommended format is \code{"YYYY-mm-dd HH:MM:SS"}.
#' 
#' Timezone determination precedence assumes that if you are passing in
#' POSIXct times then you know what you are doing.
#' 
#' \enumerate{
#' \item{get timezone from \code{startdate} if it is \code{POSIXct}}
#' \item{use passed in \code{timezone}}
#' \item{get timezone from \code{pat}}
#' }
#' 
#' @return A subset of the given \emph{pat} object.
#' 
#' @seealso \link{pat_filter}
#' @seealso \link{pat_filterDate}
#' @examples
#' library(AirSensor)
#' 
#' example_pat %>%
#'   pat_filterDatetime(
#'     startdate = "2018-08-08 06:00:00", 
#'     enddate = "2018-08-14 18:00:00"
#'   ) %>%
#'   pat_multiPlot()
#'

pat_filterDatetime <- function(
  pat = NULL, 
  startdate = NULL, 
  enddate = NULL, 
  timezone = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  MazamaCoreUtils::stopIfNull(startdate)
  MazamaCoreUtils::stopIfNull(enddate)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
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
  
  timeRange <- MazamaCoreUtils::timeRange(
    starttime = startdate, 
    endtime = enddate, 
    timezone = timezone,
    unit = "sec",
    ceilingStart = FALSE,
    ceilingEnd = FALSE
  )
  
  if (timeRange[1] > pat$data$datetime[length(pat$data$datetime)] |
      timeRange[2] < pat$data$datetime[1])
    stop("pat does not contain requested date range")
  
  
  # ----- Subset the "pat" object ----------------------------------------------
  
  data <- 
    pat$data %>%
    dplyr::filter(.data$datetime >= timeRange[1]) %>%
    dplyr::filter(.data$datetime < timeRange[2])
  
  pat$data <- data
  
  # ----- Return ---------------------------------------------------------------
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  return(pat)
  
}
