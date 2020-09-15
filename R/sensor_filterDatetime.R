#' @export
#' @importFrom rlang .data
#' 
#' @title Datetime filtering for AirSensor objects
#' 
#' @param sensor An AirSensor object.
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param timezone Olson timezone used to interpret dates.
#' 
#' @description Subsets an AirSensor object by datetime. This function
#' allows for sub-day filtering as opposed to \code{sensor_filterDate()} which
#' always filters to day-boundaries. Filtering will be performed with
#' \code{>= startdate} and \code{< enddate} so that the \code{startdate}
#' timestep will be included in the output but the \code{enddate} will not.
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
#' \item{get timezone from \code{sensor}}
#' }
#' 
#' @return A subset of the given \emph{sensor} object.
#' 
#' @seealso \link{sensor_filter}
#' @seealso \link{sensor_filterDate}
#' 
#' @examples
#' library(AirSensor)
#' 
#' example_sensor %>% 
#'   sensor_extractData() %>%
#'   dplyr::pull("datetime") %>%
#'   range()
#'   
#' example_sensor %>% 
#'   sensor_filterDatetime(
#'     startdate = "2018-08-21 06:00:00", 
#'     enddate = "2018-08-28 18:00:00",
#'     timezone = "UTC"
#'   ) %>%
#'   sensor_extractData() %>%
#'   dplyr::pull("datetime") %>%
#'   range()
#'

sensor_filterDatetime <- function(
  sensor = NULL, 
  startdate = NULL, 
  enddate = NULL, 
  timezone = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(sensor)
  MazamaCoreUtils::stopIfNull(startdate)
  MazamaCoreUtils::stopIfNull(enddate)
  
  if ( !sensor_isSensor(sensor) )
    stop("Parameter 'sensor' is not a valid 'airsensor' object.")
  
  if ( sensor_isEmpty(sensor) )
    stop("Parameter 'sensor' has no data.")
  
  # Timezone determination precedence assumes that if you are passing in
  # POSIXct times then you know what you are doing.
  #   1) get timezone from startdate if it is POSIXct
  #   2) use passed in timezone
  #   3) get timezone from sensor
  
  if ( lubridate::is.POSIXt(startdate) ) {
    timezone <- lubridate::tz(startdate)
  } else {
    if ( is.null(timezone) ) {
      timezone <- sensor$meta$timezone
    }
  }
  
  # ----- Get start and end times ----------------------------------------------
  
  timeRange <- MazamaCoreUtils::timeRange(
    starttime = startdate, 
    endtime = enddate, 
    timezone = timezone,
    unit = "sec",
    ceilingStart = FALSE,
    ceilingEnd = FALSE
  )
  
  if (timeRange[1] > sensor$data$datetime[length(sensor$data$datetime)] |
      timeRange[2] < sensor$data$datetime[1])
    stop("sensor does not contain requested date range")
  
  # ----- Subset the "sensor" object -------------------------------------------
  
  data <- 
    sensor$data %>%
    dplyr::filter(.data$datetime >= timeRange[1]) %>%
    dplyr::filter(.data$datetime < timeRange[2])
  
  sensor$data <- data
  
  return(sensor)
  
}
