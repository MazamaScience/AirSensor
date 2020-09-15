#' @export
#' @importFrom rlang .data
#' 
#' @title Date filtering for AirSensor objects
#' 
#' @param sensor An AirSensor object.
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param days Number of days to include in the filterDate interval.
#' @param weeks Number of weeks to include in the filterDate interval.
#' @param timezone Olson timezone used to interpret dates.
#' 
#' @description Subsets an AirSensor object by date. 
#' 
#' Dates can be anything that is understood by \code{lubrdiate::ymd()}
#' including either of the following recommended formats:
#' 
#' \itemize{
#' \item{\code{"YYYYmmdd"}}
#' \item{\code{"YYYY-mm-dd"}}
#' }
#' 
#' @return A subset of the given \emph{sensor} object.
#' 
#' @seealso \link{sensor_filter}
#' @seealso \link{sensor_filterMeta}
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
#'   sensor_filterDate(
#'     startdate = "2018-08-21", 
#'     enddate = "2018-08-28",
#'     timezone = "UTC"
#'   ) %>%
#'   sensor_extractData() %>%
#'   dplyr::pull("datetime") %>%
#'   range()
#'   

sensor_filterDate <- function(
  sensor = NULL, 
  startdate = NULL, 
  enddate = NULL, 
  days = NULL, 
  weeks = NULL,
  timezone = "America/Los_Angeles"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(sensor)
  MazamaCoreUtils::stopIfNull(timezone)
  
  if ( !sensor_isSensor(sensor) )
    stop("Parameter 'sensor' is not a valid 'airsensor' object.") 
  
  if ( sensor_isEmpty(sensor) ) 
    stop("Parameter 'sensor' has no data.")
  
  if ( is.null(startdate) && !is.null(enddate) )
    stop("At least one of 'startdate' or 'enddate' must be specified")
  
  # ----- Get start and end times ----------------------------------------------
  
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
    ceilingStart = FALSE,
    ceilingEnd = FALSE,
    days = days
  )
  
  # ----- Subset the "sensor" object -------------------------------------------
  
  data <- 
    sensor$data %>%
    dplyr::filter(.data$datetime >= dateRange[1]) %>%
    dplyr::filter(.data$datetime < dateRange[2])
  
  sensor$data <- data
  
  return(sensor)
  
}
