#' @export
#' 
#' @title Create a POSIXct date range
#' 
#' @param startdate desired start datetime (ISO 8601)
#' @param enddate desired end datetime (ISO 8601)
#' @param days number of days of data to include
#' @param timezone Olson timezone used to interpret dates
#' @param unit units used to determine enddates final timestep
#' 
#' @description Uses incoming parameters to return a pair of \code{POSIXct}
#' times in the proper order. This function always rounds to day boundaries.
#' 
#' Dates can be anything that is understood by 
#' \code{lubrdiate::parse_date_time()} including either of the following 
#' recommended formats:
#' 
#' \itemize{
#' \item{\code{"YYYYmmdd"}}
#' \item{\code{"YYYY-mm-dd"}}
#' }
#' 
#' @note The ending \code{POSIXct} will end one \code{unit} before the specified
#' \code{enddate}.
#' 
#' @return A vector of two \code{POSIXct}s.

.dateRange <- function(
  startdate = NULL, 
  enddate = NULL, 
  days = 7, 
  timezone = "UTC",
  unit = "sec"
) {
  
  # ----- Determine starttime and endtime --------------------------------------
  
  if ( stringr::str_detect(unit, "^day") ) {
    daySecs <- 0
  } else if ( stringr::str_detect(unit, "^day") ) {
    daySecs <- 60 * 60 * 23
  } else if ( stringr::str_detect(unit, "^min") ){
    daySecs <- 60 * 60 * 23 - (60 * 1)
  } else if ( stringr::str_detect(unit, "^sec") ) {
    daySecs <- 60 * 60 * 24 - (1)
  }
  
  orders <- c("Ymd","YmdH","YmdHM","YmdHMS")
  
  if ( !is.null(startdate) && !is.null(enddate) ) {
    
    # Both found:  use startdate, enddate
    endtime <- enddate %>%
      lubridate::parse_date_time(orders = orders, tz = timezone) %>%
      lubridate::floor_date(unit = "day") + 
      lubridate::dseconds(daySecs)
    starttime <- startdate %>%
      lubridate::parse_date_time(orders = orders, tz = timezone) %>%
      lubridate::floor_date(unit = "day")
    
  } else if ( is.null(startdate) && !is.null(enddate) ) {
    
    # Missing startdate:  use (enddate - days), enddate
    endtime <- enddate %>%
      lubridate::parse_date_time(orders = orders, tz = timezone) %>%
      lubridate::floor_date(unit = "day") + 
      lubridate::dseconds(daySecs)               # end of day
    starttime <- endtime %>%
      lubridate::floor_date(unit = "day") -      # beginning of day
      lubridate::ddays(days-1)                   # any extra days
    
  } else if ( !is.null(startdate) && is.null(enddate) ) {
    
    # Missing enddate:  use startdate, (startdate + days)
    starttime <- startdate %>%
      lubridate::parse_date_time(orders = orders, tz = timezone) %>%
      lubridate::floor_date(unit = "day")
    endtime <-  starttime %>%
      lubridate::floor_date(unit = "day") + 
      lubridate::dseconds(daySecs) +             # end of day
      lubridate::ddays(days-1)                   # any extra days
    
  } else {
    
    # Both missing:  use (now - days), now
    endtime <-  lubridate::now(timezone) %>%
      lubridate::floor_date(unit = "day" )
    starttime <- endtime %>%
      lubridate::floor_date(unit = "day") - 
      lubridate::ddays(days)
    
  }
  
  # Define tlim
  if ( starttime < endtime ) {
    tlim <- c(starttime,endtime)  
  } else {
    # just in case
    tlim <- c(endtime,starttime)  
  }
  
  return(tlim)
  
}
