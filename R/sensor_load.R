#' @export
#' @importFrom rlang .data
#' 
#' @title Load hourly-aggregated PurpleAir data
#' 
#' @description A pre-generated \code{airsensor} object will be loaded for
#' the given time interval. Archived data for SCAQMD sensors go back to 
#' January, 2018.
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
#' By default, the current week is loaded.
#'
#' @param collection Name associated with the collection.
#' @param startdate Desired start datetime (ISO 8601).
#' @param enddate Desired end datetime (ISO 8601).
#' @param days Number of days of data to include (7 or 45).
#' @param timezone Timezone used to interpret start and end dates.
#' 
#' @return An object of class "airsensor".
#' 
#' @seealso \link{sensor_loadMonth}
#' @seealso \link{sensor_loadYear}
#' 
#' @examples
#' \donttest{
#' library(AirSensor)
#' 
#' setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
#' 
#' sensor_load("scaqmd", 20200411, 20200521) %>%
#'   PWFSLSmoke::monitor_timeseriesPlot(style = 'gnats')
#' }

sensor_load <- function(
  collection = "scaqmd",
  startdate = NULL, 
  enddate = NULL, 
  days = 7, 
  timezone = "America/Los_Angeles"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(collection)
  
  # Quick return if no dates provided
  if ( is.null(startdate) && is.null(enddate) ) 
    return( sensor_loadLatest(collection) )
  
  # Get the date range
  dateRange <- MazamaCoreUtils::dateRange(
    startdate = startdate, 
    enddate = enddate, 
    timezone = timezone,
    unit = "hour",
    ceilingStart = FALSE,
    ceilingEnd = FALSE,
    days = days
  )
  
  # NOTE:  datestamps here are created with the local timezone. It is the job of
  # NOTE:  sensor_loadMonth() to convert these into UTC for use in constructing
  # NOTE:  data file URLs.
  
  # ----- Assemble archive files -----------------------------------------------
  
  # Set up empty list
  airsensorList <- list()
  
  # * annual archive files -----
  
  datestamps <-
    seq(dateRange[1], dateRange[2], by = "days") %>%
    strftime(format = "%Y", tz = timezone) %>%
    unique() %>%
    sort()
  
  for ( datestamp in datestamps ) { 
    
    # Ignore "no data file" errors (likely for future months)
    result <- try({
      
      # NOTE:  sensor_loadYear() interperets datestamp in the local timezone
      # NOTE:  and converts it to UTC.
      airsensorList[[datestamp]] <- 
        sensor_loadYear(
          collection = collection, 
          datestamp = datestamp, 
          timezone = timezone
        )
      
    }, silent = TRUE)
    
    if ( "try-error" %in% class(result) ) {
      err_msg <- geterrmessage()
      if ( stringr::str_detect(err_msg, "file could not be loaded") ) {
        # Ignore
      } else {
        stop(err_msg)
      }
    }
    
    # TODO:  Remove this when annual sensor objects get fixed to never have gaps.
    stepSizeCount <- 
      airsensorList[[datestamp]]$data$datetime %>%
      as.numeric() %>%
      diff() %>%
      unique() %>%
      length()

    if ( stepSizeCount > 1 ) {
      airsensorList <- list()
    }
    
    
  } # END of search for annual files
  
  
  # * monthly archive files -----
  
  # If no annual files are found, try to asssemble monthly archive files
  if ( length(airsensorList) == 0 ) {
    
    datestamps <-
      seq(dateRange[1], dateRange[2], by = "days") %>%
      strftime(format = "%Y%m", tz = timezone) %>%
      unique() %>%
      sort()
    
    for ( datestamp in datestamps ) { 
      
      # Ignore "no data file" errors (likely for future months)
      result <- try({
        
        airsensorList[[datestamp]] <- 
          sensor_loadMonth(
            collection = collection, 
            datestamp = datestamp, 
            timezone = timezone
          )
        
      }, silent = TRUE)
      
      if ( "try-error" %in% class(result) ) {
        err_msg <- geterrmessage()
        if ( stringr::str_detect(err_msg, "file could not be loaded") ) {
          # Ignore
        } else {
          stop(err_msg)
        }
      }
      
    } 
    
  } # END of search for monthly files
  
  if ( length(airsensorList) == 0 ) {
    stop(paste0(
      "No data found in the archive covering the period ",
      strftime(dateRange[1], "%F", tz = timezone), " to ",
      strftime(dateRange[2], "%F %Z", tz = timezone)
      ))
  }
  
  # ----- Join individual sensor objects ---------------------------------------
  
  for ( i in seq_along(airsensorList) ) {
    
    if ( i == 1 ) {
      
      airsensor <- airsensorList[[i]]
      
    } else {
      
      a <- airsensor
      b <- airsensorList[[i]]
      
      airsensor <- sensor_join(a, b)
      
    }
    
  }
  
  # Restrict the time axis
  airsensor <-
    airsensor %>%
    sensor_filterDatetime(
      startdate = startdate,
      enddate = enddate,
      timezone = timezone
    )
  
  # ----- Return ---------------------------------------------------------------
  
  return(airsensor)
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  collection = "scaqmd"
  startdate = 20190411
  enddate = 20190521
  days = 7
  timezone = "America/Los_Angeles"

}