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
#' 
#' @examples
#' \donttest{
#' library(AirSensor)
#' 
#' setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
#' 
#' sensor_load("scaqmd", 20190411, 20190521) %>%
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
  dateRange <- MazamaCoreUtils::dateRange(startdate, 
                                          enddate, 
                                          timezone, 
                                          days = days)
  
  # NOTE:  datestamps here are created with the local timezone. It is the job of
  # NOTE:  sensor_loadMonth() to convert these into UTC for use in constructing
  # NOTE:  data file URLs.
  
  # ----- Assemble archive files -----------------------------------------------
  
  # TODO:  sensor_load.R needs a lot of work to properly join including:
  # TODO:   - trim to months so there aren't overlaps
  # TODO:   - separate month1_only IDS, shared_IDS, month2_only IDS
  # TODO:   - create ws_monitor objects for 1_only 1_shared, 2_shared, 2_only
  # TODO:   - monitor_join(1_shared, 2_shared)
  # TODO:   - monitor_combine(1_only, 2_joined, 2_only)
  
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
      strftime(dateRange[1], "%F"), " to ",
      strftime(dateRange[2], "%F %Z")
      ))
  }
  
  # ----- Join individual sensor objects ---------------------------------------
  
  for ( i in seq_along(airsensorList) ) {
    
    if ( i == 1 ) {
      
      airsensor <- airsensorList[[i]]
      
    } else {
      
      a <- airsensor
      
      # Prepare b
      b <- airsensorList[[i]]
      b_mint <- max(a$data$datetime) + lubridate::dhours(1)
      b_maxt <- max(b$data$datetime)
      b <- PWFSLSmoke::monitor_subset(b, tlim = c(b_mint, b_maxt))
      
      # Split up into A only, B only and AB shared
      a_only_IDs <- setdiff(a$meta$monitorID, b$meta$monitorID)
      ab_shared_IDs <- intersect(a$meta$monitorID, b$meta$monitorID)
      b_only_IDs <- setdiff(b$meta$monitorID, a$meta$monitorID)
      
      a_only <- PWFSLSmoke::monitor_subset(a, monitorIDs = a_only_IDs, dropMonitors = FALSE)
      b_only <- PWFSLSmoke::monitor_subset(b, monitorIDs = b_only_IDs, dropMonitors = FALSE)
      
      a_shared <- PWFSLSmoke::monitor_subset(a, monitorIDs = ab_shared_IDs, dropMonitors = FALSE)
      b_shared <- PWFSLSmoke::monitor_subset(b, monitorIDs = ab_shared_IDs, dropMonitors = FALSE)
      
      # TODO:  Figure out what is not working with PWFSLSmoke::monitor_join()
      #ab_shared <- PWFSLSmoke::monitor_join(a, b)
      
      ab_shared <- a_shared
      ab_shared$data <- dplyr::bind_rows(a_shared$data, b_shared$data)
      
      airsensor <- 
        PWFSLSmoke::monitor_combine(list(a_only, b_only, ab_shared))
      
    }
    
  }
  
  # Cleanup any NaN or Inf that might have snuck in
  data <-
    airsensor$data %>%
    dplyr::mutate_all( function(x) replace(x, which(is.nan(x)), NA) ) %>%
    dplyr::mutate_all( function(x) replace(x, which(is.infinite(x)), NA) )
  
  airsensor$data <- data
  
  # ----- Return ---------------------------------------------------------------
  
  # Trim to the requested time range
  airsensor <- PWFSLSmoke::monitor_subset(airsensor, tlim = dateRange)
  
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