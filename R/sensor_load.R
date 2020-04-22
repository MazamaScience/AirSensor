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
#' # TODO:  Restore example when data become available
#' #setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
#' #sensor_load("scaqmd", 20190411, 20190521) %>%
#' #  PWFSLSmoke::monitor_timeseriesPlot(style = 'gnats')
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
  # NOTE:  pat_loadMonth() to convert these into UTC for use in constructing
  # NOTE:  data file URLs.
  
  # ----- Assemble annual archive files ----------------------------------------
  
  # Set up empty list
  airsensorList <- list()
  
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
    
  } 
  
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
      
      # Be sure to retain all monitorIDs
      monitorIDs <- 
        union(airsensor$meta$monitord, airsensorList[[i]]$meta$monitorID)
      
      airsensor <- 
        PWFSLSmoke::monitor_join(airsensor, airsensorList[[i]],
                                 monitorIDs = monitorIDs)
      
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

