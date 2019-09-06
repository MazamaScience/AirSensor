#' @export
#' @importFrom rlang .data
#' 
#' @title Load hourly-aggregated Purple Air data
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
#' @param days Number of days of data to include.
#' @param timezone Timezone used to interpret start and end dates.
#' 
#' @return An object of class "airsensor".
#' 
#' @seealso \link{sensor_loadMonth}
#' 
#' @examples
#' \donttest{
#' setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
#' sensor_load("scaqmd", 20190411, 20190521) %>%
#'   PWFSLSmoke::monitor_timeseriesPlot(style = 'gnats')
#' }

sensor_load <- 
  function(
    collection = "scaqmd",
    startdate = NULL, 
    enddate = NULL, 
    days = 7, 
    timezone = "America/Los_Angeles"
    ) {
    
    # Validate parameters ------------------------------------------------------
    
    if ( is.null(collection) ) 
      stop("Required parameter 'collection' is missing.")
    
    dateRange <- MazamaCoreUtils::dateRange(startdate, 
                                            enddate, 
                                            timezone, 
                                            days = days)
    
    # Asssemble monthly archive files ------------------------------------------
    
    datestamps <-
      sort(
        unique(
          strftime(
            seq(
              dateRange[1],
              dateRange[2],
              by = "days"
            ), 
            format = "%Y%m",
            tz = timezone
          )
        )
      )
    
    airsensorList <- list()
    
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
    
    # Join monthly objects -----------------------------------------------------
    
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
    
    # Trim to the requested time range
    airsensor <- PWFSLSmoke::monitor_subset(airsensor, tlim = dateRange)

    # Return -------------------------------------------------------------------
    
    return(airsensor)
    
  }

