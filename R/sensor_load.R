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
#' @param baseUrl Base URL for the \code{airsensor} data archive.
#' 
#' @return An object of class "airsensor".
#' 
#' @seealso \link{sensor_loadMonth}
#' 
#' @examples
#' \donttest{
#' sensor_load("scaqmd", 20190411, 20190521) %>%
#'   PWFSLSmoke::monitor_timeseriesPlot(style = 'gnats')
#' }

sensor_load <- 
  function(
    collection = "scaqmd",
    startdate = NULL, 
    enddate = NULL, 
    days = 7, 
    timezone = "America/Los_Angeles",
    baseUrl = "http://smoke.mazamascience.com/data/PurpleAir/airsensor"
    ) {
    
    # Validate parameters ------------------------------------------------------
    
    if ( is.null(collection) ) 
      stop("Required parameter 'collection' is missing.")
    
    dateRange <- MazamaCoreUtils::dateRange(startdate, enddate, timezone, days)
    
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
      
      airsensorList[[datestamp]] <- 
        sensor_loadMonth(
          collection = collection, 
          datestamp = datestamp, 
          timezone = timezone,
          baseUrl = baseUrl
        )
      
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
    

    # Return -------------------------------------------------------------------
    
    return(airsensor)
    
  }

