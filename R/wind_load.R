#' @title Load wind data
#'
#' @param monitorID A monitor ID.
#' @param startdate Desired start datetime (ISO 8601)
#' @param enddate Desired end datetime (ISO 8601).
#' @param days Number of days of data to include.
#' @param timezone Timezone used to interpret datestamp.
#' @param baseUrl Base URL for synoptic data.
#' 
#' @description Load pre-generated wind data for the given interval. Archived 
#' data appropriate for use with SCAQMD sensors go back to January, 2018. 
#' 
#' @return A data frame with columns \code{"date", "wd", "ws"}.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' wind_load(
#'   monitorID = "060950004_01",
#'   startdate = "20180521", 
#'   enddate = "20180627"
#' ) 
#' }
#' 

wind_load <-
  function(
    monitorID = NULL, 
    startdate = NULL, 
    enddate = NULL, 
    days = 7, 
    timezone = "America/Los_Angeles",
    baseUrl = "http://smoke.mazamascience.com/data/PurpleAir/wind"
  ) {
    
    # ----- Validate parameters ------------------------------------------------
    
    if ( is.null(monitorID) ) 
      stop("Required parameter 'label' is missing.")
    
    dateRange <- MazamaCoreUtils::dateRange(startdate, enddate, timezone, days)
    
    # ----- Asssemble monthly archive files ------------------------------------
    
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
    
    windList <- list()
    
    for ( datestamp in datestamps ) { 
      
      windList[[datestamp]] <- 
        wind_loadMonth(
          monitorID = monitorID, 
          datestamp = datestamp, 
          timezone = timezone,
          baseUrl = baseUrl
        )
      
    } 
    
    # ---- Return --------------------------------------------------------------
    
    # Join dfs
    windData <- 
      purrr::reduce(
        windList, 
        dplyr::full_join, 
        by = c("date", "wd", "ws")
      )
    
    # Filter dates
    windObj <- 
      dplyr::filter(
        windData, 
        dateRange[1] <= windData$date &
          dateRange[2] >= windData$date
      )
    
    return(windObj)
    
  }
