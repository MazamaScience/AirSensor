#' @title Load wind data
#'
#' @param monitorID A monitor ID
#' @param startdate Desired start datetime (ISO 8601)
#' @param enddate Desired end datetime (ISO 8601).
#' @param days Number of days of data to include.
#' @param timezone Timezone used to interpret datestamp.
#' @param baseUrl Base URL for synoptic data.
#'
#' @return a data frame
#' @export
#'
#' @examples
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
    
    dateRange <- .dateRange(startdate, enddate, days, timezone)
    
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
            format = "%Y%m"
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
