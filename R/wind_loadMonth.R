#' @title Load monthly wind data
#'
#' @param monitorID A monitorID.
#' @param datestamp Date string in ym or ymd order
#' @param timezone Timezone used to interpret datestamp.
#' @param baseUrl Base URL for wind data.
#'
#' @description Load pre-generated wind data for the given month. Archived data 
#' appropriate for use with SCAQMD sensors go back to January, 2018.
#'
#' @return A data frame with columns \code{"date", "wd", "ws"}.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' wind_loadMonth(monitorID = "060950004_01", datestamp = "201805")
#' }
#' 

wind_loadMonth <- 
  function(
    monitorID = NULL,
    datestamp = NULL,
    timezone = "America/Los_Angeles",
    baseUrl = "http://smoke.mazamascience.com/data/PurpleAir/wind"
  ) {
    
    # Validate parameters ------------------------------------------------------
    
    if ( is.null(monitorID) )
      stop("Required parameter 'monitorID' is missing.")
    
    # Default to the current month
    if ( is.null(datestamp) || datestamp == "" ) {
      
      now <- lubridate::now(timezone)
      datestamp <- strftime(now, "%Y%m%d")
      
    }
    
    # Handle the case where the day is already specified
    datestamp <- stringr::str_sub(paste0(datestamp,"01"), 1, 8)
    monthstamp <- stringr::str_sub(datestamp, 1, 6)
    yearstamp <- stringr::str_sub(datestamp, 1, 4)
    
    # ----- Load wind data from URL --------------------------------------------
    
    getWind <- 
      function(monitorID, parameter) {
        
        filename <- paste0("airsensor_", parameter, "_", monthstamp, ".rda")
        filepath <- paste0(baseUrl, '/', yearstamp, '/', filename)
        
        # Define a 'connection' object so we can close it no matter what happens
        conn <- url(filepath)
        
        result <- 
          try(
            suppressWarnings(windObjs <- get(load(conn))), 
            silent=TRUE 
          )
        
        # Close the 'connection'
        close(conn)
        
        # NOTE:  We used suppressWarnings() above so that we can have a more
        # NOTE:  uniform error response for the large variety of reasons that
        # NOTE:  loading might fail.
        
        if ( "try-error" %in% class(result) ) {
          
          # Log the error if logging is enabled. Fail silently otherwise.
          try({ logger.error("%s", geterrmessage()) }, silent = TRUE)
          stop(paste0("Data file could not be loaded: ", filepath), call.=FALSE)
          
        }
        
        # Check if provided monitorID exists in data
        if ( !(monitorID %in% names(windObjs$data)) ) 
          stop("Invalid 'monitorID' provided.")
        
        # Isolate the monitorID provided 
        data <- PWFSLSmoke::monitor_isolate(windObjs)[[monitorID]]
        
        return(data)
        
      }
    
    # Get wind speed and directions
    windSpeed <- getWind(monitorID, "WS")
    windDirection <- getWind(monitorID, "WD")
    
    # Create wind data frame
    # NOTE: This is of type 'data.frame', NOT 'ws_monitor'
    windData <- 
      dplyr::tibble(
        "date" = windDirection$data[[1]], 
        "wd" = windDirection$data[[2]],
        "ws" = windSpeed$data[[2]]
      )
    
    return(windData)
    
  }

