#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.debug
#' 
#' @title Load Purple Air Timeseries data
#' 
#' @description A pre-generated \code{pa_timeseries} object will be loaded for
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
#' @param label Purple Air 'label'
#' @param startdate desired start datetime (ISO 8601)
#' @param enddate desired end datetime (ISO 8601)
#' @param days number of days of data to include
#' @param timezone Timezone used to interpret datestamp.
#' @param baseUrl Base URL for synoptic data.
#' 
#' @return An object of class "pa_timeseries".
#' 
#' @seealso \link{pat_loadMonth}
#' @seealso \link{.dateRange}
#' 
#' @examples
#' \dontrun{
#' pat <- pat_load("SCNP_20", 20190411, 20190521)
#' pat_multiplot(pat)
#' }

pat_load <- 
  function(
    label = NULL, 
    startdate = NULL, 
    enddate = NULL, 
    days = 7, 
    timezone = "America/Los_Angeles",
    baseUrl = "http://smoke.mazamascience.com/data/PurpleAir/pat"
    ) {
     
    logger.debug("----- pat_load() -----")
    
    # Validate parameters ------------------------------------------------------
    
    if ( is.null(label) ) 
      stop("Required parameter 'label' is missing.")
    
    dateRange <- .dateRange(startdate, enddate, days, timezone)
    
    # Asssemble monthly archive files ------------------------------------------
    
    datestamps <-
      seq(
        dateRange[1],
        dateRange[2],
        by = "months"
      ) %>%
      strftime(format = "%Y%m")
    
    patList <- list()
    
    for ( datestamp in datestamps ) { 
      
      patList[[datestamp]] <- 
        pat_loadMonth(
          label = label, 
          datestamp = datestamp, 
          timezone = timezone,
          baseUrl = baseUrl
        )
      
    } 
    
    # Return -------------------------------------------------------------------
    
    patObj <- 
      patList %>% 
      pat_join() %>% 
      pat_filterDate(dateRange[1], dateRange[2])
   
    return(patObj)
    
  }

