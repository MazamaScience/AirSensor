#' @export
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' 
#' @title Load Purple Air Timeseries data
#' 
#' @description A pre-generated \code{pa_timeseries} object will be loaded for
#' the given time interval. Archived data for SCAQMD sensors go back to 
#' January, 2018.
#' 
#' The \code{startdate} and \code{enddate} can be in the following format:
#' 
#' \itemize{
#' \item{\code{"YYYYmmdd"}}
#' }
#' 
#' By default, the current month is loaded.
#'
#' @param label Purple Air 'label'
#' @param startdate A starting date in ymd order.
#' @param enddate An ending date in ymd order
#' @param timezone Timezone used to interpret datestamp.
#' @param baseUrl Base URL for synoptic data.
#' 
#' @return An object of class "pa_timeseries".
#' 
#' @seealso \link{pat_loadMonth}
#' 
#' @examples
#' \dontrun{
#' pat_load("SCNP_20", 20190411, 20190521)
#'   pat_multiplot()
#' }

pat_load <- 
  function(
    label = NULL, 
    startdate = NULL, 
    enddate = NULL, 
    timezone = "America/Los_Angeles",
    baseUrl = "http://smoke.mazamascience.com/data/PurpleAir/pat"
    ) {
     
    logger.debug("----- pat_load() -----")
    
    if ( is.null(label) ) 
      stop("Required parameter 'label' is missing.")
    
    if (is.null(startdate) || is.null(enddate) ) 
      stop("Required start and/or end date is missing")
    
    ymdBounds <- 
      c(startdate, enddate) %>% 
      lubridate::ymd(tz = timezone)
    
    ymInt <- 
      seq(
        ymdBounds[1], 
        ymdBounds[2], 
        by = "months"
      ) %>% 
      strftime(format = "%Y%m")
    
    patList <- list()
    
    for ( i in ymInt ) { 
      
      patList[[i]] <- 
        pat_loadMonth(
          label = label, 
          datestamp = i, 
          timezone = timezone,
          baseUrl = baseUrl
        )
      
    } 
    
    patObj <- 
      patList %>% 
      pat_join() %>% 
      pat_filterDate(ymdBounds[1], ymdBounds[2])
   
    return(patObj)
    
  }

