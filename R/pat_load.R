#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.debug
#' 
#' @title Load PurpleAir time series data
#' 
#' @description A pre-generated PurpleAir Timeseries \emph{pat} object will be loaded for
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
#' @param label PurpleAir sensor 'label'.
#' @param startdate Desired start time (ISO 8601).
#' @param enddate Desired end time (ISO 8601).
#' @param days Number of days of data to include.
#' @param timezone Timezone used to interpret \code{datestamp}.
#' @param baseUrl Base URL for synoptic data.
#' 
#' @return A PurpleAir Timeseries \emph{pat} object.
#' 
#' @seealso \link{pat_loadMonth}
#' @seealso \link{pat_createNew}
#' 
#' @examples
#' \donttest{
#' pat <- pat_load("SCNP_20", 20190411, 20190521)
#' pat_multiplot(pat)
#' }

pat_load <- function(
  label = NULL, 
  startdate = NULL, 
  enddate = NULL, 
  days = 7, 
  timezone = "America/Los_Angeles",
  baseUrl = "http://smoke.mazamascience.com/data/PurpleAir/pat"
) {
  
  logger.debug("----- pat_load() -----")
  
  # Validate parameters --------------------------------------------------------
  
  if ( is.null(label) ) 
    stop("Required parameter 'label' is missing.")
  
  dateRange <- MazamaCoreUtils::dateRange(startdate, enddate, days, timezone)
  
  # Asssemble monthly archive files --------------------------------------------
  
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
  
  # Return ---------------------------------------------------------------------
  
  patObj <- 
    pat_filterDate(
      pat = pat_join(patList),
      startdate = dateRange[1], 
      enddate = dateRange[2]
    )
  
  return(patObj)
  
}

