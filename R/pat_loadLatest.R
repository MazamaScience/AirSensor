#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.debug logger.error
#' 
#' @title Get Purple Air timeseries data
#' 
#' @param pas Purple Air 'enhanced' synoptic data
#' @param name Purple Air 'label'
#' @param id Purple Air 'ID'
#' @param startdate desired start datetime (ISO 8601)
#' @param enddate desired end datetime (ISO 8601)
#' @param baseURL Base URL for Thingspeak API
#' @return List with \code{meta} and \code{data} elements
#' @description Retrieve and parse timeseries data from the Thingspeak API for 
#' specific PurpleAir sensors.
#' 
#' @note Dates are interpreted to be in the local timzone for the sensor of 
#' interest.
#'
#' @seealso \link{downloadParseTimeseriesData}
#' @examples
#' \dontrun{
#' initializeMazamaSpatialUtils()
#' pas <- pas_load()
#' pat <- pat_loadLatest(pas, "Seattle", startdate = 20180701, enddate = 20180901)
#' }

pat_loadLatest <- function(
  pas = NULL,
  name = NULL,
  id = NULL,
  startdate = NULL,
  enddate = NULL,
  baseURL = "https://api.thingspeak.com/channels/"
) {
  
  logger.debug("----- pat_loadLatest() -----")
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( is.null(pas) )
    stop("Required parameter 'pas' is missing.")
  
  if ( is.null(name) )
    stop("Required parameter 'name' is missing.")
  
  if ( !name %in% pas$label )
    stop(paste0("'", name, "' is not found in the 'pas' object"))
  
  # ----- Determine date sequence ----------------------------------------------
  
  # Only one week of data can be loaded at a time. If over one week has been
  # requested, loop over weeks to download all of it
  
  timezone <- 
    pas %>% 
    filter(.data$label == name) %>% 
    pull(.data$timezone)
  
  # Guarantee a valid date range
  dateRange <- .dateRange(startdate, enddate, timezone = timezone, unit = "sec")
  
  # Create a sequence of weekly POSIXct times
  dateSeq <- seq(dateRange[1], dateRange[2], by = lubridate::ddays(7))
  
  # Tack on the final data if needed
  if ( dateRange[2] > utils::tail(dateSeq, 1) ) {
    dateSeq <- c(dateSeq, dateRange[2])
  }
  
  # ----- Load data from URL ---------------------------------------------------
  
  pat_raw <- downloadParseTimeseriesData(pas,
                                         name,
                                         id,
                                         dateSeq[1],
                                         dateSeq[2],
                                         baseURL)
  
  if ( length(dateSeq) > 2 ) {
    
    for ( i in 2:(length(dateSeq) - 1) ) {
      new_pat_raw <- downloadParseTimeseriesData(pas, 
                                                 name,
                                                 id,
                                                 dateSeq[i],
                                                 dateSeq[i+1],
                                                 baseURL)
      pat_raw$data <- dplyr::bind_rows(pat_raw$data, new_pat_raw$data)
    }
    
  }
  
  pat <- createPATimeseriesObject(pat_raw)
  
  return(pat)
  
}
