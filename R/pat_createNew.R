#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.debug logger.error
#' 
#' @title Load latest PurpleAir time series data
#' 
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param label PurpleAir sensor 'label'.
#' @param id PurpleAir sensor 'ID'.
#' @param startdate Desired UTC start time (ISO 8601).
#' @param enddate Desired UTC end time (ISO 8601).
#' @param baseURL Base URL for Thingspeak API.
#' 
#' @return A PurpleAir Timeseries \emph{pat} object.
#' 
#' @description Retrieve and parse timeseries data from the Thingspeak API for 
#' specific PurpleAir sensors.
#' 
#' @note Dates are interpreted to be in the local timzone for the sensor of 
#' interest.
#'
#' @seealso \link{downloadParseTimeseriesData}
#' 
#' @examples
#' \donttest{
#' pas <- pas_load()
#' pat <- pat_createNew(pas, "Seattle", startdate = 20180701, enddate = 20180901)
#' pat_multiplot(pat)
#' }

pat_createNew <- function(
  pas = NULL,
  label = NULL,
  id = NULL,
  startdate = NULL,
  enddate = NULL,
  baseURL = "https://api.thingspeak.com/channels/"
) {
  
  logger.debug("----- pat_createNew() -----")
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( is.null(pas) )
    stop("Required parameter 'pas' is missing.")
  
  if ( is.null(label) )
    stop("Required parameter 'label' is missing.")
  
  if ( !label %in% pas$label )
    stop(paste0("'", label, "' is not found in the 'pas' object"))
  
  # ----- Determine date sequence ----------------------------------------------
  
  # Only one week of data can be loaded at a time. If over one week has been
  # requested, loop over weeks to download all of it
  
  # TODO:  Read "programming with dplyr" to understand this better
  timezone <-
    pas %>%
    dplyr::filter(.data$label == !!label) %>%
    dplyr::pull(.data$timezone)

  # Default to a week if startdate or enddate is missing
  days <- 7 
  
  # Guarantee a valid date range
  dateRange <- MazamaCoreUtils::dateRange(startdate, enddate, timezone, days)
  
  # Create a sequence of weekly POSIXct times
  dateSeq <- seq(dateRange[1], dateRange[2], by = lubridate::ddays(7))
  
  # Tack on the final data if needed
  if ( dateRange[2] > utils::tail(dateSeq, 1) ) {
    dateSeq <- c(dateSeq, dateRange[2])
  }
  
  # ----- Load data from URL ---------------------------------------------------
  
  pat_raw <- downloadParseTimeseriesData(pas,
                                         label,
                                         id,
                                         dateSeq[1],
                                         dateSeq[2],
                                         baseURL)
  
  if ( length(dateSeq) > 2 ) {
    
    for ( i in 2:(length(dateSeq) - 1) ) {
      new_pat_raw <- downloadParseTimeseriesData(pas, 
                                                 label,
                                                 id,
                                                 dateSeq[i],
                                                 dateSeq[i+1],
                                                 baseURL)
      pat_raw$data <- dplyr::bind_rows(pat_raw$data, new_pat_raw$data)
    }
    
  }
  
  # ----- Remove bad records ---------------------------------------------------
  
  # Sometimes we get records with all bad values:
  
  # datetime pm25_A pm25_B temperature humidity uptime adc0 rssi          datetime_A          datetime_B
  # 1  2000-01-01 12:00:00     NA     NA          NA       NA     NA   NA   NA 2000-01-01 12:00:00 2000-01-01 12:00:00
  # 2  2000-01-01 12:00:00     NA     NA          NA       NA     NA   NA   NA 2000-01-01 12:00:00 2000-01-01 12:00:00
  
  # We remove them by limiting the data to the requested local time range
  
  data <- 
    pat_raw$data %>%
    dplyr::filter(.data$datetime >= dateRange[1]) %>%
    dplyr::filter(.data$datetime <= dateRange[2])
  
  pat_raw$data <- data
  
  # ----- Return ---------------------------------------------------------------
  
  pat <- createPATimeseriesObject(pat_raw)
  
  return(pat)
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  pas <- pas_load()
  label <- "SCAP_14"
  id <- NULL
  startdate <- 20190701
  enddate <- 20190708
  baseURL <- "https://api.thingspeak.com/channels/"
  
}
