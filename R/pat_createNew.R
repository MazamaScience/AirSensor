#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.debug
#' 
#' @title Load latest PurpleAir time series data
#' 
#' @param id PurpleAir sensor 'deviceDeploymentID'.
#' @param label PurpleAir sensor 'label'.
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param startdate Desired UTC start time (ISO 8601).
#' @param enddate Desired UTC end time (ISO 8601).
#' @param timezone Timezone used to interpret start and end dates.
#' @param baseURL Base URL for Thingspeak API.
#' 
#' @return A PurpleAir Timeseries \emph{pat} object.
#' 
#' @description Retrieve and parse timeseries data from the Thingspeak API for 
#' specific PurpleAir sensors.
#' 
#' Dates can be anything that is understood by 
#' \code{lubridate::parse_date_time()} including either of the following 
#' recommended formats:
#' 
#' \itemize{
#' \item{\code{"YYYYmmdd"}}
#' \item{\code{"YYYY-mm-dd"}}
#' }
#' 
#' @note When \code{timezone = NULL}, the default, dates are interpreted to be 
#' in the local timezone for the sensor of interest.
#'
#' @note Starting with \pkg{AirSensor} version 0.6, archive file names are 
#' generated with a unique "device-deployment" identifier by combining a unique 
#' location ID with a unique device ID. These "device-deployment" identifiers 
#' guarantee that movement of a sensor will result in the creation of a new
#' time series.
#' 
#' Users may request a \emph{pat} object in one of two ways:
#' 
#' 1) Pass in \code{id} with a valid a \code{deviceDeploymentID}
#' 
#' 2) Pass in both \code{label} and \code{pas} so that the 
#' \code{deviceDeploymentID} can be looked up.
#' @seealso \link{pat_downloadParseData}
#' 
#' @examples
#' \donttest{
#' pat <- pat_createNew(
#'   label = "Seattle", 
#'   pas = example_pas, 
#'   startdate = 20180701, 
#'   enddate = 20180901
#' )
#' pat_multiplot(pat)
#' }

pat_createNew <- function(
  id = NULL,
  label = NULL,
  pas = NULL,
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  baseURL = "https://api.thingspeak.com/channels/"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(baseURL)
  MazamaCoreUtils::stopIfNull(pas)
  
  # Get the deviceDeploymentID
  if ( is.null(id) && is.null(label) ) {
    
    stop(paste0("label or id must be provided"))
    
  } else if ( is.null(id) && !is.null(label) ) {
    
    if ( is.null(pas) )
      stop(paste0("pas must be provided when loading by label"))
    
    if ( !label %in% pas$label )
      stop(sprintf("label '%s' is not found in the 'pas' object", label))
    
    # Get the deviceDeploymentID from the label
    pattern <- paste0("^", label, "$")
    deviceDeploymentID <- pas_getDeviceDeploymentIDs(pas, pattern = pattern)
    
    if ( length(deviceDeploymentID) > 1 )
      stop(sprintf("label '%s' matches more than one sensor", label))
    
  } else {
    
    # Use id whenever it is defined, potentially ignoring label
    deviceDeploymentID <- id
    
  }
  
  # Check if spatial utils has been initialized
  if ( !spatialIsInitialized() ) {
    stop('Required MazamaSpatialUtils has not been initialized. 
    Please see `?initializeMazamaSpatialUtils` for details.')
  }
  
  # ----- Determine date sequence ----------------------------------------------
  
  # Find a single, parent record
  pas_single <-
    pas %>%
    dplyr::filter(is.na(.data$parentID)) %>%
    dplyr::filter(.data$deviceDeploymentID == !!deviceDeploymentID)
  
  if ( nrow(pas_single) > 1 ) {
    stop(paste0("Multiple sensors share deviceDeploymentID: ",
                deviceDeploymentID, "'"))
  } 

  # Get the timezone associated with this sensor
  if ( is.null(timezone) ) {
    timezone <-
      pas_single %>%
      dplyr::pull(.data$timezone)
  }
  
  # Create a valid dateRange
  if ( !is.null(startdate) && !is.null(enddate) ) {
    # Don't require day boundaries
    dateRange <- MazamaCoreUtils::timeRange(startdate, 
                                            enddate, 
                                            timezone = timezone)
  } else {
    # Default to 7 days with day boundaries
    dateRange <- MazamaCoreUtils::dateRange(startdate, 
                                            enddate, 
                                            timezone, 
                                            days = 7)
  }
  
  # Create a sequence of weekly POSIXct times
  dateSeq <- seq(dateRange[1], dateRange[2], by = lubridate::ddays(7))
  
  # Tack on the final data if needed
  if ( dateRange[2] > utils::tail(dateSeq, 1) ) {
    dateSeq <- c(dateSeq, dateRange[2])
  }
  
  # ----- Load data from URL ---------------------------------------------------
  
  # Use more specific ID rather than the label
  pat_raw <- pat_downloadParseData(
    id = pas_single$deviceDeploymentID,
    label = NULL,
    pas = pas,
    startdate = dateSeq[1],
    enddate = dateSeq[2],
    timezone = timezone,
    baseURL = baseURL
  )
  
  if ( length(dateSeq) > 2 ) {
    
    for ( i in 2:(length(dateSeq) - 1) ) {
      new_pat_raw <- pat_downloadParseData(
        id = pas_single$deviceDeploymentID,
        label = NULL,
        pas = pas,
        startdate = dateSeq[i],
        enddate = dateSeq[i + 1],
        timezone = timezone,
        baseURL = baseURL
      )
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
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  return(pat)
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  #id <- "27f2d8382be52aff_41449"
  id <- NULL
  label <- "Chisholm ACT Australia"
  #label <- NULL
  pas <- pas_au
  startdate <- NULL
  enddate <- NULL
  timezone <- NULL
  baseURL <- "https://api.thingspeak.com/channels/"
  
}
