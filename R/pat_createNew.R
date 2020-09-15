#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.debug
#' 
#' @title Load latest PurpleAir time series data
#' 
#' @param id PurpleAir sensor 'deviceDeploymentID'.
#' @param label PurpleAir sensor 'label'.
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param startdate Desired UTC start time (ISO 8601) or \code{POSIXct}.
#' @param enddate Desired UTC end time (ISO 8601) or \code{POSIXct}.
#' @param timezone Timezone used to interpret start and end dates.
#' @param baseUrl Base URL for Thingspeak API.
#' @param verbose Logical controlling the generation of warning and error messages.
#' 
#' @return A PurpleAir Timeseries \emph{pat} object.
#' 
#' @description Retrieve and parse timeseries data from the Thingspeak API for 
#' specific PurpleAir sensors.
#' 
#' Dates can be anything that is understood by 
#' \code{MazamaCoreUtils::parseDatetime()} including any of the following 
#' recommended formats:
#' 
#' \itemize{
#' \item{\code{"YYYYmmdd"}}
#' \item{\code{"YYYY-mm-dd"}}
#' \item{\code{"YYYY-mm-dd HH:MM:SS"}}
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
#' @seealso \link{pat_downloadParseRawData}
#' 
#' @examples
#' \donttest{
#' library(AirSensor)
#' 
#' pat <- pat_createNew(
#'   label = "Seattle", 
#'   pas = example_pas, 
#'   startdate = 20180701, 
#'   enddate = 20180901
#' )
#' pat_multiPlot(pat)
#' }

pat_createNew <- function(
  id = NULL,
  label = NULL,
  pas = NULL,
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  baseUrl = "https://api.thingspeak.com/channels/",
  verbose = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(baseUrl)
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
  
  # ----- Determine date sequence ----------------------------------------------
  
  # NOTE:  In 2019, ThingSpeak had a download maximum of 8000 records so we limit
  # NOTE:  time ranges to one week which keeps us under this limit.
  
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
    dateRange <- MazamaCoreUtils::timeRange(
      starttime = startdate, 
      endtime = enddate, 
      timezone = timezone, 
      unit = "min",
      ceilingStart = FALSE,
      ceilingEnd = FALSE
    )
  } else {
    # Default to 7 days with day boundaries
    dateRange <- MazamaCoreUtils::dateRange(
      startdate = startdate, 
      enddate = enddate, 
      timezone = timezone, 
      unit = "min",
      ceilingStart = FALSE,
      ceilingEnd = FALSE,
      days = 7
    )
  }
  
  # Create a sequence of weekly POSIXct times
  dateSeq <- seq(dateRange[1], dateRange[2], by = lubridate::ddays(7))
  
  # Tack on the final data if needed
  if ( dateRange[2] > utils::tail(dateSeq, 1) ) {
    dateSeq <- c(dateSeq, dateRange[2])
  }
  
  # ----- Load data from URL ---------------------------------------------------
  
  if ( verbose ) {
    message(sprintf("Requesting data for %s from %s to %s", 
                    id, dateSeq[1], dateSeq[2]))
  }
  
  # Use more specific ID rather than the label
  pat_rawList <- pat_downloadParseRawData(
    id = pas_single$deviceDeploymentID,
    label = NULL,
    pas = pas,
    startdate = dateSeq[1],
    enddate = dateSeq[2],
    timezone = timezone,
    baseUrl = baseUrl
  )
  
  if ( length(dateSeq) > 2 ) {
    
    for ( i in 2:(length(dateSeq) - 1) ) {
      
      if ( verbose ) {
        message(sprintf("Requesting data for %s from %s to %s", 
                        id, dateSeq[i], dateSeq[i+1]))
      }
      
      new_pat_rawList <- pat_downloadParseRawData(
        id = pas_single$deviceDeploymentID,
        label = NULL,
        pas = pas,
        startdate = dateSeq[i],
        enddate = dateSeq[i + 1],
        timezone = timezone,
        baseUrl = baseUrl
      )
      
      pat_rawList$A_PRIMARY <- 
        dplyr::bind_rows(pat_rawList$A_PRIMARY, new_pat_rawList$A_PRIMARY) %>%
        dplyr::distinct()
      pat_rawList$A_SECONDARY <- 
        dplyr::bind_rows(pat_rawList$A_SECONDARY, new_pat_rawList$A_SECONDARY) %>%
        dplyr::distinct()
      pat_rawList$B_PRIMARY <- 
        dplyr::bind_rows(pat_rawList$B_PRIMARY, new_pat_rawList$B_PRIMARY) %>%
        dplyr::distinct()
      pat_rawList$B_SECONDARY <- 
        dplyr::bind_rows(pat_rawList$B_SECONDARY, new_pat_rawList$B_SECONDARY) %>%
        dplyr::distinct()
      
    }
    
  }

  # ----- Merge and harmonize --------------------------------------------------
  
  if ( verbose ) {
    message(sprintf("Download completed, merging/harmonizing data ..."))
  }
  pat <- pat_createPATimeseriesObject(pat_rawList)
  
  # Guarantee we have no duplicates and only the requested time range
  pat <- 
    pat %>% 
    pat_distinct() %>%
    pat_filterDatetime(
      startdate = dateRange[1],
      enddate = dateRange[2],
      timezone = timezone
    )
  
  # ----- Return ---------------------------------------------------------------
  
  return(pat)
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  library(AirSensor)
  
  setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1") # SCAQMD sensors
  
  pas <- pas_load()

  id <- "0bf2ba90b55e7ce6_2025" # 
  label <- NULL
  startdate <- 20170930
  enddate <- 20171102
  timezone <- NULL
  baseUrl <- "https://api.thingspeak.com/channels/"
  verbose <- TRUE
  
  
  pat <- pat_createNew(
    id,
    label,
    pas,
    startdate,
    enddate,
    timezone,
    baseUrl,
    verbose
  )

  
    
  # Default settings
  id = NULL
  label = NULL
  pas = example_pas
  startdate = NULL
  enddate = NULL
  timezone = NULL
  baseUrl = "https://api.thingspeak.com/channels/"
  verbose = FALSE
  
  # Documentation example
  label = "Seattle"
  pas = example_pas
  startdate = 20180701
  enddate = 20180901
  
  
}
