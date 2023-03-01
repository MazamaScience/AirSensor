#' @export
#' @importFrom rlang .data
#' 
#' @title Load PurpleAir time series data for a time period
#' 
#' @description A pre-generated PurpleAir Timeseries \emph{pat} object will be 
#' loaded for the given time interval if available. Data are 
#' loaded from the archive set with either \code{setArchiveBaseUrl()} or 
#' \code{setArchiveBaseDir()} for locally archived files.
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
#' When no dates are specified, \code{pat_loadLatest()} is used, loading data
#' for the last 7 days.
#' 
#' @note Archive file names are 
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
#' 
#' @param id PurpleAir sensor 'deviceDeploymentID'.
#' @param label PurpleAir sensor 'label'.
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param startdate Desired start time (ISO 8601) or \code{POSIXct}.
#' @param enddate Desired end time (ISO 8601) or \code{POSIXct}.
#' @param timezone Timezone used to interpret start and end dates.
#' 
#' @return A PurpleAir Timeseries \emph{pat} object.
#' 
#' @seealso \link{pat_loadLatest}
#' @seealso \link{pat_loadMonth}
#' @seealso \link{pat_createNew}
#' 
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor)
#' 
#' setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
#' 
#' # Reference an older 'pas' before this sensor was dropped
#' pas <- pas_load(20190901, archival = TRUE)
#' 
#' pat <- pat_load(
#'   label = "SCNP_20", 
#'   pas = pas,
#'   startdate = 20190411, 
#'   enddate = 20190521
#' )
#' 
#' pat_multiPlot(pat)
#' 
#' }, silent = FALSE)
#' }

pat_load <- function(
  id = NULL,
  label = NULL,
  pas = NULL,
  startdate = NULL, 
  enddate = NULL, 
  timezone = "America/Los_Angeles"
) {
  
  # TODO:  This always trims to day-boundaries. At some point, we should allow
  # TODO:  specification of sub-day times.
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(timezone)
  
  # Get the deviceDeploymentID
  if ( is.null(id) && is.null(label) ) {
    
    stop(paste0("label or id must be provided"))
    
  } else if ( is.null(id) && !is.null(label) ) {
    
    if ( is.null(pas) )
      stop(paste0("pas must be provided when loading by label"))
    
    if ( !label %in% pas$label )
      stop(sprintf("label '%s' is not found in the 'pas' object", label))
    
    # Get the deviceDeploymentID from the label
    deviceDeploymentID <- pas_getDeviceDeploymentIDs(pas, pattern = label)
    
    if ( length(deviceDeploymentID) > 1 )
      stop(sprintf("label '%s' matches more than one sensor", label))
    
  } else {
    
    deviceDeploymentID <- id
    
  }
  
  # Quick return if no dates provided
  if ( is.null(startdate) && is.null(enddate) ) 
    return( pat_loadLatest(deviceDeploymentID) )
  
  # Get the date range
  dateRange <- MazamaCoreUtils::timeRange(startdate, 
                                          enddate, 
                                          timezone = timezone)
  
  # ----- Assemble monthly archive files ---------------------------------------
  
  # NOTE:  datestamps here are created with the local timezone. It is the job of
  # NOTE:  pat_loadMonth() to convert these into UTC for use in constructing
  # NOTE:  data file URLs.
  
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
    
    # Ignore "no data file" errors (sometimes entire months are missing)
    result <- try({
      
      patList[[datestamp]] <- 
        pat_loadMonth(
          id = deviceDeploymentID,
          datestamp = datestamp, 
          timezone = timezone
        )
      
    }, silent = TRUE)
    
    if ( "try-error" %in% class(result) ) {
      err_msg <- geterrmessage()
      if ( stringr::str_detect(err_msg, "file could not be loaded") ) {
        # Ignore
      } else {
        stop(err_msg)
      }
    }
    
  } 
  
  # ----- Combine and clean pat objects ----------------------------------------
  
  pat <- pat_join(patList)
  
  # Guarantee that times are arranged properly
  pat$data <- 
    pat$data %>%
    dplyr::arrange(.data$datetime)
  
  # Guarantee we have no duplicates and only the requested time range
  patObj <- 
    pat %>% 
    pat_distinct() %>%
    pat_filterDatetime(
      startdate = dateRange[1], 
      enddate = dateRange[2],
      timezone = timezone
    )
  
  # ----- Return ---------------------------------------------------------------
  
  return(patObj)
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  
  library(AirSensor)
  setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
  
  pas <- pas_load()
  id <- NULL
  label <- "SCNP_20"
  startdate <- 20200916
  enddate <- 20200923
  timezone <- "America/Los_Angeles"
  
  
  
  pat <- pat_load(
    pas = pas,
    label = label,
    startdate = startdate,
    enddate = enddate
  )

}