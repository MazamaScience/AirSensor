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
#' \code{lubridate::parse_date_time()} including either of the following 
#' recommended formats:
#' 
#' \itemize{
#' \item{\code{"YYYYmmdd"}}
#' \item{\code{"YYYY-mm-dd"}}
#' }
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
#' 
#' @param id PurpleAir sensor 'deviceDeploymentID'.
#' @param label PurpleAir sensor 'label'.
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param startdate Desired start time (ISO 8601).
#' @param enddate Desired end time (ISO 8601).
#' @param days Number of days of data to include (7 or 45).
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
#' setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
#' pat <- pat_load("SCNP_20", 20190411, 20190521)
#' pat_multiPlot(pat)
#' }

pat_load <- function(
  id = NULL,
  label = NULL,
  pas = NULL,
  startdate = NULL, 
  enddate = NULL, 
  days = 7, 
  timezone = "America/Los_Angeles"
) {
  
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
  dateRange <- MazamaCoreUtils::dateRange(startdate, 
                                          enddate, 
                                          timezone, 
                                          days = days)
  
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
  
  # ----- Combine and pat objects ----------------------------------------------
  
  pat <- pat_join(patList)
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  # Trim to requested dateRange
  patObj <- 
    pat_filterDate(
      pat,
      startdate = dateRange[1], 
      enddate = dateRange[2],
      timezone = timezone
    )
  
  # ----- Return ---------------------------------------------------------------
  
  return(patObj)
  
}

