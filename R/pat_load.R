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
#' \code{lubrdiate::parse_date_time()} including either of the following 
#' recommended formats:
#' 
#' \itemize{
#' \item{\code{"YYYYmmdd"}}
#' \item{\code{"YYYY-mm-dd"}}
#' }
#' 
#' @note Starting with \pkg{AirSensor} version 0.6, archive file names are 
#' generated with a "device-deployment" identifier by combining a unique 
#' location ID with a unique device ID. These "device-deployment" identifiers 
#' guarantee that movement of a sensor will result in the creation of a new
#' time series.
#' 
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param label PurpleAir sensor 'label'.
#' @param id PurpleAir sensor 'ID'.
#' @param startdate Desired start time (ISO 8601).
#' @param enddate Desired end time (ISO 8601).
#' @param days Number of days of data to include.
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
#' # TODO:  This needs to be updated to use USFS data
#' setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
#' pat <- pat_load("SCNP_20", 20190411, 20190521)
#' pat_multiplot(pat)
#' }

pat_load <- function(
  pas = NULL,
  label = NULL,
  id = NULL,
  startdate = NULL, 
  enddate = NULL, 
  days = 7, 
  timezone = "America/Los_Angeles"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pas)
  
  if ( !pas_isPas(pas) )
    stop("Required parameter 'pas' is not a valid 'pa_synoptic' object.")
  
  if ( pas_isEmpty(pas) )
    stop("Required parameter 'pas' has no data.") 
  
  # Get the sensorID
  if ( is.null(id) && is.null(label) ) {
    
    stop(paste0("label or id must be provided"))
    
  } else if ( is.null(id) && !is.null(label) ) {
    
    if ( ! label %in% pas$label )
      stop(sprintf("label '%s' is not found in the 'pas' object", label))
    
    # Get the sensorID from the label
    sensorID <- pas_getIDs(pas, pattern = label)
    
    if ( length(sensorID) > 1 )
      stop(sprintf("label '%s' matches more than one sensor", label))
    
  } else {
    
    if ( ! id %in% pas$ID )
      stop(sprintf("id '%s' is not found in the 'pas' object", id))
    
    sensorID <- id
    
  }
  
  dateRange <- MazamaCoreUtils::dateRange(startdate, 
                                          enddate, 
                                          timezone, 
                                          days = days)
  
  # Quick return if no dates provided
  if ( is.null(startdate) && is.null(enddate) ) 
    return( pat_loadLatest(pas, label, sensorID) )
  
  # ----- Asssemble monthly archive files --------------------------------------
  
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
          pas,
          label = NULL, 
          id = sensorID,
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

