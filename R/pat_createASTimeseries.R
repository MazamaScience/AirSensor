#' @export
#' @importFrom rlang .data
#' 
#' @title Create a AirSensor Timeseries object
#' 
#' @param pat Purple Air timeseries data from \code{createPATimeseriesObject()}
#' @param period The time period to average to. Can be "sec", "min", "hour", 
#' "day", "DSTday", "week", "month", or "year". A number can also
#' precede these options followed by a space (i.e. "2 day" or "37 min").
#' @param ... Extra parameters to fine tune \code{pat_aggregate()} function
#' 
#' @return List with original \code{meta} and restructured \code{data} elements
#' 
#' @description Aggregates a PurpleAir timeseries object along the datetime
#' axis to produce a new dataframe with 13 columns of period-aggregated
#' timeseries data: averages, standard deviations, measurement frequency, and 
#' qualitity control. 
#' 
#' @return "as_timeseries" list of aggregated time series PurpleAir data
#' 
#' @seealso \link{pat_aggregate}
#' 
#' @examples 
#' \dontrun{
#' pat <- 
#'   AirSensor::example_pat %>%
#'   pat_filterDate(20180701, 20180901)
#' ast <- pat_createASTimeseries(pat, "1 hour")
#' }

pat_createASTimeseries <- function(
  pat, 
  period = "1 hour"
) { 
  
  logger.debug("----- pat_createASTimeseries() -----")

  # ----- Validate Parameters --------------------------------------------------
  
  tolower(period) -> period
  # tolower(stats) -> stats
  # Avoid opaque error message from openair when a user types "minute(s)"
  period <- stringr::str_replace(period, "ute?", "")
  
  if ( !pat_isPat(pat) )
    stop("parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("parameter 'pat' has no data.") 

  # ----- Create as_timeseries object  -----------------------------------------
  
  # Use pat_aggregate() to convert to a regular time axis
  data <- 
    pat_aggregate(
      pat,
      period, 
      quickStats = TRUE, 
    )
  
  # Here is the original PAT metadata:
  # > str(pat$meta)
  # Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	1 obs. of  13 variables:
  #  $ ID                            : chr "15907"
  #  $ label                         : chr "SCNP_05"
  #  $ sensorType                    : chr "PMS5003+PMS5003+BME280"
  #  $ DEVICE_LOCATIONTYPE           : chr "outside"
  #  $ THINGSPEAK_PRIMARY_ID         : chr "580075"
  #  $ THINGSPEAK_PRIMARY_ID_READ_KEY: chr "XICPPJFS2R49VYAL"
  #  $ longitude                     : num -121
  #  $ latitude                      : num 35
  #  $ countryCode                   : chr "US"
  #  $ stateCode                     : chr "CA"
  #  $ timezone                      : chr "America/Los_Angeles"
  #  $ pwfsl_closestDistance         : num 2293
  #  $ pwfsl_closestMonitorID        : chr "060792004_01"
  
  # Copy almost everything
  meta <- 
    pat$meta %>%
    dplyr::select(-.data$THINGSPEAK_PRIMARY_ID,
                  -.data$THINGSPEAK_PRIMARY_ID_READ_KEY) %>%
    dplyr::rename(locationType = .data$DEVICE_LOCATIONTYPE)
  
  as_object <- list(meta = meta, data = data)
  class(as_object) <- c("as_timeseries", "list")
  
  return(as_object)
  
}
