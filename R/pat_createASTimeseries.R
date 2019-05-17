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
  
  data <- 
    pat_aggregate(
      pat,
      period, 
      quickStats = TRUE, 
    )
  
  # TODO: Determie what should be in meta
  meta <- 
    pat$meta %>% 
    dplyr::select(
      .data$ID, 
      .data$label,
      .data$longitude, 
      .data$latitude, 
      .data$countryCode, 
      .data$stateCode 
    )
  
  as_object <- list(meta = meta, data = data)
  class(as_object) <- c("as_timeseries", class(pat))
  
  return(as_object)
  
}
