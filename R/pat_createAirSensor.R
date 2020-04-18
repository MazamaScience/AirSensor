#' @export
#' @importFrom rlang .data
#' 
#' @title Create an Air Sensor object
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param period Time period to average over. Can be "sec", "min", "hour", 
#' "day", "DSTday", "week", "month", "quarter" or "year". A number can also
#'  precede these options followed by a space (i.e. "2 day" or "37 min").
#' @param parameter Parameter for which to create an \emph{as} object -- one of
#' "pm25", "humidity" or "temperature".
#' @param channel Data channel to use for PM2.5 -- one of "a", "b or "ab".
#' @param qc_algorithm Named QC algorithm to apply to hourly aggregation stats.
#' @param min_count Aggregation bins with fewer than `min_count` measurements
#' will be marked as `NA`.
#' @param aggregation_FUN Function used to convert a \emph{pat} object into a
#' tibble of hourly statistics. 
#'  
#' @description Converts data from a \emph{pat} object with an irregular time 
#' axis to an \emph{airsensor} object where the numeric data has been aggregated 
#' along a standardized hourly time axis, as well as adding additional required 
#' metadata for compatibility with the *PWFSLSmoke* package.
#'
#' Current QC algorithms exist for \code{channel = "ab"} and include:
#' \itemize{
#' \item{\code{hourly_AB_00}}
#' \item{\code{hourly_AB_01}}
#' }
#' 
#' @note
#' The \code{aggregation_FUN}, allows users to pass in custom functions that 
#' generate new aggregation statistics. These statistics can then be utilized 
#' in a custom QC algorithm function. The algorithm function applied is
#' generated from the \code{qc_algorithm} parameter with 
#' \code{paste0("PurpleAirQC_", qc_algorithm)}.
#'
#' @return An "airsensor" object of aggregated PurpleAir Timeseries data.
#' 
#' @seealso \link{PurpleAirQC_hourly_AB_00}
#' @seealso \link{PurpleAirQC_hourly_AB_01}
#' @seealso \link{pat_aggregate}
#' 
#' @examples 
#' 
#' sensor <- 
#'   example_pat %>%
#'   pat_filterDate(20180701, 20180901) %>%
#'   pat_createAirSensor()
#' PWFSLSmoke::monitor_dailyBarplot(sensor)
#' 

pat_createAirSensor <- function(
  pat = NULL,
  period = "1 hour",
  parameter = "pm25",
  channel = "ab",
  qc_algorithm = "hourly_AB_01",
  min_count = 20,
  aggregation_FUN = pat_aggregate
) {
  
  # ----- Validate Parameters --------------------------------------------------

  period <- tolower(period)
  parameter <- tolower(parameter)
  channel <- tolower(channel)
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Required parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Required parameter 'pat' has no data.") 
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  if ( !parameter %in% c("pm25", "humidity", "temperature") )
    stop("Required parameter 'parameter' must be one of 'pm25', 'humidity' or 'temperature'")
  
  if ( !is.null(channel) ) {
    if ( !channel %in% c("a", "b", "ab") )
      stop("Required parameter 'channel' must be one of 'a', 'b' or 'ab'")
  }
  
  if ( !qc_algorithm %in% c("hourly_AB_00", "hourly_AB_01") ) {
    stop("Required parameter 'qc_algorithm' must be one of 'hourly_AB_00' or 'hourly_AB_01'")
  }
  
  if ( !rlang::is_closure(aggregation_FUN) ) {
    stop(paste0("Parameter 'aggregation_FUN' is not a function.",
                "(Pass in the function with no quotes and no parentheses.)"))
  }
  
  # ----- Raw data QC ----------------------------------------------------------
  
  # Invalidate out-of-spec values. Don't invalidate based on humidity
  pat <- pat_qc(pat, 
                removeOutOfSpec = TRUE,
                max_humidity = NULL)
  
  # ----- Temporal aggregation -------------------------------------------------
  
  # NOTE: For clarification, this function acts to route the aggregation 
  #       parameters to th respective function. Currently, this method assumes 
  #       that the function is not annonymous and accepts a pat object and period. 
  aggregationStats <- aggregation_FUN(pat,
                                      period = period)
  
  # ----- Aggregation QC -------------------------------------------------------
  
  if ( parameter == "pm25" ) {

    if ( channel == "a" ) {
      
      # Simple min_count QC
      hourlyData <-
        aggregationStats %>%
        dplyr::mutate(pm25 = .data$pm25_A_mean) %>%
        dplyr::mutate(pm25 = replace(.data$pm25, which(.data$pm25_A_count < min_count), NA) ) %>%
        dplyr::select(.data$datetime, .data$pm25)
      
    } else if ( channel == "b" ) {
      
      # Simple min_count QC
      hourlyData <-
        aggregationStats %>%
        dplyr::mutate(pm25 = .data$pm25_B_mean) %>%
        dplyr::mutate(pm25 = replace(.data$pm25, which(.data$pm25_B_count < min_count), NA) ) %>%
        dplyr::select(.data$datetime, .data$pm25)
      
    } else if ( channel == "ab" ) {
      
      # Apply qc_algorithm function
      functionName <- paste0("PurpleAirQC_", qc_algorithm)
      FUN <- get(functionName)
      hourlyData <- FUN(aggregationStats)
      
    }
    
  } else if ( parameter == "temperature" ) {
    
    hourlyData <-
      aggregationStats %>%
      # Use the period averaged mean
      dplyr::mutate(temperature = .data$temperature_mean) %>%
      # Invalidate data where there are too few measurements
      dplyr::mutate(temperature = replace(.data$temperature, which(.data$temperature < min_count), NA) ) %>%
      dplyr::select(.data$datetime, .data$temperature)
    
  } else if ( parameter == "humidity" ) {
    
    hourlyData <-
      aggregationStats %>%
      # Use the period averaged mean
      dplyr::mutate(humidity = .data$humidity_mean) %>%
      # Invalidate data where there are too few measurements
      dplyr::mutate(humidity = replace(.data$humidity, which(.data$humidity < min_count), NA) ) %>%
      dplyr::select(.data$datetime, .data$humidity)
    
  }
  
  # Cleanup any NaN or Inf that might have snuck in
  data <-
    hourlyData %>%
    dplyr::mutate_all( function(x) replace(x, which(is.nan(x)), NA) ) %>%
    dplyr::mutate_all( function(x) replace(x, which(is.infinite(x)), NA) )
  
  names(data) <- c("datetime", pat$meta$deviceDeploymentID)
  
  # ----- Create metadata  -----------------------------------------------------
  
  # Copy metadata from pat object
  meta <- 
    pat$meta %>% 
    as.data.frame()
  
  # Add metadata found in PWFSLSmoke ws_monitor objects
  meta$monitorID <- meta$deviceDeploymentID
  meta$elevation <- as.numeric(NA)
  meta$siteName <- meta$label
  meta$countyName <- as.character(NA)
  meta$msaName <- as.character(NA)
  meta$monitorType <- meta$sensorType
  meta$siteID <- meta$locationID
  meta$instrumentID <- meta$deviceID
  meta$aqsID <- as.character(NA)
  meta$pwfslID <- as.character(NA)
  meta$pwfslDataIngestSource <- "ThingSpeak"
  meta$telemetryAggregator <- as.character(NA)
  meta$telemetryUnitID <- as.character(NA)
  
  # ----- Return ---------------------------------------------------------------
  
  # NOTE:  As of 2019-05-14, the PWFSLSmoke meta dataframe still has rownames
  # There should only be a single row in meta
  rownames(meta) <- colnames(data)[-1]
  
  # Add QC algorithm to metadata
  meta$PurpleAirQC_algorithm <- paste0(
    "qc_algorithm = ", qc_algorithm,
    "; period = ", period,
    "; parameter = ", parameter,
    "; channel = ", channel,
    "; min_count = ", min_count
  )
  
  as_object <- list(
    meta = meta, 
    data = data
  )
  
  class(as_object) <- c("airsensor", "ws_monitor")
  
  return(as_object)
  
}

# ===== DEBUG ================================================================

if ( FALSE ) {
  
  pat <- example_pat
  period <- "1 hour"
  parameter <- "pm25"
  channel <- "ab"
  qc_algorithm <- "hourly_AB_01"
  min_count <- 20
  
}  

