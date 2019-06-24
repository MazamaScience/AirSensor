#' @export
#' @importFrom rlang .data
#' 
#' @title Create an Air Sensor object
#' 
#' @param pat PurpleAir Timeseries "pat" object
#' @param period The time period to average to. Can be "sec", "min", "hour", 
#' "day", "DSTday", "week", "month", "quarter" or "year". A number can also
#'  precede these options followed by a space (i.e. "2 day" or "37 min").
#' @param parameter parameter for which to create an \emph{as} object -- one of
#' "pm25", "humidity" or "temperature".
#' @param channel Data channel to use for PM2.5 -- one of "a", "b or "ab".
#' @param qc_algorithm Named QC algorithm to apply to hourly aggregation stats.
#' @param min_count Aggregation bins with fewer than `min_count` measurements
#' will be marked as `NA`.
#'  
#' @description Aggregates data from a \code{pat} object into an "Air Sensor" 
#' object that has appropriate metadata to be used with the *PWFSLSmoke* package.
#'
#' Current QC algorithms exist for \code{channe = "ab"} and include:
#' \itemize{
#' \item{\code{hourly_AB_00}}
#' \item{\code{hourly_AB_01}}
#' }
#' #' 
#' @return "as" object of aggregated time series PurpleAir data
#' 
#' @seealso airSensorQC_hourly_AB_00
#' @seealso airSensorQC_hourly_AB_01
#' 
#' @examples 
#' \dontrun{
#' as <- 
#'   example_pat %>%
#'   pat_filterDate(20180701, 20180901) %>%
#'   pat_createAirSensor()
#' }

pat_createAirSensor <- function(
  pat = NULL,
  period = "1 hour",
  parameter = "pm25",
  channel = "ab",
  qc_algorithm = "hourly_AB_01",
  min_count = 10
) {
  
  # ===== DEBUG ================================================================

  if ( FALSE ) {
    
    pat = example_pat
    period = "1 hour"
    parameter = "pm25"
    channel = "ab"
    qc_algorithm = "hourly_AB_01"
    min_count = 10
    
  }  
  
  # ----- Validate Parameters --------------------------------------------------

  period <- tolower(period)
  parameter <- tolower(parameter)
  channel <- tolower(channel)
  
  if ( !pat_isPat(pat) )
    stop("Required parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Required parameter 'pat' has no data.") 
  
  if ( !parameter %in% c("pm25", "humidity", "temperature") )
    stop("Required parameter 'parameter' must be one of 'pm25', 'humidity' or 'temperature'")
  
  if ( !is.null(channel) ) {
    if ( !channel %in% c("a", "b", "ab") )
      stop("Required parameter 'channel' must be one of 'a', 'b' or 'ab'")
  }
  
  if ( !qc_algorithm %in% c("hourly_AB_00", "hourly_AB_01") ) {
    stop("Required parameter 'qc_algorithm' must be one of 'hourly_AB_00' or 'hourly_AB_01'")
  }
  
  # ----- Raw data QC ----------------------------------------------------------
  
  # Invalidate out-of-spec values. Don't invalidate based on humidity
  pat <- pat_qc(pat, 
                removeOutOfSpec = TRUE,
                max_humidity = NULL)
  
  # ----- Temporal aggregation -------------------------------------------------
  
  aggregationStats <- pat_aggregate(pat,
                                    period = period)
  
  # ----- Aggregation QC -------------------------------------------------------
  
  if ( parameter == "pm25" ) {

    if ( channel == "a" ) {
      
      hourlyData <-
        aggregationStats %>%
        dplyr::mutate(pm25 = .data$pm25_A_mean) %>%
        dplyr::mutate(pm25 = replace(.data$pm25, which(.data$pm25_A_count < min_count), NA) ) %>%
        dplyr::select(.data$datetime, .data$pm25)
      
    } else if ( channel == "b" ) {
      
      hourlyData <-
        aggregationStats %>%
        dplyr::mutate(pm25 = .data$pm25_B_mean) %>%
        dplyr::mutate(pm25 = replace(.data$pm25, which(.data$pm25_B_count < min_count), NA) ) %>%
        dplyr::select(.data$datetime, .data$pm25)
      
    } else if ( channel == "ab" ) {
      
      functionName <- paste0("airSensorQC_", qc_algorithm)
      FUNC <- get(functionName)
      hourlyData <- FUNC(aggregationStats)
      
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
  
  # ----- Create metadata  -----------------------------------------------------
  
  monitorID <- pat$meta$label
  
  names(hourlyData) <- c("datetime", monitorID)
  
  # Copy metadata from pat object
  meta <- 
    pat$meta %>% 
    dplyr::rename(monitorID = .data$label) %>%
    as.data.frame()
  
  # Add metadata found in PWFSLSmoke ws_monitor objects
  meta$elevation <- as.numeric(NA)
  meta$siteName <- meta$label
  meta$countyName <- as.character(NA)
  meta$msaName <- as.character(NA)
  meta$monitorType <- meta$sensorType
  meta$siteID <- as.character(NA)
  meta$instrumentID <- as.character(NA)
  meta$aqsID <- as.character(NA)
  meta$pwfslID <- as.character(NA)
  meta$pwfslDataIngestSource <- as.character(NA)
  meta$telemetryAggregator <- as.character(NA)
  meta$telemetryUnitID <- as.character(NA)

  # NOTE:  As of 2019-05-14, the PWFSLSmoke meta dataframe still has rownames
  rownames(meta) <- colnames(hourlyData)[-1]
  
  as_object <- list(meta = meta, data = hourlyData)
  class(as_object) <- c("airsensor", "ws_monitor")
  
  return(as_object)
  
}
