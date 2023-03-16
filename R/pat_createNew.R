#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.error logger.debug logger.isInitialized
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Create a new PurpleAir timeseries dataset.
#'
#' @param api_key PurpleAir API Read Key. If \code{api_key = NULL}, it
#' will be obtained using \code{getAPIKey("PurpleAir-read")}.
#' See \code{MazamaCoreUtils::\link[MazamaCoreUtils:setAPIKey]{setAPIKey}}.
#' @param pas Previously generated \emph{pas} object containing \code{sensor_index}.
#' @param sensor_index PurpleAir sensor unique identifier.
#' @param startdate Desired start time (ISO 8601) or \code{POSIXct}.
#' @param enddate Desired end time (ISO 8601) or \code{POSIXct}.
#' @param timezone Olson timezone used to interpret dates.
#' @param average Temporal averaging in minutes performed by PurpleAir. One of:
#' 0 (raw), 10, 30, 60 (hour), 360, 1440 (day).
#' @param baseUrl Base URL for the PurpleAir API.
#' @param verbose Logical controlling the generation of warning and error messages.
#'
#' @return A PurpleAir Timeseries \emph{pat} object.
#'
#' @description Cerate a \code{pat} object for a specific \code{sensor_index}.
#'
#' @references \href{https://www2.purpleair.com}{PurpleAir}
#' @references \href{https://api.purpleair.com}{PurpleAir API}
#' @references \href{https://www2.purpleair.com/policies/terms-of-service}{PurpleAir Terms of service}
#' @references \href{https://www2.purpleair.com/pages/license}{PurpleAir Data license}
#' @references \href{https://www2.purpleair.com/pages/attribution}{PurpleAir Data Attribution}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor)
#'
#' pat <-
#'   pat_createNew(
#'     api_key = MY_API_READ_KEY,
#'     pas = example_pas,
#'     sensor_index = "3515",
#'     startdate = "2022-07-01",
#'     enddate = "2022-07-08",
#'     timezone = "UTC",
#'     verbose = TRUE
#'   )
#'
#' View(pat$meta[1:100,])
#'
#' }, silent = FALSE)
#' }

pat_createNew <- function(
    api_key = NULL,
    pas = NULL,
    sensor_index = NULL,
    startdate = NULL,
    enddate = NULL,
    timezone = "UTC",
    average = 0,
    baseUrl = "https://api.purpleair.com/v1/sensors",
    verbose = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(api_key) )
    api_key <- MazamaCoreUtils::getAPIKey("PurpleAir-read")

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(pas)
  MazamaCoreUtils::stopIfNull(sensor_index)
  MazamaCoreUtils::stopIfNull(timezone)
  MazamaCoreUtils::stopIfNull(average)
  MazamaCoreUtils::stopIfNull(baseUrl)
  verbose <- MazamaCoreUtils::setIfNull(verbose, FALSE)

  if ( !average %in% c(0, 10, 30, 60, 360, 1440, 10080, 44640, 53560) ) {
    stop("'average' must be one of: 0, 10, 30, 60, 360, 1440, 10080, 44640, 53560")
  }

  # ----- Determine date sequence ----------------------------------------------

  # NOTE:  In 2023, the PurpleAir API limits each request to two days of data so
  # NOTE:  we break up the time range into separate requests.

  # Find a single, parent record
  pas_single <-
    pas %>%
    dplyr::filter(sensor_index == !!sensor_index)

  if ( nrow(pas_single) == 0 ) {
    stop(sprintf("'pas' has zero records with sensor_index: %s", sensor_index))
  } else if ( nrow(pas_single) > 1 ) {
    stop(sprintf("'pas' has multiple records with sensor_index: %s", sensor_index))
  }

  # Get the timezone associated with this sensor
  if ( is.null(timezone) ) {
    timezone <- pas_single$timezone
  }

  # Create a valid dateRange
  if ( !is.null(startdate) && !is.null(enddate) ) {
    # Don't require day boundaries
    dateRange <- MazamaCoreUtils::timeRange(
      starttime = startdate,
      endtime = enddate,
      timezone = timezone,
      unit = "sec",
      ceilingStart = FALSE,
      ceilingEnd = FALSE
    )
  } else {
    # Default to 2 days with day boundaries
    dateRange <- MazamaCoreUtils::dateRange(
      startdate = startdate,
      enddate = enddate,
      timezone = timezone,
      unit = "sec",
      ceilingStart = FALSE,
      ceilingEnd = FALSE,
      days = 2
    )
  }

  # Create a sequence of every-2-days POSIXct times
  dateSequence <- seq(dateRange[1], dateRange[2], by = lubridate::ddays(2))

  # Tack on the final data if needed
  if ( dateRange[2] > utils::tail(dateSequence, 1) ) {
    dateSequence <- c(dateSequence, dateRange[2])
  }

  # ----- Create data ----------------------------------------------------------

  if ( verbose ) {
    message(sprintf("Requesting data for sensor_index %s from %s to %s",
                    sensor_index, dateSequence[1], dateSequence[2]))
  }

  dataList <- list()

  # Use more specific ID rather than the label
  dataList[[1]] <-
    pat_downloadParseRawData(
      api_key = api_key,
      sensor_index = sensor_index,
      startdate = dateSequence[1],
      enddate = dateSequence[2],
      timezone = timezone,
      average = average,
      fields = AIRSENSOR_1_PAT_FIELDS,
      baseUrl = baseUrl
    )

  if ( length(dateSequence) > 2 ) {

    for ( i in 2:(length(dateSequence) - 1) ) {

      if ( verbose ) {
        message(sprintf("Requesting data for sensor_index %s from %s to %s",
                        sensor_index, dateSequence[i], dateSequence[i+1]))
      }

      dataList[[i]] <-
        pat_downloadParseRawData(
          api_key = api_key,
          sensor_index = sensor_index,
          startdate = dateSequence[i],
          enddate = dateSequence[i + 1],
          timezone = timezone,
          average = average,
          fields = AIRSENSOR_1_PAT_FIELDS,
          baseUrl = baseUrl
        )

    }

  }

  # NOTE:  In AirSensor 1.1, we have the following from pat_raw:
  # > print(names(data), width = 75)
  # [1] "time_stamp"   "sensor_index" "rssi"         "uptime"      
  # [5] "pa_latency"   "memory"       "humidity"     "temperature" 
  # [9] "pressure"     "pm1.0_atm_a"  "pm1.0_atm_b"  "pm2.5_atm_a" 
  # [13] "pm2.5_atm_b"  "pm10.0_atm_a" "pm10.0_atm_b"
  
  # NOTE:  in AirSensor 1.0, the following columns of pat data were available:
  # patData_columnNames <- c(
  #   "datetime", 
  #   "pm25_A", "pm25_B", 
  #   "temperature", "humidity", "pressure",
  #   "pm1_atm_A", "pm25_atm_A", "pm10_atm_A",
  #   "pm1_atm_B", "pm25_atm_B", "pm10_atm_B",
  #   "uptime", "rssi", "memory", "adc0", "bsec_iaq",
  #   "datetime_A", "datetime_B"
  # )
  
  data <-
    # Combine separate data requests
    dplyr::bind_rows(dataList) %>%
    # Rename to AirSensor 1.0 names
    dplyr::rename(
      datetime = .data$time_stamp,
      pm1_atm_A = .data$pm1.0_atm_a,
      pm1_atm_B = .data$pm1.0_atm_b,
      pm25_atm_A = .data$pm2.5_atm_a,
      pm25_atm_B = .data$pm2.5_atm_b,
      pm10_atm_A = .data$pm10.0_atm_a,
      pm10_atm_B = .data$pm10.0_atm_b
    ) %>%
    # Additional variables for backwards compatibility
    dplyr::mutate(
      pm25_A = .data$pm25_atm_A,
      pm25_B = .data$pm25_atm_B,
      adc0 = as.numeric(NA),
      bsec_iaq = as.numeric(NA),
      datetime_A = .data$datetime,
      datetime_B = .data$datetime
    ) %>%
    # Arrange by datetime
    dplyr::arrange(.data$datetime) %>%
    # No duplicate datetimes
    dplyr::distinct(.data$datetime, .keep_all = TRUE)
  
  # ----- Create meta ----------------------------------------------------------

  # Retain device-deployment columns from the pas object
  pat_metaNames <-
    c(
      "deviceDeploymentID", 
      "deviceID", 
      "locationID", 
      "locationName", 
      "longitude", 
      "latitude", 
      "elevation", 
      "countryCode", 
      "stateCode", 
      "countyName", 
      "timezone", 
      "houseNumber", 
      "street", 
      "city", 
      "zip", 
      "sensor_index", 
      # "last_modified", 
      # "date_created", 
      # "last_seen", 
      "privacy", 
      "name", 
      "location_type", 
      "model", 
      "hardware", 
      # "led_brightness", 
      "firmware_version", 
      "firmware_upgrade", 
      # "rssi", 
      # "uptime", 
      # "pa_latency", 
      # "memory", 
      # "position_rating", 
      "altitude", 
      # "channel_state", 
      # "channel_flags", 
      # "channel_flags_manual", 
      # "channel_flags_auto", 
      # "confidence", 
      # "confidence_auto", 
      # "confidence_manual", 
      # "humidity", 
      # "temperature", 
      # "pressure", 
      # "pm2.5_10minute", 
      # "pm2.5_30minute", 
      # "pm2.5_60minute", 
      # "pm2.5_6hour", 
      # "pm2.5_24hour", 
      # "pm2.5_1week", 
      "sensorManufacturer", 
      "ID", 
      "label", 
      "sensorType", 
      # "pm25", 
      "targetPollutant", 
      "technologyType", 
      "pwfsl_closestDistance", 
      "pwfsl_closestMonitorID",
      "communityRegion"
    )

  meta <-
    pas_single %>%
    dplyr::select(dplyr::all_of(pat_metaNames))

  # Remove "pa_synoptic" class
  attributes(meta)$class <- setdiff(attributes(meta)$class, "pa_synoptic")

  # ----- Return ---------------------------------------------------------------

  # Combine meta and data dataframes into a list
  pat <- list(meta = meta, data = data)
  class(pat) <- c("pa_timeseries", class(pat))

  return(pat)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  api_key = MY_API_READ_KEY
  pas = example_pas
  sensor_index = "3515"
  startdate = "2022-07-01"
  enddate = "2022-07-08"
  timezone = "America/Los_Angeles"
  average = 0
  fields = AIRSENSOR_1_PAT_FIELDS
  baseUrl = "https://api.purpleair.com/v1/sensors"
  verbose = TRUE

}
