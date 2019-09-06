#' @export
#' @importFrom rlang .data
#' @import dplyr
#' 
#' @title Create a Purple Air Timeseries object
#' 
#' @param pat_raw Raw Purple Air timeseries data from \code{downloadParseTimeseriesData()}
#' 
#' @return List with original \code{meta} and restructured \code{data} elements
#' 
#' @description The 'data' dataframe is converted from 'long format' with temporal
#' resolution of seconds to 'wide format' with temporal resolution in minutes.
#' In the process, the following columns of data are omitted:
#' \itemize{
#' \item{\code{pm1_atm}}
#' \item{\code{pm2.5_atm}}
#' \item{\code{pm10_atm}}
#' }
#' 
#' @return "pa_timeseries" list of time series PurpleAir data
#' 
#' @seealso \link{downloadParseTimeseriesData}
#' 
#' @examples 
#' \dontrun{
#' initializeMazamaSpatialUtils()
#' pas <- pas_load()
#' pat_raw <- downloadParseTimeseriesData(
#'   pas, 
#'   label = 'North Bend Weather', 
#'   startdate = 20180908
#' )
#' pat <- createPATimeseriesObject(pat_raw)
#' pat_multiplot(pat)
#' }

createPATimeseriesObject <- function(
  pat_raw = NULL
) {

  # ---- Validate parameters ---------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat_raw)
  
  # ----- Simplify meta --------------------------------------------------------

  meta <- 
    pat_raw$meta %>%
    dplyr::filter(is.na(.data$parentID)) %>%
    dplyr::select(.data$ID, 
                  .data$label, 
                  .data$sensorType, 
                  .data$DEVICE_LOCATIONTYPE, 
                  .data$THINGSPEAK_PRIMARY_ID, 
                  .data$THINGSPEAK_PRIMARY_ID_READ_KEY, 
                  .data$longitude, 
                  .data$latitude, 
                  .data$countryCode, 
                  .data$stateCode, 
                  .data$timezone, 
                  .data$pwfsl_closestDistance, 
                  .data$pwfsl_closestMonitorID,
                  .data$sensorManufacturer,
                  .data$targetPollutant,
                  .data$technologyType,
                  .data$communityRegion)
  
  # ----- Simplify data --------------------------------------------------------
  
  # Remove any duplicate data records
  pat_raw$data <- dplyr::distinct(pat_raw$data)
  
  # NOTE:  The incoming pat_raw has the following structure with 
  # NOTE:  a 'meta' dataframe and a 'data' dataframe:
  # NOTE:  
  # NOTE:  > names(pat_raw$data)
  # NOTE:   [1] "datetime"  "entry_id"  "pm1_atm"   "pm2.5_atm" "pm10_atm"    
  # NOTE:   [6] "uptime"    "rssi"      "temp"      "humidity"  "pm2.5_cf1"
  # NOTE:  [11] "channel"   "memory"    "adc0"     
  
  # Extract useful columns from channel A data
  A <- 
    pat_raw$data %>%
    dplyr::filter(.data$channel == 'A') %>%
    dplyr::select(.data$datetime, 
                  .data$uptime, 
                  .data$rssi, 
                  .data$temperature, 
                  .data$humidity, 
                  .data$pm2.5_cf1) %>%
    dplyr::rename(datetime_A = .data$datetime, pm25_A = .data$pm2.5_cf1)
  
  # NOTE:  Expedient conversion to a minute axis with floor_date() 
  A$datetime <- lubridate::floor_date(A$datetime_A, unit="min")
  
  # NOTE:  Check that we don't have duplicates with: any(diff(A$datetime) == 0)
  
  # Extract useful columns from channel B data
  B <- 
    pat_raw$data %>%
    dplyr::filter(.data$channel == 'B') %>%
    dplyr::select(.data$datetime, 
                  .data$memory, 
                  .data$adc0, 
                  .data$pm2.5_cf1) %>%
    dplyr::rename(datetime_B = .data$datetime, pm25_B = .data$pm2.5_cf1)
  
  # NOTE:  Expedient conversion to a minute axis with floor_date() 
  B$datetime <- lubridate::floor_date(B$datetime_B, unit="min")
  
  # NOTE:  Check that we don't have duplicates with: any(diff(B$datetime) == 0)
  
  # Combine dataframes 
  data <- 
    dplyr::full_join(A, B, by = 'datetime') %>%
    dplyr::select(.data$datetime, 
                  .data$pm25_A, 
                  .data$pm25_B, 
                  .data$temperature, 
                  .data$humidity,
                  .data$uptime, 
                  .data$adc0, 
                  .data$rssi, 
                  .data$datetime_A, 
                  .data$datetime_B) %>%
    dplyr::arrange(.data$datetime)
  
  # Fillin adc0 and rssi using last observation carry forward so both 
  # channels have these (they don't change much)
  data <- tidyr::fill(data, .data$adc0, .data$rssi)
  
  # ----- Create the Purple Air Timeseries (pat) object ------------------------
  
  # Combine meta and data dataframes into a list
  pat <- list(meta = meta, data = data)
  class(pat) <- c("pa_timeseries", class(pat))
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  # ----- Return ---------------------------------------------------------------
  
  return(pat)
  
}

