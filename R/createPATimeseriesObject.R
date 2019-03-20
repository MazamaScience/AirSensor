#' @export
#' @import dplyr
#' @title Create a Purple Air Timeseries Object
#' @param pat_raw raw Purple Air timeseries data from \code{downloadParseTimeseriesData()}
#' @return List with original \code{meta} and restructured \code{data} elements
#' @description The 'data' dataframe is converted from 'long format' with temporal
#' resolution of seconds to 'wide format' with temporal resolution in minutes.
#' In the process, the following columns of data are omitted:
#' \itemize{
#' \item{\code{pm1_atm}}
#' \item{\code{pm2.5_atm}}
#' \item{\code{pm10_atm}}
#' }
#' @importFrom rlang .data
#' @import dplyr
#' 
#' @return "pa_timeseries" list of time series PurpleAir data
#' 
#' @seealso \link{downloadParseTimeseriesData}
#' 
#' @example 
#' \dontrun{
#' initializeMazamaSpatialUtils()
#' pas <- pas_load()
#' pat_raw <- downloadParseTimeseriesData(pas, name = 'North Bend Weather', startdate = 20181908)
#' nb_pat <- createPATimeseriesObject(pat_raw)
#' }

createPATimeseriesObject <- function(pat_raw = NULL) {

  # ----- simplify meta -------------------------------------------------------
  
  meta <- pat_raw$meta %>%
    dplyr::filter(is.na(parentID)) %>%
    dplyr::select(ID, label, sensorType,
                  DEVICE_LOCATIONTYPE, THINGSPEAK_PRIMARY_ID, THINGSPEAK_PRIMARY_ID_READ_KEY,
                  longitude, latitude, countryCode, stateCode, timezone,
                  pwfsl_closestDistance, pwfsl_closestMonitorID)
  
  # ----- simplify data -------------------------------------------------------
  
  # NOTE:  The incoming pat_raw has the following structure with 
  # NOTE:  a 'meta' dataframe and a 'data' dataframe:
  # NOTE:  
  # NOTE:  > names(pat_raw$data)
  # NOTE:  [1] "datetime"  "entry_id"  "pm1_atm"   "pm2.5_atm" "pm10_atm"  "uptime"    "rssi"     
  # NOTE:  [8] "temp"      "humidity"  "pm2.5_cf1" "channel"   "memory"    "adc0"     
  
  # Extract useful columns from channel A data
  A <- pat_raw$data %>%
    dplyr::filter(channel == 'A') %>%
    dplyr::select(datetime, uptime, rssi, temperature, humidity, pm2.5_cf1) %>%
    dplyr::rename(datetime_A = datetime, pm25_A = pm2.5_cf1)

  # NOTE:  Expedient conversion to a minute axis with floor_date() 
  A$datetime <- lubridate::floor_date(A$datetime_A, unit="min")
  
  # NOTE:  Check that we don't have duplicates with: any(diff(A$datetime) == 0)
  
  # Extract useful columns from channel B data
  B <- pat_raw$data %>%
    dplyr::filter(channel == 'B') %>%
    dplyr::select(datetime, memory, adc0, pm2.5_cf1) %>%
    dplyr::rename(datetime_B = datetime, pm25_B = pm2.5_cf1)
  
  # NOTE:  Expedient conversion to a minute axis with floor_date() 
  B$datetime <- lubridate::floor_date(B$datetime_B, unit="min")
  
  # NOTE:  Check that we don't have duplicates with: any(diff(B$datetime) == 0)
  
  # Combine dataframes 
  data <- dplyr::full_join(A, B, by = 'datetime') %>%
    dplyr::select(datetime, pm25_A, pm25_B, temperature, humidity, uptime, adc0, rssi, datetime_A, datetime_B) %>%
    dplyr::arrange(datetime)
  
  # Fillin adc0 and rssi using last observation carry forward so both channels have these (they don't change much)
  data <- tidyr::fill(data, adc0, rssi)

  # ----- Create the Purple Air Timeseries (pat) object -----------------------
  
  # Combine meta and data dataframes into a list
  pat <- list(meta = meta, data = data)
  class(pat) <- c("pa_timeseries", class(pat))
  
  return(pat)

}

