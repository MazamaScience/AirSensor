#' #' @export
#' #' @importFrom rlang .data
#' #' @import dplyr
#' #' 
#' #' @title Create a PurpleAir Timeseries object
#' #' 
#' #' @param pat_rawList Raw PurpleAir timeseries data from \code{pat_downloadParseRawData()}
#' #' 
#' #' @return List with \code{meta} and \code{data} elements.
#' #' 
#' #' @description The timeseries dataframes present in \code{pat_rawList} are
#' #' merged to create a uniform dataframe with temporal resolution in minutes.
#' #' In the process, the following columns of data are omitted:
#' #' \itemize{
#' #' \item{\code{pm1_0_atm}}
#' #' \item{\code{pm2_5_atm}}
#' #' \item{\code{pm10_0_atm}}
#' #' }
#' #' 
#' #' @note 
#' #' On January 13, 2020 the PurpleAir FAQ "Whsat's the difference between CF_1
#' #' and CF_ATM?" contained the following text:
#' #' 
#' #' \preformatted{
#' #' The CF_ATM and CF_1 values are calculated from the particle count data with a 
#' #' proprietary algorithm developed by the PMS5003 laser counter manufacturer, 
#' #' PlanTower. The specifics of the calculation are not available to the public 
#' #' (or us for that matter). However, to convert the particle count data (um/dl) 
#' #' to a mass concentration (ug/m3) they must use an average particle density. 
#' #' They do provide 2 different mass concentration conversion options; CF_1 uses 
#' #' the "average particle density" for indoor particulate matter and CF_ATM uses 
#' #' the "average particle density" for outdoor particulate matter. Depending on 
#' #' the density of the particles you are measuring the sensor could appear to 
#' #' read "high" or "low". Some groups have developed conversion factors to 
#' #' convert the data from the sensor to match the unique average particle density 
#' #' within their airshed. 
#' #' }
#' #' 
#' #' @return "pa_timeseries" list of time series PurpleAir data
#' #' 
#' #' @seealso \link{pat_downloadParseRawData}
#' #' @references \url{https://www2.purpleair.com/community/faq}
#' #' 
#' #' @examples 
#' #' \dontrun{
#' #' library(AirSensor)
#' #' initializeMazamaSpatialUtils()
#' #' 
#' #' pat_rawList <- pat_downloadParseRawData(
#' #'   label = 'North Bend Weather',
#' #'   pas = example_pas,
#' #'   startdate = 20180908
#' #' )
#' #' 
#' #' pat <- pat_createPATimeseriesObject(pat_rawList)
#' #' pat_multiplot(pat)
#' #' }
#' 
#' pat_createPATimeseriesObject <- function(
#'   pat_rawList = NULL
#' ) {
#' 
#'   # ---- Validate parameters ---------------------------------------------------
#'   
#'   MazamaCoreUtils::stopIfNull(pat_rawList)
#'   
#'   # ----- Simplify meta --------------------------------------------------------
#' 
#'   # > names(pat_rawList$meta)
#'   # [1] "ID"                               "label"                           
#'   # [3] "DEVICE_LOCATIONTYPE"              "THINGSPEAK_PRIMARY_ID"           
#'   # [5] "THINGSPEAK_PRIMARY_ID_READ_KEY"   "THINGSPEAK_SECONDARY_ID"         
#'   # [7] "THINGSPEAK_SECONDARY_ID_READ_KEY" "latitude"                        
#'   # [9] "longitude"                        "pm25"                            
#'   # [11] "lastSeenDate"                     "sensorType"                      
#'   # [13] "flag_hidden"                      "isOwner"                         
#'   # [15] "humidity"                         "temperature"                     
#'   # [17] "pressure"                         "age"                             
#'   # [19] "parentID"                         "flag_highValue"                  
#'   # [21] "flag_attenuation_hardware"        "Ozone1"                          
#'   # [23] "Voc"                              "pm25_current"                    
#'   # [25] "pm25_10min"                       "pm25_30min"                      
#'   # [27] "pm25_1hr"                         "pm25_6hr"                        
#'   # [29] "pm25_1day"                        "pm25_1week"                      
#'   # [31] "statsLastModifiedDate"            "statsLastModifiedInterval"       
#'   # [33] "deviceID"                         "locationID"                      
#'   # [35] "deviceDeploymentID"               "countryCode"                     
#'   # [37] "stateCode"                        "timezone"                        
#'   # [39] "airDistrict"                      "pwfsl_closestDistance"           
#'   # [41] "pwfsl_closestMonitorID"           "sensorManufacturer"              
#'   # [43] "targetPollutant"                  "technologyType"                  
#'   # [45] "communityRegion"                 
#'   
#'   meta <- 
#'     pat_rawList$meta %>%
#'     dplyr::filter(is.na(.data$parentID)) %>%
#'     dplyr::select(.data$ID, 
#'                   .data$label, 
#'                   .data$sensorType, 
#'                   .data$DEVICE_LOCATIONTYPE, 
#'                   .data$THINGSPEAK_PRIMARY_ID, 
#'                   .data$THINGSPEAK_PRIMARY_ID_READ_KEY, 
#'                   .data$longitude, 
#'                   .data$latitude, 
#'                   .data$countryCode, 
#'                   .data$stateCode, 
#'                   .data$timezone, 
#'                   .data$deviceID, 
#'                   .data$locationID, 
#'                   .data$deviceDeploymentID, 
#'                   .data$pwfsl_closestDistance, 
#'                   .data$pwfsl_closestMonitorID,
#'                   .data$sensorManufacturer,
#'                   .data$targetPollutant,
#'                   .data$technologyType,
#'                   .data$communityRegion)
#'   
#'   # ----- Simplify data --------------------------------------------------------
#'   
#'   # Remove any duplicate data records
#'   pat_rawList$data <- dplyr::distinct(pat_rawList$data)
#'   
#'   # NOTE:  The incoming pat_rawList has the following structure with 
#'   # NOTE:  a 'meta' dataframe and a 'data' dataframe:
#'   # NOTE:  
#'   # NOTE:  > names(pat_rawList$data)
#'   # NOTE:   [1] "datetime"  "entry_id"  "pm1_0_atm"   "pm2_5_atm" "pm10_0_atm"    
#'   # NOTE:   [6] "uptime"    "rssi"      "temp"      "humidity"  "pm2_5_cf_1"
#'   # NOTE:  [11] "channel"   "memory"    "adc0"     
#'   
#'   # Extract useful columns from channel A data
#'   A <- 
#'     pat_rawList$data %>%
#'     dplyr::filter(.data$channel == 'A') %>%
#'     dplyr::select(.data$datetime, 
#'                   .data$uptime, 
#'                   .data$rssi, 
#'                   .data$temperature, 
#'                   .data$humidity, 
#'                   .data$pm1_0_atm, 
#'                   .data$pm2_5_atm, 
#'                   .data$pm10_0_atm, 
#'                   .data$pm2_5_cf_1) %>%
#'     dplyr::rename(datetime_A = .data$datetime, 
#'                   pm1_0_atm_A = .data$pm1_0_atm,
#'                   pm25_atm_A = .data$pm2_5_atm,
#'                   pm10_0_atm_A = .data$pm10_0_atm,
#'                   pm25_cf1_A = .data$pm2_5_cf_1) %>%
#'     dplyr::mutate(pm25_A = .data$pm25_atm_A)
#'   
#'   # NOTE:  Expedient conversion to a minute axis with floor_date() 
#'   A$datetime <- lubridate::floor_date(A$datetime_A, unit="min")
#'   
#'   # NOTE:  Check that we don't have duplicates with: any(diff(A$datetime) == 0)
#'   
#'   # Extract useful columns from channel B data
#'   B <- 
#'     pat_rawList$data %>%
#'     dplyr::filter(.data$channel == 'B') %>%
#'     dplyr::select(.data$datetime, 
#'                   .data$memory, 
#'                   .data$adc0, 
#'                   .data$pm1_0_atm, 
#'                   .data$pm2_5_atm, 
#'                   .data$pm10_0_atm, 
#'                   .data$pm2_5_cf_1) %>%
#'     dplyr::rename(datetime_B = .data$datetime, 
#'                   pm1_0_atm_B = .data$pm1_0_atm,
#'                   pm25_atm_B = .data$pm2_5_atm,
#'                   pm10_0_atm_B = .data$pm10_0_atm,
#'                   pm25_cf1_B = .data$pm2_5_cf_1) %>%
#'     dplyr::mutate(pm25_B = .data$pm25_atm_B)
#'   
#'   # NOTE:  Expedient conversion to a minute axis with floor_date() 
#'   B$datetime <- lubridate::floor_date(B$datetime_B, unit="min")
#'   
#'   # NOTE:  Check that we don't have duplicates with: any(diff(B$datetime) == 0)
#'   
#'   # Combine dataframes 
#'   data <- 
#'     dplyr::full_join(A, B, by = 'datetime') %>%
#'     dplyr::select(.data$datetime, 
#'                   .data$pm25_A, 
#'                   .data$pm25_B, 
#'                   .data$pm1_0_atm_A, 
#'                   .data$pm1_0_atm_B, 
#'                   .data$pm25_atm_A, 
#'                   .data$pm25_atm_B, 
#'                   .data$pm10_0_atm_A, 
#'                   .data$pm10_0_atm_B, 
#'                   .data$pm25_cf1_A, 
#'                   .data$pm25_cf1_B, 
#'                   .data$temperature, 
#'                   .data$humidity,
#'                   .data$uptime, 
#'                   .data$adc0, 
#'                   .data$rssi, 
#'                   .data$datetime_A, 
#'                   .data$datetime_B) %>%
#'     dplyr::arrange(.data$datetime)
#'   
#'   # Fill in adc0 and rssi using last observation carry forward so both 
#'   # channels have these (they don't change much)
#'   data <- tidyr::fill(data, .data$adc0, .data$rssi)
#'   
#'   # ----- Create the PurpleAir Timeseries (pat) object ------------------------
#'   
#'   # Combine meta and data dataframes into a list
#'   pat <- list(meta = meta, data = data)
#'   class(pat) <- c("pa_timeseries", class(pat))
#'   
#'   # Remove any duplicate data records
#'   pat <- pat_distinct(pat)
#'   
#'   # ----- Return ---------------------------------------------------------------
#'   
#'   return(pat)
#'   
#' }
#' 
