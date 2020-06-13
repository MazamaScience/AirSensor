#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.debug
#' @importFrom tidyselect all_of
#' 
#' @title Combine PurpleAir raw dataframes
#' 
#' @param pat_rawList List of dataframes as returned by \code{pat_downloadParseRawData()}.
#' @param plantowerAlgorithm Identifier for which fields to use for PM data.
#' 
#' @return A PurpleAir Timeseries \emph{pat} object.
#' 
#' @description The \code{pat_downloadParseRawData()} function returns four
#' dataframes of data from ThingSpeak. These must be combined into the single
#' \code{data} dataframe found in a 'pat' object. This process involves selecting
#' data columns to use and bringing all data onto a unified time axis.
#' 
#' Two sets of data values exist in the raw data, one for each of two algorithms
#' that convert particle counts into aerosol density.
#' 
#' PurpleAir has the following description:
#' 
#' \emph{
#' The CF_ATM and CF_1 values are calculated from the particle count data with a 
#' proprietary algorithm developed by the PMS5003 laser counter manufacturer, 
#' PlanTower. The specifics of the calculation are not available to the public 
#' (or us for that matter). However, to convert the particle count data (um/dl) 
#' to a mass concentration (ug/m3) they must use an average particle density. 
#' They do provide 2 different mass concentration conversion options; CF_1 uses 
#' the "average particle density" for indoor particulate matter and CF_ATM uses 
#' the "average particle density" for outdoor particulate matter.
#' }
#' 
#' By default, the \pkg{AirSensor} package and all associated archive data
#' use \code{plantowerAlgorithm = CF_ATM}. The ability to set
#' \code{plantowerAlgorithm = CF_1} is intended only for those who wish to do 
#' a detailed analaysis of the difference between these two algorithms.
#' 
#' @references \url{https://www2.purpleair.com/community/faq#!hc-what-is-the-difference-between-cf-1-and-cf-atm}
#' 
#' @examples
#' \donttest{
#' library(AirSensor)
#' 
#' setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
#' 
#' pas <- pas_load()
#' 
#' pat_rawList <- pat_downloadParseRawData(
#'   id = "78df3c292c8448f7_21257",
#'   pas = pas
#' )
#' 
#' pat <- pat_createPATimeseriesObject(pat_rawList)
#' 
#' pat_multiPlot(pat)
#' }

pat_createPATimeseriesObject <- function(
  pat_rawList = NULL,
  plantowerAlgorithm = "CF_ATM"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat_rawList)
  MazamaCoreUtils::stopIfNull(plantowerAlgorithm)
  
  if ( !plantowerAlgorithm %in% c("CF_ATM", "CF_1") ) {
    stop(sprintf("plantowerAlgorithm = '%s' is not recognized",
                 plantowerAlgorithm))
  }

  meta <- pat_rawList$meta
  A_PRIMARY <- pat_rawList$A_PRIMARY
  A_SECONDARY <- pat_rawList$A_SECONDARY
  B_PRIMARY <- pat_rawList$B_PRIMARY
  B_SECONDARY <- pat_rawList$B_SECONDARY
  
  MazamaCoreUtils::stopIfNull(meta)
  MazamaCoreUtils::stopIfNull(A_PRIMARY)
  MazamaCoreUtils::stopIfNull(A_SECONDARY)
  MazamaCoreUtils::stopIfNull(B_PRIMARY)
  MazamaCoreUtils::stopIfNull(B_SECONDARY)
  
  # ----- Simplify meta --------------------------------------------------------
  
  # > names(pat_rawList$meta)
  # [1] "ID"                               "label"                           
  # [3] "DEVICE_LOCATIONTYPE"              "THINGSPEAK_PRIMARY_ID"           
  # [5] "THINGSPEAK_PRIMARY_ID_READ_KEY"   "THINGSPEAK_SECONDARY_ID"         
  # [7] "THINGSPEAK_SECONDARY_ID_READ_KEY" "latitude"                        
  # [9] "longitude"                        "pm25"                            
  # [11] "lastSeenDate"                     "sensorType"                      
  # [13] "flag_hidden"                      "isOwner"                         
  # [15] "humidity"                         "temperature"                     
  # [17] "pressure"                         "age"                             
  # [19] "parentID"                         "flag_highValue"                  
  # [21] "flag_attenuation_hardware"        "Ozone1"                          
  # [23] "Voc"                              "pm25_current"                    
  # [25] "pm25_10min"                       "pm25_30min"                      
  # [27] "pm25_1hr"                         "pm25_6hr"                        
  # [29] "pm25_1day"                        "pm25_1week"                      
  # [31] "statsLastModifiedDate"            "statsLastModifiedInterval"       
  # [33] "deviceID"                         "locationID"                      
  # [35] "deviceDeploymentID"               "countryCode"                     
  # [37] "stateCode"                        "timezone"                        
  # [39] "airDistrict"                      "pwfsl_closestDistance"           
  # [41] "pwfsl_closestMonitorID"           "sensorManufacturer"              
  # [43] "targetPollutant"                  "technologyType"                  
  # [45] "communityRegion"                 
  
  meta <- 
    pat_rawList$meta %>%
    dplyr::filter(is.na(.data$parentID)) %>%
    dplyr::select(.data$ID, 
                  .data$label, 
                  .data$sensorType, 
                  .data$DEVICE_LOCATIONTYPE, 
                  .data$THINGSPEAK_PRIMARY_ID, 
                  .data$THINGSPEAK_PRIMARY_ID_READ_KEY, 
                  .data$THINGSPEAK_SECONDARY_ID, 
                  .data$THINGSPEAK_SECONDARY_ID_READ_KEY, 
                  .data$longitude, 
                  .data$latitude, 
                  .data$countryCode, 
                  .data$stateCode, 
                  .data$timezone, 
                  .data$deviceID, 
                  .data$locationID, 
                  .data$deviceDeploymentID, 
                  .data$pwfsl_closestDistance, 
                  .data$pwfsl_closestMonitorID,
                  .data$sensorManufacturer,
                  .data$targetPollutant,
                  .data$technologyType,
                  .data$communityRegion)
  
  # ----- Create A and B channels ----------------------------------------------

  # NOTE:  Here is the structure of the raw data:
  
  # > names(A_PRIMARY)
  # [1] "created_at"  "entry_id"    "pm1.0_cf1"   "pm2.5_cf1"   "pm10.0_cf1" 
  # [6] "uptime"      "rssi"        "temperature" "humidity"    "pm2.5_atm"  
  # > names(A_SECONDARY)
  # [1] "created_at"  "entry_id"    "counts_0.3"  "counts_0.5"  "counts_1.0" 
  # [6] "counts_2.5"  "counts_5.0"  "counts_10.0" "pm1.0_atm"   "pm10.0_atm" 
  # > names(B_PRIMARY)
  # [1] "created_at" "entry_id"   "pm1.0_cf1"  "pm2.5_cf1"  "pm10.0_cf1"     
  # [6] "memory"     "adc0"       "pressure"   "bsec_iaq"   "pm2.5_atm" 
  # > names(B_SECONDARY)
  # [1] "created_at"  "entry_id"    "counts_0.3"  "counts_0.5"  "counts_1.0" 
  # [6] "counts_2.5"  "counts_5.0"  "counts_10.0" "pm1.0_atm"   "pm10.0_atm" 
  
  # NOTE:  Here is the structure of the data we wish to have in the end:
  
  # TODO:  Update this documentation to reflect new columns
  # > names(example_pat$data)
  # [1] "datetime"    "pm25_A"      "pm25_B"      "pm1_atm_A"   "pm1_atm_B"  
  # [6] "pm25_atm_A"  "pm25_atm_B"  "pm10_atm_A"  "pm10_atm_B"  "temperature"
  # [11] "humidity"    "uptime"      "adc0"        "rssi"        "datetime_A" 
  # [16] "datetime_B" 
  
  # NOTE:  When dropouts occur, you will not always have the same number of
  # NOTE:  rows in each dataframe so we cannot use dplyr::bind_rows(). Instead,
  # NOTE:  we first create a new "datetime" column on a 1-minute time axis and
  # NOTE:  then use dplyr::full_join() to create a combined dataframe that has
  # NOTE:  some missing values.
  
  # ----- A channel ------------------------------------------------------------
  
  A_PRIMARY_columns <- c(
    "datetime", "datetime_A",
    "pm25_A", "pm25_atm_A",
    "uptime", "rssi", "temperature", "humidity"
  )
  
  A_PRIMARY <-
    pat_rawList$A_PRIMARY %>%
    dplyr::mutate(
      datetime = lubridate::floor_date(.data$created_at, unit = "min"),
      datetime_A = .data$created_at,
      pm25_atm_A = .data$pm2.5_atm, 
      pm25_A = .data$pm2.5_atm
    ) %>%
    dplyr::select(all_of(A_PRIMARY_columns))
  
  A_SECONDARY_columns <- c(
    "datetime",
    "pm1_atm_A", "pm10_atm_A"
  )
  
  A_SECONDARY <-
    pat_rawList$A_SECONDARY %>%
    dplyr::mutate(
      datetime = lubridate::floor_date(.data$created_at, unit = "min"),
      pm1_atm_A = .data$pm1.0_atm, 
      pm10_atm_A = .data$pm10.0_atm
    ) %>%
    dplyr::select(all_of(A_SECONDARY_columns))
  
  A_data <- 
    dplyr::full_join(A_PRIMARY, A_SECONDARY, by = "datetime") %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$datetime)
  
  # NOTE:  The result is imperfect and the SECONDARY channel occasionally gets
  # NOTE:  assigned to the next 'datetime' minute as seen here:
  
  #   datetime            datetime_A          pm25_A pm25_atm_A uptime  rssi temperature humidity pm1_atm_A pm10_atm_A
  #   <dttm>              <dttm>               <dbl>      <dbl>  <int> <dbl>       <dbl>    <dbl>     <dbl>      <dbl>
  # 1 2018-08-02 14:39:00 2018-08-02 14:39:29   3.14       3.14   6306   -74          65       63      2.05       3.26
  # 2 2018-08-02 14:40:00 2018-08-02 14:40:49   3.21       3.21   6308   -74          65       63     NA         NA   
  # 3 2018-08-02 14:41:00 NA                   NA         NA        NA    NA          NA       NA      2.48       3.21
  # 4 2018-08-02 14:42:00 2018-08-02 14:42:09   2.12       2.12   6309   -74          65       63      1.3        2.12
  
  # NOTE:  We can take of this by separating our single dataframe into 
  # NOTE:  pieces, subtracting a minute from rows with SECONDARY data only and
  # NOTE:  then recombining.
  
  # Separate into records with full data, PRIMARY only or SECONDARY only
  
  PRIMARY_only_mask <- !is.na(A_data$datetime_A) & is.na(A_data$pm1_atm_A)
  SECONDARY_only_mask <- is.na(A_data$datetime_A) & !is.na(A_data$pm1_atm_A)
  full_mask <- !is.na(A_data$datetime_A) & !is.na(A_data$pm1_atm_A)
  
  fullRecords <- A_data[full_mask, ]
  
  primaryOnlyRecords <- A_data[PRIMARY_only_mask, A_PRIMARY_columns]

  secondaryOnlyRecords <- 
    A_data[SECONDARY_only_mask, A_SECONDARY_columns] %>%
    # Subtract one minute from secondary only so that 'datetime' will match up  
    dplyr::mutate(datetime = .data$datetime - lubridate::dminutes(1))

  # Create the repaired records by joining 
  
  repairedRecords <- 
    dplyr::full_join(primaryOnlyRecords, secondaryOnlyRecords, by = "datetime")  

  # Combine fullRecords and repairedRecords and arrange
  
  A_data <-
    dplyr::bind_rows(fullRecords, repairedRecords) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$datetime)
  

  # ----- B channel ------------------------------------------------------------
  
  B_PRIMARY_columns <- c(
    "datetime", "datetime_B",
    "pm25_B", "pm25_atm_B",
    "memory", "adc0", "pressure", "bsec_iaq"
  )
  
  B_PRIMARY <-
    pat_rawList$B_PRIMARY %>%
    dplyr::mutate(
      datetime = lubridate::floor_date(.data$created_at, unit = "min"),
      datetime_B = .data$created_at,
      pm25_atm_B = .data$pm2.5_atm, 
      pm25_B = .data$pm2.5_atm
    ) %>%
    dplyr::select(all_of(B_PRIMARY_columns))
  
  B_SECONDARY_columns <- c(
    "datetime",
    "pm1_atm_B", "pm10_atm_B"
  )
  
  B_SECONDARY <-
    pat_rawList$B_SECONDARY %>%
    dplyr::mutate(
      datetime = lubridate::floor_date(.data$created_at, unit = "min"),
      pm1_atm_B = .data$pm1.0_atm, 
      pm10_atm_B = .data$pm10.0_atm
    ) %>%
    dplyr::select(all_of(B_SECONDARY_columns))
  
  B_data <- 
    dplyr::full_join(B_PRIMARY, B_SECONDARY, by = "datetime") %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$datetime)
  
  # NOTE:  The result is imperfect and the SECONDARY channel occasionally gets
  # NOTE:  assigned to the next 'datetime' minute as seen here:
  
  # # A tibble: 6 x 10
  #   datetime            datetime_B          pm25_B pm25_atm_B memory  adc0 pressure bsec_iaq pm1_atm_B pm10_atm_B
  #   <dttm>              <dttm>               <dbl>      <dbl>  <int> <dbl>    <dbl>    <dbl>     <dbl>      <dbl>
  # 1 2018-08-01 07:00:00 NA                   NA         NA        NA NA          NA       NA      3.77       5.93
  # 2 2018-08-01 07:01:00 2018-08-01 07:01:18   5.76       5.76  28568  0.06       NA       NA      4.41       6.12
  # 3 2018-08-01 07:02:00 2018-08-01 07:02:38   5.36       5.36  28568  0.06       NA       NA      3.43       5.62
  # 4 2018-08-01 07:03:00 2018-08-01 07:03:58   5.23       5.23  28568  0.06       NA       NA     NA         NA   
  # 5 2018-08-01 07:04:00 NA                   NA         NA        NA NA          NA       NA      3.48       5.77
  # 6 2018-08-01 07:05:00 2018-08-01 07:05:18   5.36       5.36  28568  0.06       NA       NA      4.27       5.57
 
  # NOTE:  We can take of this by separating our single dataframe into 
  # NOTE:  pieces, subtracting a minute from rows with SECONDARY data only and
  # NOTE:  then recombining.
  
  # Separate into records with full data, PRIMARY only or SECONDARY only
  
  PRIMARY_only_mask <- !is.na(B_data$datetime_B) & is.na(B_data$pm1_atm_B)
  SECONDARY_only_mask <- is.na(B_data$datetime_B) & !is.na(B_data$pm1_atm_B)
  full_mask <- !is.na(B_data$datetime_B) & !is.na(B_data$pm1_atm_B)
  
  fullRecords <- B_data[full_mask, ]
  
  primaryOnlyRecords <- B_data[PRIMARY_only_mask, B_PRIMARY_columns]
  
  secondaryOnlyRecords <- 
    B_data[SECONDARY_only_mask, B_SECONDARY_columns] %>%
    # Subtract one minute from secondary only so that 'datetime' will match up  
    dplyr::mutate(datetime = .data$datetime - lubridate::dminutes(1))
  
  # Create the repaired records by joining 
  
  repairedRecords <- 
    dplyr::full_join(primaryOnlyRecords, secondaryOnlyRecords, by = "datetime")  
  
  # Combine fullRecords and repairedRecords and arrange
  
  B_data <-
    dplyr::bind_rows(fullRecords, repairedRecords) %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$datetime)

  # ----- Combine A and B channels ---------------------------------------------
  
  # > names(A)
  # [1] "datetime"    "datetime_A"  "pm25_A"      "pm1_atm_A"   "pm25_atm_A" 
  # [6] "pm10_atm_A"  "uptime"      "rssi"        "temperature" "humidity"   
  # > names(B)
  # [1] "datetime"    "datetime_B"  "pm25_B"      "pm1_atm_B"   "pm25_atm_B"
  # [6] "pm10_atm_B"  "memory"      "adc0"        "pressure"    "bsec_iaq"    
  
  # NOTE:  Here are the columns we wish to have in the end in preferred order:
  # NOTE:  If we just take columns 1-6 we have a very useful dataframe.
  
  # NOTE:  This set of columns must match those defined in
  # NOTE:    pat_createPATimeseriesObject.R
  patData_columnNames <- c(
    "datetime", 
    "pm25_A", "pm25_B", 
    "temperature", "humidity", "pressure",
    "pm1_atm_A", "pm25_atm_A", "pm10_atm_A",
    "pm1_atm_B", "pm25_atm_B", "pm10_atm_B",
    "uptime", "rssi", "memory", "adc0", "bsec_iaq",
    "datetime_A", "datetime_B"
  )
  
  data <-
    dplyr::full_join(A_data, B_data, by = "datetime") %>%
    dplyr::select(all_of(patData_columnNames)) %>%
    dplyr::distinct()

  # ----- Return ---------------------------------------------------------------
  
  # Combine meta and data dataframes into a list
  pat <- list(meta = meta, data = data)
  class(pat) <- c("pa_timeseries", class(pat))
  
  return(pat)
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  library(AirSensor)
  
  setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1") # SCAQMD sensors
  
  pas <- pas_load()
  
  id <- '78df3c292c8448f7_21257'
  label <- NULL
  startdate <- NULL
  enddate <- NULL
  timezone <- NULL
  baseUrl <- "https://api.thingspeak.com/channels/"
  
  pat_rawList <- pat_downloadParseRawData(
    id,
    label,
    pas,
    startdate,
    enddate,
    timezone,
    baseUrl
  )
 
  plantowerAlgorithm <- "CF_ATM" 

  pat <- pat_createPATimeseriesObject(
    pat_rawList,
    plantowerAlgorithm
  )
  
}
