#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.error logger.debug logger.isInitialized
#' @importFrom MazamaCoreUtils getAPIKey
#'
#' @title Download synoptic data from PurpleAir
#'
#' @param apiReadKey PurpleAir API Read Key. If \code{apiReadKey = NULL}, it
#' will be obtained using \code{getAPIKey("PurpleAir-read")}.
#' See \code{MazamaCoreUtils::\link[MazamaCoreUtils:setAPIKey]{setAPIKey}}.
#' @param maxAge Number of seconds used to filter results to only include sensors
#' modified or updated within the \code{maxAge} seconds. Using a value of 0 will match all sensors.
#' @param outsideOnly Logical specifying whether to restrict requests to outside sensors only.
#' @param west Longitude of the western edge of the bounding box in which to find sensors.
#' @param east Longitude of the eastern edge of the bounding box in which to find sensors.
#' @param south Latitude of the southern edge of the bounding box in which to find sensors.
#' @param north Latitude of the northern edge of the bounding box in which to find sensors.
#' @param baseUrl Base URL for the PurpleAir API.
#'
#' @return Dataframe of synoptic PurpleAir data.
#'
#' @description Download and parse synoptic data for PurpleAir within the
#' specified region.
#'
#' The synoptic data provides access to data from many PurpleAir sensors at
#' a moment in time and includes both metadata and recent PM2.5 averages for
#' each sensor.
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
#' pas_raw <-
#'   pas_downloadParseRawData(
#'     apiReadKey = PA_API_READ_KEY,
#'     maxAge = 3600 * 24,
#'     outsideOnly = TRUE,
#'     west = -125,
#'     east = -117,
#'     south = 42,
#'     north = 49
#'   )
#'
#' View(pas_raw[1:100,])
#'
#' }, silent = FALSE)
#' }

pas_downloadParseRawData <- function(
  apiReadKey = NULL,
  maxAge = 3600 * 24 * 7,
  outsideOnly = TRUE,
  west = NULL,
  east = NULL,
  south = NULL,
  north = NULL,
  baseUrl = "https://api.purpleair.com/v1/sensors"
) {

  # ----- Validate parameters --------------------------------------------------

  if ( is.null(apiReadKey) )
    apiReadKey <- MazamaCoreUtils::getAPIKey("PurpleAir-read")

  MazamaCoreUtils::stopIfNull(apiReadKey)
  maxAge <- MazamaCoreUtils::setIfNull(maxAge, 3600 * 7 * 24)
  MazamaCoreUtils::stopIfNull(outsideOnly)
  MazamaCoreUtils::stopIfNull(west)
  MazamaCoreUtils::stopIfNull(east)
  MazamaCoreUtils::stopIfNull(south)
  MazamaCoreUtils::stopIfNull(north)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # Ensure correct order of longitudes
  if ( west > east ) {
    a <- east
    east <- west
    west <- a
  }

  # Ensure correct order of latitudes
  if ( south > north) {
    a <- north
    north <- south
    south <- a
  }

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # Placeholder in case things get more complicated
  webserviceUrl <- baseUrl

  # From: https://api.purpleair.com/#api-sensors-get-sensor-data
  #
  # The 'Fields' parameter specifies which 'sensor data fields' to include in the response. It is a comma separated list with one or more of the following:
  #
  # Station information and status fields:
  #   name, icon, model, hardware, location_type, private, latitude, longitude, altitude, position_rating, led_brightness, firmware_version, firmware_upgrade, rssi, uptime, pa_latency, memory, last_seen, last_modified, date_created, channel_state, channel_flags, channel_flags_manual, channel_flags_auto, confidence, confidence_manual, confidence_auto
  #
  # Environmental fields:
  #   humidity, humidity_a, humidity_b, temperature, temperature_a, temperature_b, pressure, pressure_a, pressure_b
  #
  # Miscellaneous fields:
  #   voc, voc_a, voc_b, ozone1, analog_input
  #
  # PM1.0 fields:
  #   pm1.0, pm1.0_a, pm1.0_b, pm1.0_atm, pm1.0_atm_a, pm1.0_atm_b, pm1.0_cf_1, pm1.0_cf_1_a, pm1.0_cf_1_b
  #
  # PM2.5 fields:
  #   pm2.5_alt, pm2.5_alt_a, pm2.5_alt_b, pm2.5, pm2.5_a, pm2.5_b, pm2.5_atm, pm2.5_atm_a, pm2.5_atm_b, pm2.5_cf_1, pm2.5_cf_1_a, pm2.5_cf_1_b
  #
  # PM2.5 pseudo (simple running) average fields:
  #   pm2.5_10minute, pm2.5_10minute_a, pm2.5_10minute_b, pm2.5_30minute, pm2.5_30minute_a, pm2.5_30minute_b, pm2.5_60minute, pm2.5_60minute_a, pm2.5_60minute_b, pm2.5_6hour, pm2.5_6hour_a, pm2.5_6hour_b, pm2.5_24hour, pm2.5_24hour_a, pm2.5_24hour_b, pm2.5_1week, pm2.5_1week_a, pm2.5_1week_b
  #
  # PM10.0 fields:
  #   pm10.0, pm10.0_a, pm10.0_b, pm10.0_atm, pm10.0_atm_a, pm10.0_atm_b, pm10.0_cf_1, pm10.0_cf_1_a, pm10.0_cf_1_b
  #
  # Particle count fields:
  #   0.3_um_count, 0.3_um_count_a, 0.3_um_count_b, 0.5_um_count, 0.5_um_count_a, 0.5_um_count_b, 1.0_um_count, 1.0_um_count_a, 1.0_um_count_b, 2.5_um_count, 2.5_um_count_a, 2.5_um_count_b, 5.0_um_count, 5.0_um_count_a, 5.0_um_count_b, 10.0_um_count 10.0_um_count_a, 10.0_um_count_b
  #
  # ThingSpeak fields, used to retrieve data from api.thingspeak.com:
  #   primary_id_a, primary_key_a, secondary_id_a, secondary_key_a, primary_id_b, primary_key_b, secondary_id_b, secondary_key_b

  fields <-
    paste(
      # Station information and status fields:
      "name, icon, model, hardware, location_type, private, latitude, longitude, altitude, position_rating, led_brightness, firmware_version, firmware_upgrade, rssi, uptime, pa_latency, memory, last_seen, last_modified, date_created, channel_state, channel_flags, channel_flags_manual, channel_flags_auto, confidence, confidence_manual, confidence_auto",
      # Environmental fields:
      "humidity, temperature, pressure",
      # Miscellaneous fields:
      # PM1.0 fields:
      # PM2.5 fields:
      # PM2.5 pseudo average fields:
      "pm2.5_10minute, pm2.5_30minute, pm2.5_60minute, pm2.5_6hour, pm2.5_24hour, pm2.5_1week",
      # PM10.0 fields:
      # Particle count field:
      # ThingSpeak fields:
      "primary_id_a, primary_key_a, secondary_id_a, secondary_key_a, primary_id_b, primary_key_b, secondary_id_b, secondary_key_b",
      sep = ",",
      collapse = ","
    ) %>%
    stringr::str_replace_all(" ", "")

  queryList <-
    list(
      fields = fields,
      nwlng = west,
      nwlat = north,
      selng = east,
      selat = south
    )

  if ( outsideOnly ) {
    queryList$location_type <- 0
  }

  # NOTE:  https://httr.r-lib.org/articles/quickstart.html
  r <-
    httr::GET(
      webserviceUrl,
      httr::add_headers("X-API-Key" = apiReadKey),
      query = queryList
    )

  # * Error response -----

  if ( httr::http_error(r) ) {  # web service failed to respond

    status_code <- httr::status_code(r)

    err_msg <- sprintf(
      "web service error %s from:\n  %s\n\n%s",
      status_code,
      webserviceUrl,
      ###httpcode::http_code(status_code)$explanation
      http_status_codes[[as.character(status_code)]][[2]]
    )

    if ( logger.isInitialized() ) {
      logger.error("Web service failed to respond: %s", webserviceUrl)
      logger.error(err_msg)
    }

    stop(err_msg)

  }

  # * Success response -----

  content <- httr::content(r, as = "text", encoding = "UTF-8") # don't interpret

  # ----- Parse JSON -----------------------------------------------------------

  # * Convert JSON to an R list -----

  PAList <-
    jsonlite::fromJSON(
      content,
      simplifyVector = TRUE,
      simplifyDataFrame = TRUE,
      simplifyMatrix = TRUE,
      flatten = FALSE
    )

  if ( length(PAList$data) == 0 ) {
    stop("no data were returned")
  }

  # > str(PAList)
  # List of 11
  # $ api_version             : chr "V1.0.10-0.0.17"
  # $ time_stamp              : int 1650933831
  # $ data_time_stamp         : int 1650933778
  # $ location_type           : int 0
  # $ max_age                 : int 604800
  # $ firmware_default_version: chr "6.01"
  # $ fields                  : chr [1:45] "sensor_index" "name" "icon" "model" ...
  # $ location_types          : chr [1:2] "outside" "inside"
  # $ channel_states          : chr [1:4] "No PM" "PM-A" "PM-B" "PM-A+PM-B"
  # $ channel_flags           : chr [1:4] "Normal" "A-Downgraded" "B-Downgraded" "A+B-Downgraded"
  # $ data                    : chr [1:958, 1:45] "131707" "896" "912" "920" ...

  if ( logger.isInitialized() ) {
    logger.debug("api_version = \"%s\"", PAList$api_version)
    logger.debug("time_stamp = \"%s\"", as.POSIXct(PAList$time_stamp, tz = "UTC", origin = lubridate::origin))
    logger.debug("data_time_stamp = \"%s\"", as.POSIXct(PAList$data_time_stamp, tz = "UTC", origin = lubridate::origin))
    logger.debug("max_age = \"%s\"", PAList$max_age) # seconds
    logger.debug("firmware_default_version = \"%s\"", PAList$firmware_default_version)
  }


  # Pull out the tibble of results
  colnames(PAList$data) <- PAList$fields
  tbl <- dplyr::as_tibble(PAList$data)

  # NOTE:  This was generated on 2022-04-25

  # > dplyr::glimpse(tbl, width = 75)
  # Rows: 958
  # Columns: 45
  # $ sensor_index         <chr> "131707", "896", "912", "920", "924", "928",…
  # $ name                 <chr> "Havillah", "Chemainus Elementary", "The Hub…
  # $ icon                 <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0",…
  # $ model                <chr> "PA-II-SD", "PA-II", "PA-II", "PA-II", "PA-I…
  # $ hardware             <chr> "2.0+OPENLOG+31037 MB+DS3231+BME280+PMSX003-…
  # $ location_type        <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0",…
  # $ private              <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0",…
  # $ latitude             <chr> "48.819916", "48.930725", "48.72967", "48.79…
  # $ longitude            <chr> "-119.184746", "-123.73338", "-123.66412", "…
  # $ altitude             <chr> "3650", "160", "185", "190", "215", "386", "…
  # $ position_rating      <chr> "0", "5", "5", "5", "5", "5", "5", "5", "0",…
  # $ led_brightness       <chr> "15", "15", "15", "15", "15", "15", "15", "1…
  # $ firmware_version     <chr> "6.01", "6.01", "3.00", "6.01", "6.01", "6.0…
  # $ firmware_upgrade     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
  # $ rssi                 <chr> "-69", "-67", "-90", "-76", "-60", "-72", "-…
  # $ uptime               <chr> "5769", "30723", "89", "42561", "13208", "92…
  # $ pa_latency           <chr> "379", "226", NA, "281", "208", "222", "242"…
  # $ memory               <chr> "16032", "15088", "29272", "14928", "15424",…
  # $ last_seen            <chr> "1650933807", "1650933854", "1650931185", "1…
  # $ last_modified        <chr> "1645469408", "1506718153", "1648969388", "1…
  # $ date_created         <chr> "1633552393", "1484435197", "1484454581", "1…
  # $ channel_state        <chr> "3", "3", "3", "3", "3", "3", "3", "3", "3",…
  # $ channel_flags        <chr> "0", "0", "0", "0", "0", "0", "0", "0", "2",…
  # $ channel_flags_manual <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0",…
  # $ channel_flags_auto   <chr> "0", "0", "0", "0", "0", "0", "0", "0", "2",…
  # $ confidence           <chr> "100", "96", "100", "100", "100", "100", "10…
  # $ confidence_manual    <chr> "100", "96", "100", "100", "100", "100", "10…
  # $ confidence_auto      <chr> "100", "96", "100", "100", "100", "100", "10…
  # $ humidity             <chr> "31", "31", "31", "30", "27", "38", "44", "3…
  # $ temperature          <chr> "61", "69", "74", "69", "71", "62", "58", "6…
  # $ pressure             <chr> "887", "1012", "1011.5", "1012.1", "1011.2",…
  # $ pm2.5_10minute       <chr> "1.4", "0.5", "0.9", "1.4", "0.4", "1.2", "1…
  # $ pm2.5_30minute       <chr> "1", "0.5", "1.4", "1.1", "0.2", "0.8", "1.2…
  # $ pm2.5_60minute       <chr> "1.1", "0.6", "2.7", "0.9", "0.3", "0.6", "1…
  # $ pm2.5_6hour          <chr> "2.1", "2", "6.3", "2.6", "1.9", "2.4", "1.7…
  # $ pm2.5_24hour         <chr> "1.2", "4", "7.2", "4.4", "2.9", "3.8", "1.9…
  # $ pm2.5_1week          <chr> "2.8", "5.2", "7.6", "5.2", "5.2", "4.3", "1…
  # $ primary_id_a         <chr> "1528330", "214110", "214181", "214469", "21…
  # $ primary_key_a        <chr> "9UCNK357N813BXAS", "U7OR5QH16KYA2MPE", "7WQ…
  # $ secondary_id_a       <chr> "1528331", "214111", "214182", "214470", "21…
  # $ secondary_key_a      <chr> "2U3LINBJK83JFXNE", "RA40WAKD0ZHVDH1K", "2M0…
  # $ primary_id_b         <chr> "1528332", "214112", "214183", "214471", "21…
  # $ primary_key_b        <chr> "9ZNIQQM2ZQKCRFYF", "5X8IIT6314C8SK3I", "0VQ…
  # $ secondary_id_b       <chr> "1528333", "214113", "214184", "214472", "21…
  # $ secondary_key_b      <chr> "ICJZ9D888O7TB21S", "26HVB5N9565P603J", "L9C…

  # ----- Return ---------------------------------------------------------------

  return(tbl)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  apiReadKey = API_READ_KEY
  maxAge = 3600 * 24
  outsideOnly = TRUE
  west = -125
  east = -117
  south = 42
  north = 49
  baseUrl = "https://api.purpleair.com/v1/sensors"

  pas_raw <-
    pas_downloadParseRawData(
      apiReadKey,
      maxAge,
      outsideOnly,
      west,
      east,
      south,
      north,
      baseUrl = "https://api.purpleair.com/v1/sensors"
    )





}
