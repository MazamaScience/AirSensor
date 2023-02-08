#' @export
#' @importFrom rlang .data
#'
#' @title Enhance synoptic data from PurpleAir
#'
#' @description Enhance raw synoptic data from PurpleAir to create an improved
#' dataframe compatible with the \pkg{MazamaLocationUtils} package.
#'
#' Steps include:
#'
#' 1) Replace variable names with more consistent, human readable names.
#'
#' 2) Add spatial metadata for each sensor including:
#' \itemize{
#'   \item{timezone -- Olson timezone}
#'   \item{countryCode -- ISO 3166-1 alpha-2}
#'   \item{stateCode -- ISO 3166-2 alpha-2}
#' }
#'
#' 3) Convert data types from character to \code{POSIXct} and \code{numeric}.
#'
#' 4) Add additional metadata items:
#' \itemize{
#' \item{sensorManufacturer = "Purple Air"}
#' }
#'
#' Filtering by country can speed up the process of enhancement and may be
#' performed by providing a vector of ISO country codes to the \code{countryCodes}
#' argument. By default, no subsetting is performed.
#'
#' Users may also limit results by specifying \code{stateCodes} when
#' \code{countryCodes} is limited to a single country.
#'
#' When a single US state is specified, named \code{counties} may be specified
#' to further limit the results.
#'
#' @param pas_raw Dataframe returned by \code{pas_downloadParseRawData()}.
#' @param countryCodes ISO 3166-1 alpha-2 country codes used to subset the data.
#' @param stateCodes ISO-3166-2 alpha-2 state codes used to subset the data.
#' @param counties US county names or 5-digit FIPS codes used to subset the data.
#'
#' @return Enhanced dataframe of synoptic PurpleAir data.
#'
#' @seealso \link{pas_downloadParseRawData}
#'
#' @references \href{https://www2.purpleair.com}{PurpleAir}
#' @references \href{https://api.purpleair.com/}{PurpleAir API}
#' @references \href{https://www2.purpleair.com/policies/terms-of-service}{PurpleAir Terms of service}
#' @references \href{https://www2.purpleair.com/pages/license}{PurpleAir Data license}
#' @references \href{https://www2.purpleair.com/pages/attribution}{PurpleAir Data Attribution}
#'

pas_enhanceRawData <- function(
  pas_raw = NULL,
  countryCodes = NULL,
  stateCodes = NULL,
  counties = NULL
) {

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(pas_raw)

  if ( !is.data.frame(pas_raw) )
    stop("parameter 'pas_raw' parameter is not a dataframe")

  # Guarantee uppercase codes
  countryCodes <- toupper(countryCodes)

  # Validate countryCodes
  if ( any(!(countryCodes %in% countrycode::codelist$iso2c)) )
    stop("parameter 'countryCodes' has values that are not recognized as ISO-2 country codes")

  if ( !is.null(stateCodes) ) {
    if ( is.null(countryCodes) ) {
      stop("'stateCodes' can only be used when also specifying a single country with 'countryCodes'")
    } else if ( length(countryCodes) != 1 ) {
      stop("please limit 'countryCodes' to a single country when using 'stateCodes'")
    }
    if ( !is.null(counties) ) {
      if ( length(stateCodes) != 1 ) {
        stop("please limit 'stateCodes' to a single state when using 'counties'")
      }
    }
  }

  # ----- Harmonize table ------------------------------------------------------

  # > dplyr::glimpse(pas_raw, width = 75)
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
  # $ rssi                 <chr> "-75", "-66", "-81", "-81", "-60", "-72", "-…
  # $ uptime               <chr> "11401", "36354", "70", "1270", "1730", "148…
  # $ pa_latency           <chr> "382", "238", NA, "257", "224", "208", "249"…
  # $ memory               <chr> "15192", "15088", "30184", "15136", "15424",…
  # $ last_seen            <chr> "1651271770", "1651271714", "1651271741", "1…
  # $ last_modified        <chr> "1645469408", "1506718153", "1648969388", "1…
  # $ date_created         <chr> "1633552393", "1484435197", "1484454581", "1…
  # $ channel_state        <chr> "3", "3", "3", "3", "3", "3", "3", "3", "3",…
  # $ channel_flags        <chr> "0", "0", "0", "0", "0", "0", "0", "0", "2",…
  # $ channel_flags_manual <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0",…
  # $ channel_flags_auto   <chr> "0", "0", "0", "0", "0", "0", "0", "0", "2",…
  # $ confidence           <chr> "100", "100", "100", "100", "100", "100", "1…
  # $ confidence_manual    <chr> "100", "100", "100", "100", "100", "100", "1…
  # $ confidence_auto      <chr> "100", "100", "100", "100", "100", "100", "1…
  # $ humidity             <chr> "26", "43", "38", "31", "32", "37", "43", "4…
  # $ temperature          <chr> "62", "65", "69", "74", "74", "67", "61", "6…
  # $ pressure             <chr> "892.4", "1014.7", "1013.8", "1014.2", "1013…
  # $ pm2.5_10minute       <chr> "0.1", "3", "2.2", "1.8", "1", "2.3", "1.8",…
  # $ pm2.5_30minute       <chr> "0.2", "3.6", "2.9", "2.4", "1.2", "3.2", "2…
  # $ pm2.5_60minute       <chr> "0.4", "4", "3.6", "2.9", "1.8", "3.7", "2.2…
  # $ pm2.5_6hour          <chr> "1", "4", "4.6", "3.4", "2.4", "3.8", "1.4",…
  # $ pm2.5_24hour         <chr> "1.2", "3.2", "4.8", "3.8", "2.4", "3.2", "1…
  # $ pm2.5_1week          <chr> "2.6", "4.5", "6.8", "4.8", "4.4", "3.9", "1…
  # $ primary_id_a         <chr> "1528330", "214110", "214181", "214469", "21…
  # $ primary_key_a        <chr> "9UCNK357N813BXAS", "U7OR5QH16KYA2MPE", "7WQ…
  # $ secondary_id_a       <chr> "1528331", "214111", "214182", "214470", "21…
  # $ secondary_key_a      <chr> "2U3LINBJK83JFXNE", "RA40WAKD0ZHVDH1K", "2M0…
  # $ primary_id_b         <chr> "1528332", "214112", "214183", "214471", "21…
  # $ primary_key_b        <chr> "9ZNIQQM2ZQKCRFYF", "5X8IIT6314C8SK3I", "0VQ…
  # $ secondary_id_b       <chr> "1528333", "214113", "214184", "214472", "21…
  # $ secondary_key_b      <chr> "ICJZ9D888O7TB21S", "26HVB5N9565P603J", "L9C…

  pas <-
    pas_raw %>%

    # * Rename columns -----
    dplyr::rename(
      privacy = .data$private
    ) %>%

    # * Modify columns -----
    dplyr::mutate(
      sensorManufacturer = "Purple Air",
      deviceID = paste0("pa.", .data$sensor_index),
      privacy = dplyr::if_else(.data$privacy == "0", "public", "private", as.character(NA)),
      location_type = dplyr::if_else(.data$location_type == "0", "outside", "inside", as.character(NA)),

      longitude = as.numeric(.data$longitude),
      latitude = as.numeric(.data$latitude),
      altitude = as.numeric(.data$altitude),
      elevation = round(as.numeric(.data$altitude) * 0.3048), # convert from feet to meters

      position_rating = as.numeric(.data$position_rating),
      led_brightness = as.numeric(.data$led_brightness),
      rssi = as.numeric(.data$rssi),
      uptime = as.numeric(.data$uptime),
      pa_latency = as.numeric(.data$pa_latency),
      memory = as.numeric(.data$memory),

      last_seen = as.POSIXct(as.numeric(.data$last_seen), tz = "UTC", origin = lubridate::origin),
      last_modified = as.POSIXct(as.numeric(.data$last_modified), tz = "UTC", origin = lubridate::origin),
      date_created = as.POSIXct(as.numeric(.data$date_created), tz = "UTC", origin = lubridate::origin),

      confidence = as.numeric(.data$confidence),
      confidence_manual = as.numeric(.data$confidence_manual),
      confidence_auto = as.numeric(.data$confidence_auto),

      humidity = as.numeric(.data$humidity),
      temperature = as.numeric(.data$temperature),
      pressure = as.numeric(.data$pressure),

      pm2.5_10minute = as.numeric(.data$pm2.5_10minute),
      pm2.5_30minute = as.numeric(.data$pm2.5_30minute),
      pm2.5_60minute = as.numeric(.data$pm2.5_60minute),
      pm2.5_6hour = as.numeric(.data$pm2.5_6hour),
      pm2.5_24hour = as.numeric(.data$pm2.5_24hour),
      pm2.5_1week = as.numeric(.data$pm2.5_1week)

    ) %>%

    # * Remove unwanted columns -----
    dplyr::select(-c(
      "icon"
    )) %>%

    # * Add core metadata -----
    MazamaLocationUtils::table_addCoreMetadata() %>%

    # Fill in new columns where possible
    dplyr::mutate(
      deviceDeploymentID = paste0(.data$locationID, "_", .data$deviceID),
      locationName = .data$name
    )

  # Put 'deviceDeploymentID' and 'deviceID' in front
  startingIDs <- c("deviceDeploymentID", "deviceID", "locationID")
  otherColumns <- setdiff(names(pas), startingIDs)
  orderedColumns <- c(startingIDs, otherColumns)
  pas <- pas %>% dplyr::select(orderedColumns)

  # ----- Add spatial metadata -------------------------------------------------

  # * countryCode -----

  pas$countryCode <-
    MazamaSpatialUtils::getCountryCode(
      longitude = pas$longitude,
      latitude = pas$latitude,
      countryCodes = countryCodes,
      allData = FALSE,
      useBuffering = FALSE            # No buffering needed with the EEZ dataset
    )

  # Limit to valid countryCodes
  pas <-
    pas %>%
    dplyr::filter(!is.na(.data$countryCode))

  # * stateCode -----

  # Suppress annoying 'Discarded datum Unknown' messages
  suppressWarnings({
    pas$stateCode <-
      MazamaSpatialUtils::getStateCode(
        longitude = pas$longitude,
        latitude = pas$latitude,
        countryCodes = countryCodes,
        allData = FALSE,
        useBuffering = TRUE
      )
  })

  # Limit to valid stateCodes
  if ( !is.null(stateCodes) ) {
    pas <-
      pas %>%
      dplyr::filter(.data$stateCode %in% stateCodes)
  }

  # * countyName -----

  if ( length(countryCodes) == 1 && countryCodes[1] == "US" ) {

    # Suppress annoying 'Discarded datum Unknown' messages
    suppressWarnings({
      pas$countyName <-
        MazamaSpatialUtils::getUSCounty(
          longitude = pas$longitude,
          latitude = pas$latitude,
          stateCodes = stateCodes,
          allData = FALSE,
          useBuffering = TRUE
        )
    })

    # Limit to valid counties
    if ( !is.null(counties) ) {
      counties <- as.character(counties)
      # Convert from FIPS to countyName
      if ( stringr::str_detect(counties[1], "[0-9]{5}") ) {
        counties <-
          MazamaSpatialUtils::US_countyFIPSToName(
            state = stateCodes,
            countyFIPS = counties
          )
      }
      # Handle input inconsistencies
      counties <-
        stringr::str_to_title(counties) %>%
        stringr::str_replace(" County", "")
      # Limit to valid counties
      pas <-
        pas %>%
        dplyr::filter(.data$countyName %in% counties)
    }

  }

  # * timezone -----

  # Suppress annoying 'Discarded datum Unknown' messages
  suppressWarnings({
    pas$timezone <-
      MazamaSpatialUtils::getTimezone(
        longitude = pas$longitude,
        latitude = pas$latitude,
        countryCodes = countryCodes,
        allData = FALSE,
        useBuffering = TRUE
      )
  })

  # ----- Return ---------------------------------------------------------------

  # Add the "purple_air_synoptic" class name
  class(pas) <- union("purple_air_synoptic", class(pas))

  return(pas)

}
