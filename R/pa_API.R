#
# Wrapper functions for all API endpoints described at:
#   https://api.purpleair.com/
#

# ===== Keys ===================================================================

#' @export
#'
#' @title Check the validity and type for the provided \code{api_key}.
#'
#' @param api_key PurpleAir API key.
#' @param baseUrl URL endpoint for the "Check Key" API.
#'
#' @return List containing key type information.
#'
#' @description Sends a request to the PurpleAirAPI API endpoint described at:
#' \url{https://api.purpleair.com/#api-keys-check-api-key}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor)
#'
#'   pa_checkAPIKey(
#'     api_key = MY_API_READ_KEY
#'   )
#'
#' }, silent = FALSE)
#' }

pa_checkAPIKey <- function(
    api_key = NULL,
    baseUrl = "https://api.purpleair.com/v1/keys"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api.purpleair.com/#api-keys-check-api-key
  webserviceUrl <- baseUrl

  queryList <-
    list(
    )

  PAList <- PurpleAir_API_GET(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  return(PAList)

}


# ===== Sensors ================================================================


#' @export
#'
#' @title Retrieve the latest data of a single sensor.
#'
#' @param api_key PurpleAir API READ key.
#' @param sensor_index The \code{sensor_index} as found in the JSON for this
#' specific sensor.
#' @param fields Optional parameter specifying sensor data fields to return.
#' @param baseUrl URL endpoint for the "Get Member Data" API.
#'
#' @return List containing all recent data for a single sensor.
#'
#' @description Sends a request to the PurpleAir API endpoint described at:
#' \url{https://api.purpleair.com/#api-sensors-get-sensor-data}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor)
#'
#'   pa_getSensorData(
#'     api_key = MY_API_READ_KEY,
#'     sensor_index = MY_SENSOR_INDEX,
#'     fields = SENSOR_DATA_PM25_FIELDS
#'   )
#'
#' }, silent = FALSE)
#' }

pa_getSensorData <- function(
    api_key = NULL,
    sensor_index = NULL,
    fields = SENSOR_DATA_PM25_FIELDS,
    baseUrl = "https://api.purpleair.com/v1/sensors"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(sensor_index)
  MazamaCoreUtils::stopIfNull(fields)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api.purpleair.com/#api-sensors-get-sensor-data
  webserviceUrl <- sprintf("%s/%s", baseUrl, sensor_index)

  if ( is.null(fields) ) {
    queryList <- list()
  } else {
    queryList <-
      list(
        fields = fields
      )
  }

  PAList <- PurpleAir_API_GET(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  # ----- Fix returned data ----------------------------------------------------

  for ( name in names(PAList$sensor) ) {
    if ( name %in% PurpleAir_Numeric_Fields ) {
      PAList$sensor[[name]] <- as.numeric(PAList$sensor[[name]])
    } else if ( name %in% PurpleAir_POSIXct_Fields ) {
      PAList$sensor[[name]] <- lubridate::as_datetime(as.numeric(PAList$sensor[[name]]))
    }
  }

  PAList$sensor$stats$time_stamp <-
    lubridate::as_datetime(as.numeric(PAList$sensor$stats$time_stamp))
  PAList$sensor$stats_a$time_stamp <-
    lubridate::as_datetime(as.numeric(PAList$sensor$stats_a$time_stamp))
  PAList$sensor$stats_b$time_stamp <-
    lubridate::as_datetime(as.numeric(PAList$sensor$stats_b$time_stamp))

  return(PAList)

}


#' @export
#'
#' @title Retrieve historical data for a single sensor as CSV.
#'
#' @param api_key PurpleAir API READ key.
#' @param sensor_index The \code{sensor_index} as found in the JSON for this
#' specific sensor.
#' @param start_timestamp Optional Unix timestamp in seconds since Jan 1, 1970.
#' @param end_timestamp Optional Unix timestamp in seconds since Jan 1, 1970.
#' @param average Temporal averaging in minutes performed by PurpleAir. One of:
#' 0 (raw), 10, 30, 60 (hour), 360, 1440 (day).
#' @param fields Character string specifying which 'sensor data fields' to include in the response.
#' @param baseUrl URL endpoint for the "Get Sensor History (CSV)" API.
#'
#' @return Tibble with historical data for a single sensor.
#'
#' @description Sends a request to the PurpleAir API endpoint described at:
#' \url{https://api.purpleair.com/#api-sensors-get-sensor-history-csv}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor)
#'
#' start <-
#'   MazamaCoreUtils::parseDatetime("2023-01-29 00:00:00", timezone = "UTC") %>%
#'   as.numeric()
#'
#' end <-
#'   MazamaCoreUtils::parseDatetime("2023-01-30 00:00:00", timezone = "UTC") %>%
#'   as.numeric()
#'
#' pa_getSensorHistoryCSV(
#'   api_key = MY_API_READ_KEY,
#'   sensor_index = 896,
#'   start_timestamp = start,
#'   end_timestamp = end,
#'   average = 0,
#'   fields = SENSOR_HISTORY_PM25_FIELDS
#' )
#'
#' }, silent = FALSE)
#' }

pa_getSensorHistoryCSV <- function(
    api_key = NULL,
    sensor_index = NULL,
    start_timestamp = NULL,
    end_timestamp = NULL,
    average = 10,
    fields = SENSOR_HISTORY_PM25_FIELDS,
    baseUrl = "https://api.purpleair.com/v1/sensors"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(sensor_index)
  MazamaCoreUtils::stopIfNull(average)
  MazamaCoreUtils::stopIfNull(fields)
  MazamaCoreUtils::stopIfNull(baseUrl)

  if ( !average %in% c(0, 10, 30, 60, 360, 1440, 10080, 44640, 53560) ) {
    stop("'average' must be one of: 0, 10, 30, 60, 360, 1440, 10080, 44640, 53560")
  }

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api.purpleair.com/#api-sensors-get-sensor-history-csv
  webserviceUrl <- sprintf("%s/%s/history/csv", baseUrl, sensor_index)

  queryList <-
    list(
      average = average,
      fields = fields
    )

  if ( !is.null(start_timestamp) ) {
    queryList$start_timestamp <- start_timestamp
  }

  if ( !is.null(end_timestamp) ) {
    queryList$end_timestamp <- end_timestamp
  }

  tbl <-
    PurpleAir_API_csvGET(
      webserviceUrl = webserviceUrl,
      api_key = api_key,
      queryList = queryList
    ) %>%
    dplyr::arrange(.data$time_stamp)

  return(tbl)

}


#' @export
#'
#' @title Retrieve historical data for a single sensor.
#'
#' @param api_key PurpleAir API READ key.
#' @param sensor_index The \code{sensor_index} as found in the JSON for this
#' specific sensor.
#' @param start_timestamp Optional Unix timestamp in seconds since Jan 1, 1970.
#' @param end_timestamp Optional Unix timestamp in seconds since Jan 1, 1970.
#' @param average Temporal averaging in minutes performed by PurpleAir. One of:
#' 0 (raw), 10, 30, 60 (hour), 360, 1440 (day).
#' @param fields Character string specifying which 'sensor data fields' to include in the response.
#' @param baseUrl URL endpoint for the "Get Sensor History" API.
#'
#' @return List with historical data for a single sensor.
#'
#' @description Sends a request to the PurpleAir API endpoint described at:
#' \url{https://api.purpleair.com/#api-sensors-get-sensor-history}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor)
#'
#' start <-
#'   MazamaCoreUtils::parseDatetime("2023-01-29 00:00:00", timezone = "UTC") %>%
#'   as.numeric()
#'
#' end <-
#'   MazamaCoreUtils::parseDatetime("2023-01-30 00:00:00", timezone = "UTC") %>%
#'   as.numeric()
#'
#' pa_getSensorHistory(
#'   api_key = MY_API_READ_KEY,
#'   sensor_index = 896,
#'   start_timestamp = start,
#'   end_timestamp = end,
#'   average = 0,
#'   fields = SENSOR_HISTORY_PM25_FIELDS
#' )
#'
#' }, silent = FALSE)
#' }

pa_getSensorHistory <- function(
    api_key = NULL,
    sensor_index = NULL,
    start_timestamp = NULL,
    end_timestamp = NULL,
    average = 10,
    fields = SENSOR_HISTORY_PM25_FIELDS,
    baseUrl = "https://api.purpleair.com/v1/sensors"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(sensor_index)
  MazamaCoreUtils::stopIfNull(average)
  MazamaCoreUtils::stopIfNull(fields)
  MazamaCoreUtils::stopIfNull(baseUrl)

  if ( !average %in% c(0, 10, 30, 60, 360, 1440, 10080, 44640, 53560) ) {
    stop("'average' must be one of: 0, 10, 30, 60, 360, 1440, 10080, 44640, 53560")
  }

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api.purpleair.com/#api-sensors-get-sensor-history
  webserviceUrl <- sprintf("%s/%s/history", baseUrl, sensor_index)

  queryList <-
    list(
      average = average,
      fields = fields
    )

  if ( !is.null(start_timestamp) ) {
    queryList$start_timestamp <- start_timestamp
  }

  if ( !is.null(end_timestamp) ) {
    queryList$end_timestamp <- end_timestamp
  }

  PAList <- PurpleAir_API_GET(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  # ----- Fix returned data ----------------------------------------------------

  colnames(PAList$data) <- PAList$fields
  tbl <- dplyr::as_tibble(PAList$data)

  # Convert to proper class
  for ( name in names(tbl) ) {
    if ( name %in% PurpleAir_Numeric_Fields ) {
      tbl[[name]] <- as.numeric(tbl[[name]])
    } else if ( name %in% PurpleAir_POSIXct_Fields ) {
      tbl[[name]] <- lubridate::as_datetime(as.numeric(tbl[[name]]))
    }
  }

  PAList$data <-
    tbl %>%
    dplyr::arrange(.data$time_stamp)

  return(PAList)

}


#' @export
#'
#' @title Retrieve the latest data of multiple sensors matching the provided parameters.
#'
#' @param api_key PurpleAir API READ key.
#' @param fields Optional parameter specifying sensor data fields to return.
#' @param location_type The \code{location_type} of the sensors. Possible values
#' are: 0 = Outside, 1 = Inside or \code{NULL} = both.
#' @param read_keys Optional comma separated list of sensor read_keys is required
#' for private devices. It is separate to the api_key and each sensor has its own
#' read_key. Submit multiple keys by separating them with a comma (,) character
#' for example: key-one,key-two,key-three.
#' @param show_only Optional comma separated list of sensor_index values. When
#' provided, the results are limited only to the sensors included in this list.
#' @param modified_since The modified_since parameter causes only sensors modified
#' after the provided time stamp to be included in the results. Using the
#' time_stamp value from a previous call (recommended) will limit results to
#' those with new values since the last request. Using a value of 0 will match
#' sensors modified at any time.
#' @param max_age Filter results to only include sensors modified or updated
#' within the last \code{max_age} seconds. Using a value of 0 will match sensors of any age.
#' @param nwlng A north west longitude for the bounding box.
#' @param nwlat A north west latitude for the bounding box.
#' @param selng A south east longitude for the bounding box.
#' @param selat A south east latitude for the bounding box.
#' @param baseUrl URL endpoint for the "Get Member Data" API.
#'
#' @return List containing latest data for multiple sensors.
#'
#' @description Sends a request to the PurpleAir API endpoint described at:
#' \url{https://api.purpleair.com/#api-sensors-get-sensors-data}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor)
#'
#'   pa_getSensorsData(
#'     api_key = MY_API_READ_KEY,
#'     fields = SENSOR_DATA_PM25_FIELDS
#'   )
#'
#' }, silent = FALSE)
#' }

pa_getSensorsData <- function(
    api_key = NULL,
    fields = SENSOR_DATA_PM25_FIELDS,
    location_type = NULL,
    read_keys = NULL,
    show_only = NULL,
    modified_since = NULL,
    max_age = 604800,
    nwlng = NULL,
    nwlat = NULL,
    selng = NULL,
    selat = NULL,
    baseUrl = "https://api.purpleair.com/v1/sensors"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(fields)
  MazamaCoreUtils::stopIfNull(max_age)
  MazamaCoreUtils::stopIfNull(nwlng)
  MazamaCoreUtils::stopIfNull(nwlat)
  MazamaCoreUtils::stopIfNull(selng)
  MazamaCoreUtils::stopIfNull(selat)
  MazamaCoreUtils::stopIfNull(baseUrl)

  if ( !is.null(location_type) ) {
    location_type <- as.numeric(location_type)
    if ( !location_type %in% c(0, 1) ) {
      stop("'location_type' must be one of 0 (outside) or 1 (inside).")
    }
  }

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api.purpleair.com/#api-sensors-get-sensors-data
  webserviceUrl <- baseUrl

  queryList <-
    list(
      fields = fields,
      max_age = max_age,
      nwlng = nwlng,
      nwlat = nwlat,
      selng = selng,
      selat = selat
    )

  if ( !is.null(location_type) ) {
    queryList$location_type <- location_type
  }

  if ( !is.null(read_keys) ) {
    queryList$read_keys <- read_keys
  }

  if ( !is.null(show_only) ) {
    queryList$show_only <- show_only
  }

  if ( !is.null(modified_since) ) {
    # NOTE:  this will work with numeric and POSIXct input
    queryList$modified_since <- as.numeric(modified_since)
  }

  PAList <- PurpleAir_API_GET(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  # ----- Fix returned data ----------------------------------------------------

  colnames(PAList$data) <- PAList$fields
  tbl <- dplyr::as_tibble(PAList$data)

  # Convert to proper class
  for ( name in names(tbl) ) {
    if ( name %in% PurpleAir_Numeric_Fields ) {
      tbl[[name]] <- as.numeric(tbl[[name]])
    } else if ( name %in% PurpleAir_POSIXct_Fields ) {
      tbl[[name]] <- lubridate::as_datetime(as.numeric(tbl[[name]]))
    }
  }

  PAList$data <- tbl

  return(PAList)

}


# ===== Groups =================================================================


#' @export
#'
#' @title Create a new group.
#'
#' @param api_key PurpleAir API WRITE key.
#' @param name Human readable name associated with the new group.
#' @param baseUrl URL endpoint for the "Create Group" API.
#'
#' @return List containing all members of the specified group.
#'
#' @description Sends a request to the PurpleAirAPI API endpoint described at:
#' \url{https://api.purpleair.com/#api-groups-create-group}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor)
#'
#'   pa_createGroup(
#'     api_key = MY_API_WRITE_KEY,
#'     name = "My new group"
#'   )
#'
#' }, silent = FALSE)
#' }

pa_createGroup <- function(
    api_key = NULL,
    name = NULL,
    baseUrl = "https://api.purpleair.com/v1/groups"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(name)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api.purpleair.com/#api-groups-create-group
  webserviceUrl <- baseUrl

  queryList <-
    list(
      name = name
    )

  PAList <- PurpleAir_API_POST(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  return(PAList)

}


#' @export
#'
#' @title Create a new member within the specified group.
#'
#' @param api_key PurpleAir API WRITE key.
#' @param group_id The \code{group_id} of the requested group. This group must
#' be owned by the \code{api_key}.
#' @param sensor_index Sensor index as returned by \code{pa_getSensorsData()}.
#' @param baseUrl URL endpoint for the "Create Member" API.
#'
#' @return List containing data associated with this this sensor.
#'
#' @description Sends a request to the PurpleAir API endpoint described at:
#' \url{https://api.purpleair.com/#api-groups-create-member}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor)
#'
#'   pa_getGroupDetail(
#'     api_key = MY_API_READ_KEY,
#'     group_id = MY_GROUP_ID,
#'     sensor_index = MY_SENSOR_INDEX
#'   )
#'
#' }, silent = FALSE)
#' }

pa_createMember <- function(
    api_key = NULL,
    group_id = NULL,
    sensor_index = NULL,
    baseUrl = "https://api.purpleair.com/#api-groups-create-member"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(group_id)
  MazamaCoreUtils::stopIfNull(sensor_index)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api.purpleair.com/#api-groups-get-group-detail
  webserviceUrl <- sprintf("%s/%s/members", baseUrl, group_id)

  queryList <-
    list(
      sensor_index = sensor_index
    )

  PAList <- PurpleAir_API_POST(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  return(PAList)

}


#' @export
#'
#' @title Delete the specified group.
#'
#' @param api_key PurpleAir API WITE key.
#' @param group_id The \code{group_id} to be deleted. This group must
#' be owned by the \code{api_key}.
#' @param baseUrl URL endpoint for the "Delete Group" API.
#'
#' @return No return.
#'
#' @description Sends a request to the PurpleAirAPI API endpoint described at:
#' \url{https://api.purpleair.com/#api-groups-delete-group}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor)
#'
#'   pa_deleteGroup(
#'     api_key = MY_API_READ_KEY,
#'     group_id = MY_GROUP_ID
#'   )
#'
#' }, silent = FALSE)
#' }

pa_deleteGroup <- function(
    api_key = NULL,
    group_id = NULL,
    baseUrl = "https://api.purpleair.com/v1/groups"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(group_id)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api.purpleair.com/#api-groups-delete-group
  webserviceUrl <- sprintf("%s/%s", baseUrl, group_id)

  queryList <-
    list(
    )

  PAList <- PurpleAir_API_DELETE(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  return(PAList)

}


#' @export
#'
#' @title Delete a member from the specified group.
#'
#' @param api_key PurpleAir API WITE key.
#' @param group_id The \code{group_id} of the requested group.
#' @param member_id The \code{member_id} to be deleted.
#' @param baseUrl URL endpoint for the "Delete Member" API.
#'
#' @return No return.
#'
#' @description Sends a request to the PurpleAirAPI API endpoint described at:
#' \url{https://api.purpleair.com/#api-groups-delete-member}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor)
#'
#'   pa_getGroupDetail(
#'     api_key = MY_API_READ_KEY,
#'     group_id = MY_GROUP_ID
#'   )
#'
#' }, silent = FALSE)
#' }

pa_deleteMember <- function(
    api_key = NULL,
    group_id = NULL,
    member_id = NULL,
    baseUrl = "https://api.purpleair.com/v1/groups"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(group_id)
  MazamaCoreUtils::stopIfNull(member_id)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api.purpleair.com/#api-groups-delete-member
  webserviceUrl <- sprintf("%s/%s/members/%s", baseUrl, group_id, member_id)

  queryList <-
    list(
    )

  PAList <- PurpleAir_API_DELETE(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  return(PAList)

}


#' @export
#'
#' @title Retrieve all members of the specified group.
#'
#' @param api_key PurpleAir API READ key.
#' @param group_id The \code{group_id} of the requested group. This group must
#' be owned by the \code{api_key}.
#' @param baseUrl URL endpoint for the "Get Group Detail" API.
#'
#' @return List containing all members of the specified group.
#'
#' @description Sends a request to the PurpleAirAPI API endpoint described at:
#' \url{https://api.purpleair.com/#api-groups-get-group-detail}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor)
#'
#'   pa_getGroupDetail(
#'     api_key = MY_API_READ_KEY,
#'     group_id = MY_GROUP_ID
#'   )
#'
#' }, silent = FALSE)
#' }

pa_getGroupDetail <- function(
    api_key = NULL,
    group_id = NULL,
    baseUrl = "https://api.purpleair.com/v1/groups"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(group_id)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api.purpleair.com/#api-groups-get-group-detail
  webserviceUrl <- sprintf("%s/%s", baseUrl, group_id)

  queryList <-
    list(
    )

  PAList <- PurpleAir_API_GET(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  # ----- Fix returned data ----------------------------------------------------

  tbl <- dplyr::as_tibble(PAList$members)
  tbl$created <- lubridate::as_datetime(tbl$created)

  PAList$members <- tbl

  return(PAList)

}


#' @export
#'
#' @title Retrieve all groups owned by the provided \code{api_key}.
#'
#' @param api_key PurpleAir API READ ey.
#' @param baseUrl URL endpoint for the "Get Groups List" API.
#'
#' @return List containing all groups owned by \code{api_key}.
#'
#' @description Sends a request to the PurpleAirAPI API endpoint described at:
#' \url{https://api.purpleair.com/#api-groups-get-groups-list}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor)
#'
#'   pa_getGroupsList(
#'     api_key = MY_API_READ_KEY
#'   )
#'
#' }, silent = FALSE)
#' }

pa_getGroupsList <- function(
    api_key = NULL,
    baseUrl = "https://api.purpleair.com/v1/groups"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api.purpleair.com/#api-groups-get-groups-list
  webserviceUrl <- baseUrl

  queryList <-
    list(
    )

  PAList <- PurpleAir_API_GET(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  # ----- Fix returned data ----------------------------------------------------

  tbl <- dplyr::as_tibble(PAList$groups)
  tbl$created <- lubridate::as_datetime(tbl$created)

  PAList$groups <- tbl

  return(PAList)

}


#' @export
#'
#' @title Retrieve recent data for a single sensor in the specified group.
#'
#' @param api_key PurpleAir API READ key.
#' @param group_id The \code{group_id} of the requested group. This group must
#' be owned by the \code{api_key}.
#' @param member_id Unique \code{member_id} for a sensor within \code{group_id}.
#' @param fields Optional parameter specifying sensor data fields to return.
#' @param baseUrl URL endpoint for the "Get Member Data" API.
#'
#' @return List containing all recent data for a single sensor.
#'
#' @description Sends a request to the PurpleAir API endpoint described at:
#' \url{https://api.purpleair.com/#api-groups-get-member-data}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor)
#'
#'   pa_getMemberData(
#'     api_key = MY_API_READ_KEY,
#'     group_id = MY_GROUP_ID,
#'     member_id = MY_MEMBER_ID
#'   )
#'
#' }, silent = FALSE)
#' }

pa_getMemberData <- function(
    api_key = NULL,
    group_id = NULL,
    member_id = NULL,
    fields = NULL,
    baseUrl = "https://api.purpleair.com/v1/groups"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(group_id)
  MazamaCoreUtils::stopIfNull(member_id)
  MazamaCoreUtils::stopIfNull(baseUrl)

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api.purpleair.com/#api-groups-get-member-data
  webserviceUrl <- sprintf("%s/%s/members/%s", baseUrl, group_id, member_id)

  if ( is.null(fields) ) {
    queryList <- list()
  } else {
    queryList <-
      list(
        fields = fields
      )
  }

  PAList <- PurpleAir_API_GET(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  # ----- Fix returned data ----------------------------------------------------

  for ( name in names(PAList$sensor) ) {
    if ( name %in% PurpleAir_Numeric_Fields ) {
      PAList$sensor[[name]] <- as.numeric(PAList$sensor[[name]])
    } else if ( name %in% PurpleAir_POSIXct_Fields ) {
      PAList$sensor[[name]] <- lubridate::as_datetime(as.numeric(PAList$sensor[[name]]))
    }
  }

  PAList$sensor$stats$time_stamp <-
    lubridate::as_datetime(as.numeric(PAList$sensor$stats$time_stamp))
  PAList$sensor$stats_a$time_stamp <-
    lubridate::as_datetime(as.numeric(PAList$sensor$stats_a$time_stamp))
  PAList$sensor$stats_b$time_stamp <-
    lubridate::as_datetime(as.numeric(PAList$sensor$stats_b$time_stamp))

  return(PAList)

}


#' @export
#'
#' @title Retrieve historical data for a single sensor of the specified group.
#'
#' @param api_key PurpleAir API READ key.
#' @param group_id The \code{group_id} of the requested group. This group must
#' be owned by the \code{api_key}.
#' @param member_id Unique \code{member_id} for a sensor within \code{group_id}.
#' @param start_timestamp Desired start datetime (ISO 8601).
#' @param end_timestamp Desired end datetime (ISO 8601).
#' @param average Temporal averaging in minutes performed by PurpleAir. One of:
#' 0 (raw), 10, 30, 60 (hour), 360, 1440 (day).
#' @param fields Character string specifying which 'sensor data fields' to include in the response.
#' @param baseUrl URL endpoint for the "Get Groups list" API.
#'
#' @return Tibble with historical data for a single sensor.
#'
#' @description Sends a request to the PurpleAirAPI API endpoint described at:
#' \url{https://api.purpleair.com/#api-groups-get-member-history}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor)
#'
#'   pa_getMemberData(
#'     api_key = MY_API_READ_KEY,
#'     group_id = MY_GROUP_ID,
#'     member_id = MY_MEMBER_ID
#'   )
#'
#' }, silent = FALSE)
#' }

pa_getMemberHistory <- function(
    api_key = NULL,
    group_id = NULL,
    member_id = NULL,
    start_timestamp = NULL,
    end_timestamp = NULL,
    average = 10,
    fields = SENSOR_HISTORY_PM25_FIELDS,
    baseUrl = "https://api.purpleair.com/v1/groups"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(group_id)
  MazamaCoreUtils::stopIfNull(member_id)
  MazamaCoreUtils::stopIfNull(average)
  MazamaCoreUtils::stopIfNull(fields)
  MazamaCoreUtils::stopIfNull(baseUrl)

  if ( !average %in% c(0, 10, 30, 60, 360, 1440, 10080, 44640, 53560) ) {
    stop("'average' must be one of: 0, 10, 30, 60, 360, 1440, 10080, 44640, 53560")
  }

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api.purpleair.com/#api-groups-get-member-history
  webserviceUrl <- sprintf("%s/%s/members/%s/history/csv", baseUrl, group_id, member_id)

  queryList <-
    list(
      average = average,
      fields = fields
    )

  if ( !is.null(start_timestamp) ) {
    queryList$start_timestamp <- start_timestamp
  }

  if ( !is.null(end_timestamp) ) {
    queryList$end_timestamp <- end_timestamp
  }

  tbl <-
    PurpleAir_API_csvGET(
      webserviceUrl = webserviceUrl,
      api_key = api_key,
      queryList = queryList
    ) %>%
    dplyr::arrange(.data$time_stamp)

  return(tbl)

}


#' @export
#'
#' @title Retrieve current data for all sensors in the specified group.
#'
#' @param api_key PurpleAir API READ key.
#' @param group_id The \code{group_id} of the requested group. This group must
#' be owned by the \code{api_key}.
#' @param fields Comma-separated list of 'sensor data fields' to include in the response.
#' @param location_type The \code{location_type} of the sensors. Possible values
#' are: 0 = Outside, 1 = Inside or \code{NULL} = both.
#' @param max_age Filter results to only include sensors modified or updated
#' within the last \code{max_age} seconds. Using a value of 0 will match sensors of any age.
#' @param baseUrl URL endpoint for the "Get Members Data" API.
#'
#' @return List containing current data for all sensors in the specified group.
#'
#' @description Sends a request to the PurpleAir API endpoint described at:
#' \url{https://api.purpleair.com/#api-groups-get-members-data}
#'
#' Retrieves data for all sensors in the specified group.
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor)
#'
#'   pa_getMembersData(
#'     api_key = MY_API_READ_KEY,
#'     group_id = MY_GROUP_ID
#'   )
#'
#' }, silent = FALSE)
#' }

pa_getMembersData <- function(
    api_key = NULL,
    group_id = NULL,
    fields = SENSOR_DATA_PM25_FIELDS,
    location_type = NULL,
    max_age = 604800,
    baseUrl = "https://api.purpleair.com/v1/groups"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(group_id)
  MazamaCoreUtils::stopIfNull(fields)
  MazamaCoreUtils::stopIfNull(max_age)
  MazamaCoreUtils::stopIfNull(baseUrl)

  if ( !is.null(location_type) ) {
    location_type <- as.numeric(location_type)
    if ( !location_type %in% c(0, 1) ) {
      stop("'location_type' must be one of 0 (outside) or 1 (inside).")
    }
  }

  # ----- Request data ---------------------------------------------------------

  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl, "/$", "")

  # See: https://api.purpleair.com/#api-groups-get-members-data
  webserviceUrl <- sprintf("%s/%s/members", baseUrl, group_id)

  queryList <-
    list(
      fields = fields,
      max_age = max_age
    )

  if ( !is.null(location_type) ) {
    queryList$location_type <- location_type
  }

  PAList <- PurpleAir_API_GET(
    webserviceUrl = webserviceUrl,
    api_key = api_key,
    queryList = queryList
  )

  # ----- Fix returned data ----------------------------------------------------

  colnames(PAList$data) <- PAList$fields
  tbl <- dplyr::as_tibble(PAList$data)

  # Convert to proper class
  for ( name in names(tbl) ) {
    if ( name %in% PurpleAir_Numeric_Fields ) {
      tbl[[name]] <- as.numeric(tbl[[name]])
    } else if ( name %in% PurpleAir_POSIXct_Fields ) {
      tbl[[name]] <- lubridate::as_datetime(as.numeric(tbl[[name]]))
    }
  }

  PAList$data <- tbl

  return(PAList)

}


# ===== Public Data ============================================================

#' @export
#' @docType data
#' @name SENSOR_DATA_AVG_PM25_FIELDS
#' @title Comma-separated list of fields needed to create a \emph{pas} object.
#' @format String with comma-separated field names
#' @description Character string with PurpleAir field names used in
#' \code{pas_downloadParseRawData()}. These fields include most of the
#' "information and status" fields, "humidity", "temperature", "pressure" and
#' simple running average PM2.5 fields for different time periods.
#'
#' @references \href{https://api.purpleair.com/#api-sensors-get-sensor-data}{Get Sensor Data API}

SENSOR_DATA_AVG_PM25_FIELDS <-
  paste(
    # Station information and status fields:
    "name, icon, model, hardware, location_type, private, latitude, longitude, altitude, position_rating, led_brightness, firmware_version, firmware_upgrade, rssi, uptime, pa_latency, memory, last_seen, last_modified, date_created, channel_state, channel_flags, channel_flags_manual, channel_flags_auto, confidence, confidence_manual, confidence_auto",
    #
    # Environmental fields:
    ###"humidity, humidity_a, humidity_b, temperature, temperature_a, temperature_b, pressure, pressure_a, pressure_b",
    "humidity, temperature, pressure",
    #
    # Miscellaneous fields:
    #   "voc, voc_a, voc_b, ozone1, analog_input"
    #
    # PM1.0 fields:
    #   "pm1.0, pm1.0_a, pm1.0_b, pm1.0_atm, pm1.0_atm_a, pm1.0_atm_b, pm1.0_cf_1, pm1.0_cf_1_a, pm1.0_cf_1_b"
    #
    # PM2.5 fields:
    #   "pm2.5_alt, pm2.5_alt_a, pm2.5_alt_b, pm2.5, pm2.5_a, pm2.5_b, pm2.5_atm, pm2.5_atm_a, pm2.5_atm_b, pm2.5_cf_1, pm2.5_cf_1_a, pm2.5_cf_1_b"
    #
    # PM2.5 pseudo (simple running) average fields:
    ###"pm2.5_10minute, pm2.5_10minute_a, pm2.5_10minute_b, pm2.5_30minute, pm2.5_30minute_a, pm2.5_30minute_b, pm2.5_60minute, pm2.5_60minute_a, pm2.5_60minute_b, pm2.5_6hour, pm2.5_6hour_a, pm2.5_6hour_b, pm2.5_24hour, pm2.5_24hour_a, pm2.5_24hour_b, pm2.5_1week, pm2.5_1week_a, pm2.5_1week_b",
    "pm2.5_10minute, pm2.5_30minute, pm2.5_60minute, pm2.5_6hour, pm2.5_24hour, pm2.5_1week",
    #
    # PM10.0 fields:
    #   "pm10.0, pm10.0_a, pm10.0_b, pm10.0_atm, pm10.0_atm_a, pm10.0_atm_b, pm10.0_cf_1, pm10.0_cf_1_a, pm10.0_cf_1_b"
    #
    # Particle count fields:
    #   "0.3_um_count, 0.3_um_count_a, 0.3_um_count_b, 0.5_um_count, 0.5_um_count_a, 0.5_um_count_b, 1.0_um_count, 1.0_um_count_a, 1.0_um_count_b, 2.5_um_count, 2.5_um_count_a, 2.5_um_count_b, 5.0_um_count, 5.0_um_count_a, 5.0_um_count_b, 10.0_um_count 10.0_um_count_a, 10.0_um_count_b"
    #
    # ThingSpeak fields, used to retrieve data from api.thingspeak.com:
    #   "primary_id_a, primary_key_a, secondary_id_a, secondary_key_a, primary_id_b, primary_key_b, secondary_id_b, secondary_key_b"
    sep = ",",
    collapse = ","
  ) %>%
  stringr::str_replace_all(" ", "") %>%
  stringr::str_replace_all(",$", "")


#' @export
#' @docType data
#' @name SENSOR_DATA_PM25_FIELDS
#' @title Comma-separated list of fields needed for PM2.5 data analysis.
#' @format String with comma-separated field names
#' @description Character string with default PurpleAir field names used in
#' \code{pas_downloadParaseRawData()}. These fields include most of the
#' "information and status" fields, "humidity", "temperature", "pressure" and
#' the PM2.5 fields for both A and B channels.
#'
#' @references \href{https://api.purpleair.com/#api-sensors-get-sensor-data}{Get Sensor Data API}

SENSOR_DATA_PM25_FIELDS <-
  paste(
    # Station information and status fields:
    "name, icon, model, hardware, location_type, private, latitude, longitude, altitude, position_rating, led_brightness, firmware_version, firmware_upgrade, rssi, uptime, pa_latency, memory, last_seen, last_modified, date_created, channel_state, channel_flags, channel_flags_manual, channel_flags_auto, confidence, confidence_manual, confidence_auto",
    #
    # Environmental fields:
    ###"humidity, humidity_a, humidity_b, temperature, temperature_a, temperature_b, pressure, pressure_a, pressure_b",
    "humidity, temperature, pressure",
    #
    # Miscellaneous fields:
    #   "voc, voc_a, voc_b, ozone1, analog_input"
    #
    # PM1.0 fields:
    #   "pm1.0, pm1.0_a, pm1.0_b, pm1.0_atm, pm1.0_atm_a, pm1.0_atm_b, pm1.0_cf_1, pm1.0_cf_1_a, pm1.0_cf_1_b"
    #
    # PM2.5 fields:
    "pm2.5_alt, pm2.5_alt_a, pm2.5_alt_b, pm2.5, pm2.5_a, pm2.5_b, pm2.5_atm, pm2.5_atm_a, pm2.5_atm_b, pm2.5_cf_1, pm2.5_cf_1_a, pm2.5_cf_1_b",
    #
    # PM2.5 pseudo (simple running) average fields:
    # "pm2.5_10minute, pm2.5_10minute_a, pm2.5_10minute_b, pm2.5_30minute, pm2.5_30minute_a, pm2.5_30minute_b, pm2.5_60minute, pm2.5_60minute_a, pm2.5_60minute_b, pm2.5_6hour, pm2.5_6hour_a, pm2.5_6hour_b, pm2.5_24hour, pm2.5_24hour_a, pm2.5_24hour_b, pm2.5_1week, pm2.5_1week_a, pm2.5_1week_b",
    #
    # PM10.0 fields:
    #   "pm10.0, pm10.0_a, pm10.0_b, pm10.0_atm, pm10.0_atm_a, pm10.0_atm_b, pm10.0_cf_1, pm10.0_cf_1_a, pm10.0_cf_1_b"
    #
    # Particle count fields:
    #   "0.3_um_count, 0.3_um_count_a, 0.3_um_count_b, 0.5_um_count, 0.5_um_count_a, 0.5_um_count_b, 1.0_um_count, 1.0_um_count_a, 1.0_um_count_b, 2.5_um_count, 2.5_um_count_a, 2.5_um_count_b, 5.0_um_count, 5.0_um_count_a, 5.0_um_count_b, 10.0_um_count 10.0_um_count_a, 10.0_um_count_b"
    #
    # ThingSpeak fields, used to retrieve data from api.thingspeak.com:
    #   "primary_id_a, primary_key_a, secondary_id_a, secondary_key_a, primary_id_b, primary_key_b, secondary_id_b, secondary_key_b"
    sep = ",",
    collapse = ","
  ) %>%
  stringr::str_replace_all(" ", "") %>%
  stringr::str_replace_all(",$", "")


#' @export
#' @docType data
#' @name SENSOR_HISTORY_PM25_FIELDS
#' @title Comma-separated list of fields needed for PM2.5 data analysis.
#' @format String with comma-separated field names
#' @description Character string with default PurpleAir field names used in
#' \code{pat_downloadParaseRawData()}. These fields are sufficient for most
#' QC algorithms and include most of the "information and status" fields,
#' "humidity", "temperature", "pressure" and the PM2.5 "pseudo average" fields.
#'
#' @references \href{https://api.purpleair.com/#api-sensors-get-sensor-history-csv}{Get Sensor History API}

# From: https://api.purpleair.com/#api-sensors-get-sensor-history-csv
#
# The 'Fields' parameter specifies which 'sensor data fields' to include in the
# response. Not all fields are available as history fields and we will be working
# to add more as time goes on. Fields marked with an asterisk (*) may not be
# available when using averages. It is a comma separated list with one or more of the following:
#


SENSOR_HISTORY_PM25_FIELDS <-
  paste(
    # Station information and status fields:
    #"hardware, latitude, longitude, altitude, firmware_version",
    "rssi, uptime, pa_latency, memory",
    #
    # Environmental fields:
    ###"humidity, humidity_a, humidity_b, temperature, temperature_a, temperature_b, pressure, pressure_a, pressure_b",
    "humidity, temperature, pressure",
    #
    # Miscellaneous fields:
    #   "voc, voc_a, voc_b, analog_input",
    #
    # PM1.0 fields:
    #   "pm1.0_atm, pm1.0_atm_a, pm1.0_atm_b, pm1.0_cf_1, pm1.0_cf_1_a, pm1.0_cf_1_b",
    #
    # PM2.5 fields:
    "pm2.5_alt, pm2.5_alt_a, pm2.5_alt_b, pm2.5_atm, pm2.5_atm_a, pm2.5_atm_b, pm2.5_cf_1, pm2.5_cf_1_a, pm2.5_cf_1_b",
    #
    # PM10.0 fields:
    #   "pm10.0_atm, pm10.0_atm_a, pm10.0_atm_b, pm10.0_cf_1, pm10.0_cf_1_a, pm10.0_cf_1_b",
    #
    # Visibility fields:
    #   "scattering_coefficient, scattering_coefficient_a, scattering_coefficient_b, deciviews, deciviews_a, deciviews_b, visual_range, visual_range_a, visual_range_b",
    #
    # Particle count fields:
    #   "0.3_um_count, 0.3_um_count_a, 0.3_um_count_b, 0.5_um_count, 0.5_um_count_a, 0.5_um_count_b, 1.0_um_count, 1.0_um_count_a, 1.0_um_count_b, 2.5_um_count, 2.5_um_count_a, 2.5_um_count_b, 5.0_um_count, 5.0_um_count_a, 5.0_um_count_b, 10.0_um_count, 10.0_um_count_a, 10.0_um_count_b"
    sep = ",",
    collapse = ","
  ) %>%
  stringr::str_replace_all(" ", "") %>%
  stringr::str_replace_all(",$", "")

# ===== Private Functions ======================================================


# GET and parse a JSON return

PurpleAir_API_GET <- function(
    webserviceUrl = NULL,
    api_key = NULL,
    queryList = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(webserviceUrl)
  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(queryList)

  # ----- Request data ---------------------------------------------------------

  # NOTE:  https://httr.r-lib.org/articles/quickstart.html
  r <-
    httr::GET(
      webserviceUrl,
      httr::add_headers("X-API-Key" = api_key),
      query = queryList
    )

  # * Error response -----

  if ( httr::http_error(r) ) {  # web service failed to respond

    content <- httr::content(r)

    err_msg <- sprintf(
      "%s - %s",
      content$error,
      content$description
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

  # * Convert to proper class -----

  for ( name in names(PAList) ) {
    if ( name %in% PurpleAir_Numeric_Fields ) {
      PAList[[name]] <- as.numeric(PAList[[name]])
    } else if ( name %in% PurpleAir_POSIXct_Fields ) {
      PAList[[name]] <- lubridate::as_datetime(as.numeric(PAList[[name]]))
    }
  }

  return(PAList)

}


# GET and parse a CSV return

PurpleAir_API_csvGET <- function(
    webserviceUrl = NULL,
    api_key = NULL,
    queryList = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(webserviceUrl)
  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(queryList)

  # ----- Request data ---------------------------------------------------------

  # NOTE:  https://httr.r-lib.org/articles/quickstart.html
  r <-
    httr::GET(
      webserviceUrl,
      httr::add_headers("X-API-Key" = api_key),
      query = queryList
    )

  # * Error response -----

  if ( httr::http_error(r) ) {  # web service failed to respond

    content <- httr::content(r)

    err_msg <- sprintf(
      "%s - %s",
      content$error,
      content$description
    )

    if ( logger.isInitialized() ) {
      logger.error("Web service failed to respond: %s", webserviceUrl)
      logger.error(err_msg)
    }

    stop(err_msg)

  }

  # * Success response -----

  content <- httr::content(r, as = "text", encoding = "UTF-8") # don't interpret

  # ----- Parse CSV ------------------------------------------------------------

  tbl <- readr::read_csv(
    file = content,
    show_col_types = FALSE
  )

  # Convert to proper class
  for ( name in names(tbl) ) {
    if ( name %in% PurpleAir_Numeric_Fields ) {
      tbl[[name]] <- as.numeric(tbl[[name]])
    } else if ( name %in% PurpleAir_POSIXct_Fields ) {
      tbl[[name]] <- lubridate::as_datetime(as.numeric(tbl[[name]]))
    }
  }

  return(tbl)

}


# POST and parse a JSON return

PurpleAir_API_POST <- function(
    webserviceUrl = NULL,
    api_key = NULL,
    queryList = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(webserviceUrl)
  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(queryList)

  # ----- Request data ---------------------------------------------------------

  # NOTE:  https://httr.r-lib.org/articles/quickstart.html
  r <-
    httr::POST(
      webserviceUrl,
      httr::add_headers("X-API-Key" = api_key),
      query = queryList
    )

  # * Error response -----

  if ( httr::http_error(r) ) {  # web service failed to respond

    content <- httr::content(r)

    err_msg <- sprintf(
      "%s - %s",
      content$error,
      content$description
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

  # * Convert to proper class -----

  for ( name in names(PAList) ) {
    if ( name %in% PurpleAir_Numeric_Fields ) {
      PAList[[name]] <- as.numeric(PAList[[name]])
    } else if ( name %in% PurpleAir_POSIXct_Fields ) {
      PAList[[name]] <- lubridate::as_datetime(as.numeric(PAList[[name]]))
    }
  }

  return(PAList)

}


# DELETE and parse a JSON return

PurpleAir_API_DELETE <- function(
    webserviceUrl = NULL,
    api_key = NULL,
    queryList = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(webserviceUrl)
  MazamaCoreUtils::stopIfNull(api_key)
  MazamaCoreUtils::stopIfNull(queryList)

  # ----- Request data ---------------------------------------------------------

  # NOTE:  https://httr.r-lib.org/articles/quickstart.html
  r <-
    httr::DELETE(
      webserviceUrl,
      httr::add_headers("X-API-Key" = api_key),
      query = queryList
    )

  # * Error response -----

  if ( httr::http_error(r) ) {  # web service failed to respond

    content <- httr::content(r)

    err_msg <- sprintf(
      "%s - %s",
      content$error,
      content$description
    )

    if ( logger.isInitialized() ) {
      logger.error("Web service failed to respond: %s", webserviceUrl)
      logger.error(err_msg)
    }

    stop(err_msg)

  }

  # Nothing returned upon success.

  return()

}


# ===== Private data ===========================================================

PurpleAir_Numeric_Fields <- c(
  # Station information and status fields:
  #"name",
  #"icon",
  #"model",
  #"hardware",
  #"location_type",
  #"private",
  "latitude",
  "longitude",
  "altitude",
  "position_rating",
  "led_brightness",
  #"firmware_version",
  #"firmware_upgrade",
  "rssi",
  "uptime",
  "pa_latency",
  "memory",
  #"last_seen",
  #"last_modified",
  #"date_created",
  #"channel_state",
  #"channel_flags",
  #"channel_flags_manual",
  #"channel_flags_auto",
  "confidence",
  "confidence_manual",
  "confidence_auto",

  # Environmental fields:
  "humidity",
  "humidity_a",
  "humidity_b",
  "temperature",
  "temperature_a",
  "temperature_b",
  "pressure",
  "pressure_a",
  "pressure_b",

  # Miscellaneous fields:
  "voc",
  "voc_a",
  "voc_b",
  "ozone1",
  "analog_input",

  # PM1.0 fields:
  "pm1.0",
  "pm1.0_a",
  "pm1.0_b",
  "pm1.0_atm",
  "pm1.0_atm_a",
  "pm1.0_atm_b",
  "pm1.0_cf_1",
  "pm1.0_cf_1_a",
  "pm1.0_cf_1_b",

  # PM2.5 fields:
  "pm2.5_alt",
  "pm2.5_alt_a",
  "pm2.5_alt_b",
  "pm2.5",
  "pm2.5_a",
  "pm2.5_b",
  "pm2.5_atm",
  "pm2.5_atm_a",
  "pm2.5_atm_b",
  "pm2.5_cf_1",
  "pm2.5_cf_1_a",
  "pm2.5_cf_1_b",

  # PM2.5 pseudo (simple running) average fields:
  "pm2.5_10minute",
  "pm2.5_10minute_a",
  "pm2.5_10minute_b",
  "pm2.5_30minute",
  "pm2.5_30minute_a",
  "pm2.5_30minute_b",
  "pm2.5_60minute",
  "pm2.5_60minute_a",
  "pm2.5_60minute_b",
  "pm2.5_6hour",
  "pm2.5_6hour_a",
  "pm2.5_6hour_b",
  "pm2.5_24hour",
  "pm2.5_24hour_a",
  "pm2.5_24hour_b",
  "pm2.5_1week",
  "pm2.5_1week_a",
  "pm2.5_1week_b",

  # PM10.0 fields:
  "pm10.0",
  "pm10.0_a",
  "pm10.0_b",
  "pm10.0_atm",
  "pm10.0_atm_a",
  "pm10.0_atm_b",
  "pm10.0_cf_1",
  "pm10.0_cf_1_a",
  "pm10.0_cf_1_b",

  # Visibility fields:
  "scattering_coefficient",
  "scattering_coefficient_a",
  "scattering_coefficient_b",
  "deciviews",
  "deciviews_a",
  "deciviews_b",
  "visual_range",
  "visual_range_a",
  "visual_range_b",

  # Particle count fields:
  "0.3_um_count",
  "0.3_um_count_a",
  "0.3_um_count_b",
  "0.5_um_count",
  "0.5_um_count_a",
  "0.5_um_count_b",
  "1.0_um_count",
  "1.0_um_count_a",
  "1.0_um_count_b",
  "2.5_um_count",
  "2.5_um_count_a",
  "2.5_um_count_b",
  "5.0_um_count",
  "5.0_um_count_a",
  "5.0_um_count_b",
  "10.0_um_count",
  "10.0_um_count_a",
  "10.0_um_count_b"

  # ThingSpeak fields, used to retrieve data from api.thingspeak.com:
  #"primary_id_a",
  #"secondary_id_a",
  #"secondary_key_a",
  #"primary_id_b",
  #"primary_key_b",
  #"secondary_id_b",
  #"secondary_key_b"
)


PurpleAir_POSIXct_Fields <- c(
  "time_stamp",
  "data_time_stamp",
  "start_timestamp",
  "end_timestamp",
  "last_seen",
  "last_modified",
  "date_created"
)


