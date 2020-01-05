#' @export
#'
#' @importFrom rlang .data
#'
#' @title Download PurpleAir timeseries data
#'
#' @param id PurpleAir sensor 'deviceDeploymentID'.
#' @param label PurpleAir sensor 'label'.
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param startdate Desired start time (ISO 8601).
#' @param enddate Desired end time (ISO 8601).
#' @param timezone Timezone used to interpret start and end dates.
#' @param baseURL Base URL for Thingspeak API.
#' 
#' @return List of type \code{pa_timeseries} containing \code{meta} and 
#' \code{data} elements with timeseries metadata and data, respectively.
#' 
#' @description Downloads timeseries data for a specific PurpleAir sensor 
#' from the ThingSpeak API and parses the content into a dataframe.
#'
#' @references https://www2.purpleair.com/community/faq

downloadParseTimeseriesData <- function(
  id = NULL,
  label = NULL,
  pas = NULL,
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  baseURL = "https://api.thingspeak.com/channels/"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(baseURL)
  
  # Get the deviceDeploymentID
  if ( is.null(id) && is.null(label) ) {
    
    stop(paste0("label or id must be provided"))
    
  } else if ( is.null(id) && !is.null(label) ) {
    
    if ( is.null(pas) )
      stop(paste0("pas must be provided when loading by label"))
    
    if ( !label %in% pas$label )
      stop(sprintf("label '%s' is not found in the 'pas' object", label))
    
    # Get the deviceDeploymentID from the label
    pattern <- paste0("^", label, "$")
    deviceDeploymentID <- pas_getDeviceDeploymentIDs(pas, pattern = pattern)
    
    if ( length(deviceDeploymentID) > 1 )
      stop(sprintf("label '%s' matches more than one sensor", label))
    
  } else {
    
    # Use id whenever it is defined, potentially ignoring label
    deviceDeploymentID <- id
    
  }
  
  # ----- Determine date sequence ----------------------------------------------
  
  # Find a single, parent record
  pas_single <-
    pas %>%
    dplyr::filter(is.na(.data$parentID)) %>%
    dplyr::filter(.data$deviceDeploymentID == !!deviceDeploymentID)
  
  if ( nrow(pas_single) > 1 ) {
    stop(paste0("Multilpe sensors share deviceDeploymentID: ",
                deviceDeploymentID, "'"))
  } 
  
  # Get the timezone associated with this sensor
  if ( is.null(timezone) ) {
    timezone <-
      pas_single %>%
      dplyr::pull(.data$timezone)
  }
  
  # Create a valid dateRange
  if ( !is.null(startdate) && !is.null(enddate) ) {
    # Don't require day boundaries
    dateRange <- MazamaCoreUtils::timeRange(startdate, 
                                            enddate, 
                                            timezone = timezone)
  } else {
    # Default to 7 days with day boundaries
    dateRange <- MazamaCoreUtils::dateRange(startdate, 
                                            enddate, 
                                            timezone, 
                                            days = 7,
                                            unit = "min")
  }
  
  startString <- strftime(dateRange[1], "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  endString <- strftime(dateRange[2], "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  
  # Determine which channel was given and access the other channel from it
  if ( is.na(pas_single$parentID) ) {
    A_meta <- pas_single
    B_meta <- dplyr::filter(pas, .data$parentID == A_meta$ID)
  } else {
    B_meta <- pas_single
    A_meta <- dplyr::filter(pas, .data$ID == B_meta$parentID)
  }
  
  # Get identifiers from the A channel
  sensorID <- A_meta$ID
  sensorLabel <- A_meta$label
  
  # Combine channel A and B monitor metadata
  meta <- dplyr::bind_rows(A_meta, B_meta)
  
  # Generate Thingspeak request URLs
  A_url <- 
    paste0(
      baseURL,
      A_meta$THINGSPEAK_PRIMARY_ID,
      "/feeds.json?api_key=",
      A_meta$THINGSPEAK_PRIMARY_ID_READ_KEY,
      "&start=", startString,
      "&end=",
      endString
    )
  B_url <-
    paste0(
      baseURL,
      B_meta$THINGSPEAK_PRIMARY_ID,
      "/feeds.json?api_key=",
      B_meta$THINGSPEAK_PRIMARY_ID_READ_KEY,
      "&start=", startString,
      "&end=",
      endString
    )
  
  # ----- Request A channel data from Thingspeak -------------------------------
  
  webserviceUrl <- A_url
  
  # message(webserviceUrl)
  
  # NOTE:  using Hadley Wickham style:
  # NOTE:  https://github.com/hadley/httr/blob/master/vignettes/quickstart.Rmd
  r <- httr::GET(webserviceUrl)
  
  # Handle the response
  status_code <- httr::status_code(r)
  content <- httr::content(r, as = "text") # don't interpret the JSON
  
  # Handle "no data" response by generating an empty but complete "pat" object
  err_JSON <- ('{
       "channel": {
        "id": 0,
        "name": "NA",
        "latitude": "0.0",
        "longitude": "0.0",
        "field1": "PM1.0 (ATM)",
        "field2": "PM2.5 (ATM)",
        "field3": "PM10.0 (ATM)",
        "field4": "Uptime",
        "field5": "RSSI",
        "field6": "Temperature",
        "field7": "Humidity",
        "field8": "PM2.5 (CF=1)",
        "created_at": "2000-01-01T12:00:00Z",
        "updated_at": "2000-01-01T12:00:00Z",
        "last_entry_id": 0
      },
      "feeds": [
        {
          "created_at": "2000-01-01T12:00:00Z",
          "entry_id": 0,
          "field1": "NA",
          "field2": "NA",
          "field3": "NA",
          "field4": "NA",
          "field5": "NA",
          "field6": "NA",
          "field7": "NA",
          "field8": "NA"
        }
        ]
      }')
  
  err_list <- jsonlite::fromJSON(
    txt = err_JSON,
    simplifyVector = TRUE,
    simplifyDataFrame = TRUE,
    simplifyMatrix = TRUE,
    flatten = FALSE
  ) 
  
  err_data <- err_list$feeds
  
  if ( httr::http_error(r) ) { # web service failed to respond
    
    # https://digitalocean.com/community/tutorials/how-to-troubleshoot-common-http-error-codes
    if ( httr::status_code(r) == 429 ) {
      err_msg <- paste0(
        "web service error 429: Too Many Requests from ",
        webserviceUrl
      )
    } else if ( httr::status_code(r) == 500 ) {
      err_msg <- paste0(
        "web service error 500: Internal Server Error from ",
        webserviceUrl
      )
    } else if ( httr::status_code(r) == 502 ) {
      err_msg <- paste0(
        "web service error 502: Bad Gateway from ",
        webserviceUrl
      )
    } else if ( httr::status_code(r) == 503 ) {
      err_msg <- paste0(
        "web service error 503: Service Unavailable from ",
        webserviceUrl
      )
    } else if ( httr::status_code(r) == 504 ) {
      err_msg <- paste0(
        "web service error 504: Gateway Timeout from ",
        webserviceUrl
      )
    } else {
      err_msg <- paste0(
        "web service error ", httr::status_code(r), " from ",
        webserviceUrl
      )
    }
    
    message(paste0("Channel A: ", err_msg, " === Returning empty A channel ==="))
    
    A_list <- err_list
    A_data <- err_data
    
  } else { # Response successful
    
    A_list <- 
      jsonlite::fromJSON(
        content,
        simplifyVector = TRUE,
        simplifyDataFrame = TRUE,
        simplifyMatrix = TRUE,
        flatten = FALSE
      )
    A_data <- A_list$feeds
    
  }
  
  # ----- Request B channel data from Thingspeak -------------------------------
  
  webserviceUrl <- B_url
  
  # NOTE:  using Hadley Wickham style:
  # NOTE:  https://github.com/hadley/httr/blob/master/vignettes/quickstart.Rmd
  r <- httr::GET(webserviceUrl)
  
  # Handle the response
  status_code <- httr::status_code(r)
  content <- httr::content(r, as = "text") # don't interpret the JSONw
  
  if ( httr::http_error(r) ) { # web service failed to respond
    
    # https://digitalocean.com/community/tutorials/how-to-troubleshoot-common-http-error-codes
    if ( httr::status_code(r) == 429 ) {
      err_msg <- paste0(
        "web service error 429: Too Many Requests from ",
        webserviceUrl
      )
    } else if ( httr::status_code(r) == 500 ) {
      err_msg <- paste0(
        "web service error 500: Internal Server Error from ",
        webserviceUrl
      )
    } else if ( httr::status_code(r) == 502 ) {
      err_msg <- paste0(
        "web service error 502: Bad Gateway from ",
        webserviceUrl
      )
    } else if ( httr::status_code(r) == 503 ) {
      err_msg <- paste0(
        "web service error 503: Service Unavailable from ",
        webserviceUrl
      )
    } else if ( httr::status_code(r) == 504 ) {
      err_msg <- paste0(
        "web service error 504: Gateway Timeout from ",
        webserviceUrl
      )
    } else {
      err_msg <- paste0(
        "web service error ", httr::status_code(r), " from ",
        webserviceUrl
      )
    }
    
    message(paste0("Channel B: ", err_msg, " === Returning empty B channel ==="))
    
    B_list <- err_list
    B_data <- err_data
    
  } else {
    
    # Response successful
    
    B_list <- 
      jsonlite::fromJSON(
        content,
        simplifyVector = TRUE,
        simplifyDataFrame = TRUE,
        simplifyMatrix = TRUE,
        flatten = FALSE
      )
    B_data <- B_list$feeds
    
  }
  
  # Sanity check for data -> fill if empty to avoid error DL 
  if ( ncol(A_data) == 0 && ncol(B_data) == 0 ) {
    A_data <- err_data
    B_data <- err_data
    warning(sprintf(
      "Sensor %s -- %s: A & B channels for the requested time period do not exist.",
      sensorID, sensorLabel
    ))
  } else if ( ncol(A_data) == 0) {
    A_data <- err_data
    warning(sprintf(
      "Sensor %s -- %s: A channel for the requested time period does not exist.",
      sensorID, sensorLabel
    ))
  } else if ( ncol(B_data) == 0) {
    B_data <- err_data
    warning(sprintf(
      "Sensor %s -- %s: B channel for the requested time period does not exist",
      sensorID, sensorLabel
    ))
  }
  
  # Rename columns
  names(A_data) <- c(
    "datetime", "entry_id", "pm1_atm", "pm2.5_atm", "pm10_atm",
    "uptime", "rssi", "temperature", "humidity", "pm2.5_cf1"
  )
  names(B_data) <- c(
    "datetime", "entry_id", "pm1_atm", "pm2.5_atm", "pm10_atm",
    "memory", "adc0", "unused1", "unused2", "pm2.5_cf1"
  )
  
  # Add channel identifier
  A_data$channel <- "A"
  B_data$channel <- "B"
  
  # Drop unused columns
  B_data <- dplyr::select(B_data, -.data$unused1, -.data$unused2)
  
  # Combine data from both channels
  data <-
    dplyr::bind_rows(A_data, B_data) %>%
    dplyr::arrange(.data$datetime)
  
  # NOTE:  > names(data)
  # NOTE:  [1] "datetime"    "entry_id"    "pm1_atm"     "pm2.5_atm"   "pm10_atm"    "uptime"      "rssi"
  # NOTE:  [8] "temperature" "humidity"    "pm2.5_cf1"   "channel"     "memory"      "adc0"
  
  numeric_columns <- c(
    "pm1_atm", "pm2.5_atm", "pm10_atm", "uptime", "rssi",
    "temperature", "humidity", "pm2.5_cf1", "memory", "adc0"
  )
  
  # Convert to proper types
  data$datetime <- lubridate::ymd_hms(data$datetime, tz = "UTC")
  data$entry_id <- as.character(data$entry_id)
  for (columnName in numeric_columns) {
    data[[columnName]] <- as.numeric(data[[columnName]])
  }
  
  # Round values to reflect resolution as specified in
  # https://www.purpleair.com/sensors
  
  # NOTE:  Rounding breaks outlier detection
  # data$pm1_atm <- round(data$pm1_atm)
  # data$pm1_atm <- round(data$pm1_atm)
  # data$pm2.5_atm <- round(data$pm2.5_atm)
  # data$pm10_atm <- round(data$pm10_atm)
  # data$temperature <- round(data$temperature)
  # data$humidity <- round(data$humidity)
  # data$pm2.5_cf1 <- round(data$pm2.5_cf1)
  
  # Combine meta and data dataframes into a list
  pat_raw <- list(meta = meta, data = data)
  
  # Remove any duplicate data records
  pat_raw$data <- dplyr::distinct(pat_raw$data)
  
  # ----- Return ---------------------------------------------------------------
  
  return(pat_raw)
  
}

# TODO:  Probably have an internal function to create an empty "pat" object.
# TODO:  This can then be returned immediately whenever a "no data" response
# TODO:  is detected when requesting data.

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  # id <- NULL
  # label <- "Seattle"
  # pas <- example_pas
  id <- "ebcb53584e44bb6f_3218"
  label <- NULL
  pas <- example_pas
  startdate <- "2018-08-01"
  enddate <- "2018-08-28"
  timezone <- NULL
  baseURL <- "https://api.thingspeak.com/channels/"
  
}
