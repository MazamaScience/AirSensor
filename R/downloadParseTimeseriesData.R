#' @export
#'
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.debug logger.error
#'
#' @title Download Purple Air timeseries data
#'
#' @param pas Purple Air 'enhanced' synoptic data
#' @param name Purple Air 'label'
#' @param id Purple Air 'ID'
#' @param startdate desired start datetime (ISO 8601)
#' @param enddate desired end datetime (ISO 8601)
#' @param baseURL Base URL for Thingspeak API
#' @return The monitor time series given broken up into metadata and readings data
#' @description Timeseries data from a specific PurpleAir can be retrieved from the Thingspeak API .
#'

downloadParseTimeseriesData <- function(
  pas = NULL,
  name = NULL,
  id = NULL,
  startdate = NULL,
  enddate = NULL,
  baseURL = "https://api.thingspeak.com/channels/"
) {
  
  # Default to the most recent week of data
  if ( is.null(startdate) || is.null(enddate)) {
    enddate <- lubridate::floor_date(lubridate::now("UTC"), unit = "minute")
    startdate <- lubridate::floor_date(enddate - lubridate::ddays(7))
  } else {
    startdate <- 
      lubridate::parse_date_time(
        startdate,
        c("ymd", "ymd_H", "ymd_HM", "ymd_HMS"),
        tz = "UTC"
      )
    enddate <- 
      lubridate::parse_date_time(
        enddate,
        c("ymd", "ymd_H", "ymd_HM", "ymd_HMS"),
        tz = "UTC"
      )
  }
  
  startString <- strftime(startdate, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  endString <- strftime(enddate, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  
  # Prefer to use the monitor's name over it's ID
  if ( !is.null(name) ) {
    requested_meta <- dplyr::filter(pas, .data$label == name)
  } else if ( !is.null(id) ) {
    requested_meta <- dplyr::filter(pas, .data$ID == id)
  }
  
  # Determine which channel was given and access the other channel from it
  if ( is.na(requested_meta$parentID) ) {
    A_meta <- requested_meta
    B_meta <- dplyr::filter(pas, .data$parentID == A_meta$ID)
  } else {
    B_meta <- requested_meta
    A_meta <- dplyr::filter(pas, .data$ID == B_meta$parentID)
  }
  
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
  
  # print(webserviceUrl)
  
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
    if ( httr::status_code(r) == 500 ) {
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
    
    print(paste0("Channel A: ", err_msg, " === Returning empty PAS object ==="))
    
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
  
  if ( httr::http_error(r)) { # web service failed to respond
    
    # https://digitalocean.com/community/tutorials/how-to-troubleshoot-common-http-error-codes
    if ( httr::status_code(r) == 500 ) {
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
    
    print(paste0("Channel B: ", err_msg, " === Returning empty PAS object ==="))
    
    B_list <- err_list
    B_data <- err_data
  } else { # Response successful
    
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
  
  # Sanity check for data
  if ( length(A_data) == 0 && length(B_data) == 0 ) {
    stop("No data returned for A or B channels for the requested time period.")
  } else if ( length(A_data) == 0) {
    stop("No data returned for A channel for the requested time period.")
  } else if ( length(B_data) == 0) {
    stop("No data returned for B channel for the requested time period.")
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
  data$datetime <- lubridate::ymd_hms(data$datetime)
  data$entry_id <- as.character(data$entry_id)
  for (name in numeric_columns) {
    data[[name]] <- as.numeric(data[[name]])
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
  pat <- list(meta = meta, data = data)
  
  return(pat)
}

# TODO:  Probably have an internal function to create an empty "pat" object.
# TODO:  This can then be returned immediately whenever a "no data" response
# TODO:  is detected when requesting data.

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  pas_raw <- downloadParseSynopticData()
  pas <- enhanceSynopticData(pas_raw)
  name <- "MV Clean Air Ambassador @ Winthrop Library"
  pat_raw <- downloadParseTimeseriesData(pas, name)
  pat <- createPATimeseriesObject(pat_raw)
  
}
