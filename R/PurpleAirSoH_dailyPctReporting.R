#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains
#' 
#' @title Daily reporting percentage
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param samplingInterval The number of seconds between samples when the sensor 
#' is operating optimally.
#' 
#' @description The number of sensor readings recorded per hour are summed over 
#' the course of a calendar day. This is then divided by the 
#' number of samples the sensor would record in an ideal day 
#' (\code{24 * 3600 / samplingInterval}) to return a percentage of each 
#' day that the sensor is reporting data.
#' 
#' @note Purple Air II sensors reporting after the June, 2019 firmware
#' upgrade report data every 120 seconds. Prior to the upgrade, data were 
#' reported every 80 seconds.
#' 
#' @examples 
#' library(AirSensor)
#' 
#' tbl <- 
#'   example_pat %>%
#'   PurpleAirSoH_dailyPctReporting(80) 
#' 
#' timeseriesTbl_multiPlot(tbl, ylim = c(0,101))

PurpleAirSoH_dailyPctReporting <- function(
  pat = NULL,
  samplingInterval = 120
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  
  if ( !is.numeric(samplingInterval) || is.na(samplingInterval) ) 
    samplingInterval <- 120
  
  # ----- Prepare data ---------------------------------------------------------
  
  # Grab the first day with data and end on the last day with data, partial days
  # will have tapered results. Either provide full days or trim to full days
  # after the fact.
  timezone <- pat$meta$timezone
  localTime <- lubridate::with_tz(pat$dat$datetime, tzone = timezone)
  hour <- lubridate::hour(localTime)
  
  range <- range(localTime)
  start <- lubridate::floor_date(range[1], unit = "day")
  end <- lubridate::ceiling_date(range[2], unit = "day")
  
  # Filter the pat based on the times established above.
  pat <- pat_filterDate(
    pat, 
    startdate = start, 
    # enddate = end + lubridate::ddays(1),
    enddate = end,
    timezone = timezone
  )
  
  # Create daily tibble based on date range to join later.
  # This will ensure that missing records from valid_tbl will have NA.
  days <- dplyr::tibble(
    # Special function to handle daylight savings transitions
    datetime = MazamaCoreUtils::dateSequence(start, end - lubridate::ddays(1), timezone = timezone)
  )
  
  
  # Samples collected per day in an ideal day:
  samplesPerDay <- 24 * 3600 / samplingInterval
  
  # ----- Calculate dailyPctReporting ------------------------------------------
  
  result <- try({
    
    tbl <- 
      pat$data %>%  
      
      # Group by local time daystamp and count all non-NA values
      dplyr::mutate(daystamp = strftime(.data$datetime, "%Y%m%d", tz = timezone)) %>%
      dplyr::group_by(.data$daystamp) %>%
      dplyr::summarise_at(
        .vars = c("pm25_A", "pm25_B", "temperature", "humidity"),
        .funs = function(x) { length(na.omit(x)) }
      ) %>%
      dplyr::rename(
        pm25_A_count = .data$pm25_A,
        pm25_B_count = .data$pm25_B,
        temperature_count = .data$temperature,
        humidity_count = .data$humidity
      ) %>%
      
      # Calculate pctReporting = count/samplesPerDay * 100
      dplyr::mutate(pm25_A_pctReporting =.data$pm25_A_count/samplesPerDay*100) %>%
      dplyr::mutate(pm25_B_pctReporting =.data$pm25_B_count/samplesPerDay*100) %>%
      dplyr::mutate(humidity_pctReporting =.data$humidity_count/samplesPerDay*100) %>%
      dplyr::mutate(temperature_pctReporting =.data$temperature_count/samplesPerDay*100) %>%
      dplyr::mutate(datetime = MazamaCoreUtils::parseDatetime(.data$daystamp, timezone = timezone)) %>%
      dplyr::select("datetime", contains("pctReporting"))
    
  }, silent = TRUE)
  
  # If successful, join with days
  if ( ! "try-error" %in% class(result) ) {
    result <- try({
      tbl <- dplyr::left_join(days, tbl, by = "datetime") 
    }, silent = TRUE)
  }
  
  # Handle either failure
  if ( "try-error" %in% class(result) ) {
    tbl <- 
      days %>%
      dplyr::mutate(pm25_A_pctReporting = as.numeric(NA)) %>%
      dplyr::mutate(pm25_B_pctReporting = as.numeric(NA)) %>%
      dplyr::mutate(humidity_pctReporting = as.numeric(NA)) %>%
      dplyr::mutate(temperature_pctReporting = as.numeric(NA))
  }
  
  # Convert all NA (for any reason) to zero
  tbl <- 
    tbl %>%
    dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
  # ----- Return ---------------------------------------------------------------
  
  return(tbl)
  
}


