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
#' timeseriesTbl_multiplot(tbl, ylim = c(0,101), style = "point")

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
  
  # Get full days in the local timezone
  timezone <- pat$meta$timezone
  localTime <- lubridate::with_tz(pat$dat$datetime, tzone = timezone)
  hour <- lubridate::hour(localTime)
  start <- lubridate::floor_date(localTime[ min(which(hour == 0)) ], unit = "hour")
  end <- lubridate::floor_date(localTime[ max(which(hour == 23)) ], unit = "hour")

  # NOTE:  pat_filterDate only goes to the beginning of enddate and we want it
  # NOTE:  to go to the end of enddate.
  
  # Filter the pat based on the times established above.
  pat <- pat_filterDate(
    pat, 
    startdate = start, 
    enddate = end + lubridate::ddays(1)
  )
 
  # Create daily tibble based on daterange to join with the valid_tbl and 
  # flag missing data 
  datetime <- 
    seq(start, end, by = "day") %>% 
    strftime("%Y%m%d", tz = timezone) %>%
    MazamaCoreUtils::parseDatetime(timezone = timezone)
  days <- tibble(datetime = datetime) 
  
  
  # Samples collected per day in an ideal day:
  samplesPerDay <- 24 * 3600 / samplingInterval
  
  # ----- Calculate dailyPctReporting ------------------------------------------
  
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
  
    tbl <- dplyr::left_join(days, tbl, by = "datetime") 
    tbl <- 
      tbl %>%
      dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0))
    
  # ----- Return ---------------------------------------------------------------
  
  return(tbl)
  
}


