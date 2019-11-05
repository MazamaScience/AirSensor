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
#' the course of a calendar day (24 hours unless the \code{pat} datetime column
#' includes a partial day at the beginning or end). This is then divided by the 
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
#'   PurpleAirSoH_dailyPctReporting_OLD(80) 
#' 
#' timeseriesTbl_multiplot(tbl, ylim = c(0,101), style = "point")

PurpleAirSoH_dailyPctReporting_OLD <- function(
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
  
  # ----- Create aggregation tbl -----------------------------------------------
  
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
  
  # Samples collected per day in an ideal day:
  samplesPerDay <- 24 * 3600 / samplingInterval
  
  # Create hourly tibble based on daterange to flag missing data
  hours <- tibble(datetime = seq(start, end, by = "hour"))
  
  # Calculate hourly aggregation statistics
  tbl <- 
    pat %>%
    pat_aggregateOutlierCounts(period = "1 hour")
  
  # Put them on a local time axis and trim
  tbl$datetime <- lubridate::with_tz(tbl$datetime, tzone = timezone)
  tbl <- dplyr::filter(tbl, .data$datetime >= start & .data$datetime <= end)
    
  # Join the hours-only tbl with the data tbl. This will input NA's in the tbl
  # on days when data were not recorded.
  tbl <- dplyr::left_join(hours, tbl, by = "datetime")
  
  # ----- Calculate dailyPctReporting_OLD ------------------------------------------
  
  # Change all the NA values to zero in this function since "zero" counts
  # means the channel is not reporting. 
  tbl$pm25_A_count[is.na(tbl$pm25_A_count)] <- 0
  tbl$pm25_B_count[is.na(tbl$pm25_B_count)] <- 0
  tbl$humidity_count[is.na(tbl$humidity_count)] <- 0
  tbl$temperature_count[is.na(tbl$temperature_count)] <- 0
  
  # additional aggregation using dyplyr as mentioned in the notes above.
  tbl <-
    tbl %>%  
    dplyr::mutate(daystamp = strftime(.data$datetime, "%Y%m%d", tz = timezone)) %>%
    dplyr::group_by(.data$daystamp) %>%
    dplyr::summarise_at(.vars = c("pm25_A_count", "pm25_B_count", "humidity_count", "temperature_count"),
                        .funs = sum) %>%
    # Divide the total count per day by the number of samples in a day where the
    # sensor was working perfectly (30 samp/hr * 24 hr/day)*100 to make percent.
    dplyr::mutate(pm25_A_pctReporting =.data$pm25_A_count/samplesPerDay*100) %>%
    dplyr::mutate(pm25_B_pctReporting =.data$pm25_B_count/samplesPerDay*100) %>%
    dplyr::mutate(humidity_pctReporting =.data$humidity_count/samplesPerDay*100) %>%
    dplyr::mutate(temperature_pctReporting =.data$temperature_count/samplesPerDay*100) %>%
    dplyr::mutate(datetime = MazamaCoreUtils::parseDatetime(.data$daystamp, timezone = timezone)) %>%
    dplyr::select("datetime", contains("pctReporting"))
  
  
  # ----- Return ---------------------------------------------------------------
  
  return(tbl)
  
}


