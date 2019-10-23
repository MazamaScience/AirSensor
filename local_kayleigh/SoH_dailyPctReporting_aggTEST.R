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
#' tbl <- 
#'   example_pat %>%
#'   SoH_dailyPctReporting(80) 
#' 
#' timeseriesTbl_multiplot(tbl)

SoH_dailyPctReporting <- function(
  pat = NULL,
  samplingInterval = 120
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  
  
  # ----- SoH_dailyPctReporting() ---------------------------------------------------
  
  timezone <- pat$meta$timezone
  
  # # The following lines of code were used in other SoH functions successfully 
  # # but did not work in this case, I think because of using pat_aggregationOutlierCounts
  # # on a day basis. 
  # # Note: after initial completion of this function, decided to chop the passed
  # # in pat objects by full days. First convert the datetime column in the pat to
  # # local time, then filter based on the first and last full day in the local
  # # timezone. 

  pat$data$datetime <- lubridate::with_tz(pat$data$datetime,
                                          tzone = timezone)
  # Parse the hours in datetime to find the first and last full days
  hour <- lubridate::hour(pat$data$datetime)
  start <- pat$data$datetime[ min(which(hour == 0)) ]
  end <- pat$data$datetime[ max(which(hour == 23)) ]

  # Filter the pat based on the times established above.
  pat <- pat_filterDate(pat, start, end, timezone = timezone)
  
  # Begin daily reporting percentage calculations:
  samplesPerDay <- 24 * 3600 / samplingInterval
  
  tbl <- 
    pat %>%
    # Calculate the aggregation statistics based on a day rather than hourly.
    pat_aggregateOutlierCounts(period = "1 hour") %>%
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


