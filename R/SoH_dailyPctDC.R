#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains
#' 
#' @title Daily DC Signal percentage
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param testPeriod The period of time over which to test for a DC signal. 
#' Choose from an hour scale time period or less, i.e., "10 min", "30 min", or 
#' "1 hour". 
#' 
#' @description This function calculates the daily percentage of DC signal 
#' recorded by the \code{pm25_A}, \code{pm25_B}, \code{humidity}, and 
#' \code{temperature} channels. Each day is broken up into individual chunks
#' defined by \code{testPeriod}. The data are flagged as DC signal when the 
#' standard deviation of the data from each channel equals zero over a 
#' \code{testPeriod}. The number of chunks with a DC signal are summed over the 
#' day and a daily DC percentage is returned. 
#' 
#' 
#' @examples  
#' tbl <- 
#'   example_pat_failure_A %>%
#'   SoH_dailyPctDC(testPeriod = "30 min") 
#' 
#' timeseriesTbl_multiplot(tbl, ylim = c(0,100))

SoH_dailyPctDC <- function(
  pat = NULL,
  testPeriod = "30 min"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  
  # ----- Convert period to seconds --------------------------------------------
  
  periodParts <- strsplit(testPeriod, " ", fixed = TRUE)[[1]]
  
  if ( length(periodParts) == 1 ) {
    periodCount <- 1
    units <- periodParts[1]
  } else {
    periodCount <- as.numeric(periodParts[1])
    units <- periodParts[2]
  }
  
  if ( units == "sec"     ) unitSecs <- 1
  if ( units == "min"     ) unitSecs <- 60
  if ( units == "hour"    ) unitSecs <- 3600
  if ( units == "day"     ) unitSecs <- 3600 * 24
  if ( units == "week"    ) unitSecs <- 3600 * 24 * 7
  if ( units == "month"   ) unitSecs <- 3600 * 24 * 31
  if ( units == "quarter" ) unitSecs <- 3600 * 24 * 31 * 3
  if ( units == "year"    ) unitSecs <- 3600 * 8784 
  
  periodSeconds <- periodCount * unitSecs 
  
  hourFactor <- 3600 / periodSeconds
  
  # ----- Calculate pct_DC -----------------------------------------------------
  
  timezone <- pat$meta$timezone
  
  # Note: after initial completion of this function, decided to chop the passed 
  # in pat objects by full days. First convert the datetime column in the pat to
  # local time, then filter based on the first and last full day in the local
  # timezone
  
  pat$data$datetime <- lubridate::with_tz(pat$data$datetime, 
                                          tzone = timezone)
  
  # Parse the hours in datetime to find the first and last full days
  hour <- lubridate::hour(pat$data$datetime)
  start <- pat$data$datetime[ min(which(hour == 0)) ]
  end <- pat$data$datetime[ max(which(hour == 23)) ]
  
  # Add create hourly tibble based on daterange to join later and flag missing data
  days <- tibble(datetime = seq(start, end, by = "day")) 
  days$datetime <- lubridate::as_date(days$datetime)
  days$datetime <- MazamaCoreUtils::parseDatetime(days$datetime, timezone = timezone)
  
  # Filter the pat based on the times established above.
  pat <- pat_filterDate(pat, start, end, timezone = timezone) 
  
  # Begin percent DC calculations:
  pct_DC_tbl <-
    pat %>%
    pat_aggregate(period = testPeriod) %>%
    
    # Group by day so that the DC time segments can be summed over the day even
    # if the aggregation period is less than one day. Each day must be a
    # complete day to avoid tapering when dividing by 24 hours later on.
    dplyr::mutate(daystamp = strftime(.data$datetime, "%Y%m%d", tz = timezone)) %>%
    dplyr::group_by(.data$daystamp) %>% 
    
    # Tally the number of time segments for which the standard deviation is 0
    dplyr::add_tally(.data$pm25_A_sd==0, name = "pm25_A_DCSignalCount") %>%
    dplyr::add_tally(.data$pm25_B_sd==0, name = "pm25_B_DCSignalCount") %>%
    dplyr::add_tally(.data$humidity_sd==0, name = "pm25_B_humidity") %>%
    dplyr::add_tally(.data$temperature_sd==0, name = "pm25_B_temperature") %>%
    
    # Summarize each tally channel. Since, it's a tally per day, take the max
    # each day rather than sum.
    dplyr::summarise_at(.vars = c("pm25_A_DCSignalCount", "pm25_B_DCSignalCount", 
                                  "pm25_B_temperature", "pm25_B_humidity"),max) %>%
    
    # Turn the DC signal time segments into hours per day 
    dplyr::mutate(pm25_A_DCHourCount = .data$pm25_A_DCSignalCount/hourFactor) %>%
    dplyr::mutate(pm25_B_DCHourCount = .data$pm25_B_DCSignalCount/hourFactor ) %>%
    dplyr::mutate(humidity_DCHourCount = .data$pm25_B_humidity/hourFactor ) %>%
    dplyr::mutate(temperature_DCHourCount = .data$pm25_B_temperature/hourFactor ) %>%
    
    # Turn the hours of DC per day into a percentage
    dplyr::mutate(pm25_A_pctDC = .data$pm25_A_DCHourCount/24*100) %>%
    dplyr::mutate(pm25_B_pctDC = .data$pm25_B_DCHourCount/24*100) %>%
    dplyr::mutate(humidity_pctDC = .data$humidity_DCHourCount/24*100) %>%
    dplyr::mutate(temperature_pctDC = .data$temperature_DCHourCount/24*100) %>%
    
    # Add back in the datetime column that was removed during summarizing.
    dplyr::mutate(datetime = MazamaCoreUtils::parseDatetime(.data$daystamp, timezone = timezone)) %>%
    dplyr::select("datetime", contains("pctDC"))
  
  # join with empty daily column to flag missing days
  pct_DC_tbl <- dplyr::left_join(days, pct_DC_tbl, by = "datetime")
  
  # ----- Return ---------------------------------------------------------------
  
  return(pct_DC_tbl)
  
}






