#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains
#' 
#' @title Daily valid percentage
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' 
#' @description The number of valid (\emph{i.e.}, not NA or out-of-spec) sensor 
#' measurements are summed over the course of a calendar day (24 
#' hours unless a partial day is included in the data), then divided by the 
#' total number of measurements the sensor actually recorded in during that day 
#' (including NA and out-of-spec values) to return a percentage of the total
#' recorded measurements that are considered plausible.
#' 
#' @examples  
#' tbl <- 
#'   example_pat_failure_B %>%
#'   SoH_dailyPctValid() 
#' 
#' timeseriesTbl_multiplot(tbl, ylim = c(0,100))

SoH_dailyPctValid <- function(
  pat = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  
  
  # ----- SoH_dailyPctValid() ---------------------------------------------------

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
  
  # Filter the pat based on the times established above.
  pat <- pat_filterDate(pat, start, end, timezone = timezone) 
  
  # Begin pctValid calculations:
  # Calculate a baseline tbl that contains the count without removing entries 
  # containing NA or out of spec values
  baseline_tbl <-
    pat %>%
    pat_aggregateOutlierCounts(period = "1 hour") %>%
    dplyr::mutate(daystamp = strftime(.data$datetime, "%Y%m%d", tz = timezone)) %>%
    dplyr::group_by(.data$daystamp) %>%
    dplyr::summarise_at(.vars = c("pm25_A_count", "pm25_B_count", "humidity_count", "temperature_count"),
                                  .funs = sum)
  
  # Calculate a tbl after removing entries containing NA and out of spec values
  valid_tbl <-
    pat %>%
    pat_qc()%>%
    pat_aggregateOutlierCounts(period = "1 hour") %>%
    dplyr::mutate(daystamp = strftime(.data$datetime, "%Y%m%d", tz = timezone)) %>%
    dplyr::group_by(.data$daystamp) %>%
    dplyr::summarise_at(.vars = c("pm25_A_count", "pm25_B_count", "humidity_count", "temperature_count"),
                        .funs = sum) %>%
    # Add columns to the valid tbl to contain valid percentages
    dplyr::mutate(pm25_A_pctValid = 
             .data$pm25_A_count/baseline_tbl$pm25_A_count*100) %>%
    dplyr::mutate(pm25_B_pctValid = 
             .data$pm25_B_count/baseline_tbl$pm25_B_count*100) %>%
    dplyr::mutate(humidity_pctValid = 
             .data$humidity_count/baseline_tbl$humidity_count*100) %>%
    dplyr::mutate(temperature_pctValid = 
                    .data$temperature_count/baseline_tbl$temperature_count*100) %>%
    dplyr::mutate(datetime = MazamaCoreUtils::parseDatetime(.data$daystamp, timezone = timezone)) %>%
    dplyr::select("datetime", contains("Valid"))
  
  
  # ----- Return ---------------------------------------------------------------
  
  return(valid_tbl)
  
}


