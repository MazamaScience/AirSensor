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
  
  # Notes:
  # # Ideally, we would aggregate over a daily basis up front. This did not work
  # # in this case because using pat_aggregationOutlierCounts on a day basis 
  # # poses issues with timezones. As a work around, I reduced the aggregation 
  # # period of pat_aggregationOutlierCounts to 1 hour and did additional 
  # # aggregation using dplyr.
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
  
  # Create hourly tibble based on daterange to join with the baseline_tbl
  # and flag missing data
  hours <- tibble(datetime = seq(start, end, by = "hour"))
  
  # Create daily tibble based on daterange to join with the valid_tbl and 
  # flag missing data
  days <- tibble(datetime = seq(start, end, by = "day")) 
  days$datetime <- lubridate::as_date(days$datetime)
  days$datetime <- MazamaCoreUtils::parseDatetime(days$datetime, timezone = timezone)

  # Begin pctValid calculations:
  # Calculate a baseline tbl that contains the count without removing entries 
  # containing NA or out of spec values
  baseline_tbl <-
    pat %>%
    pat_aggregateOutlierCounts(period = "1 hour") 
  
  # Must break the pipeline because the order of tibble arguments in left_join 
  # matters. This will add NA values to hours (rows) where data wasn't recorded
  baseline_tbl <- dplyr::left_join(hours, baseline_tbl, by = "datetime")
  
  # Change all the "NA" values to zero since zero counts means the channel is 
  # not reporting. 
  baseline_tbl$pm25_A_count[is.na(baseline_tbl$pm25_A_count)] <- 0
  baseline_tbl$pm25_B_count[is.na(baseline_tbl$pm25_B_count)] <- 0
  baseline_tbl$humidity_count[is.na(baseline_tbl$humidity_count)] <- 0
  baseline_tbl$temperature_count[is.na(baseline_tbl$temperature_count)] <- 0
  
  # Additional aggregation using dplyr as mentioned in the notes above. This will
  # result in daily sums of the counts for each variable of interest.
  baseline_tbl <-
    baseline_tbl %>%  
    dplyr::mutate(daystamp = strftime(.data$datetime, "%Y%m%d", tz = timezone)) %>%
    dplyr::group_by(.data$daystamp) %>%
    dplyr::summarise_at(.vars = c("pm25_A_count", "pm25_B_count", "humidity_count", "temperature_count"),
                        .funs = sum)
  
  # Calculate a tbl after removing entries containing NA and out of spec values
  valid_tbl <-
    pat %>%
    pat_qc()%>% # Remove NA and out of spec
    pat_aggregateOutlierCounts(period = "1 hour") %>%
    # additional daily aggregation
    dplyr::mutate(daystamp = strftime(.data$datetime, "%Y%m%d", tz = timezone)) %>%
    dplyr::group_by(.data$daystamp) %>%
    dplyr::summarise_at(.vars = c("pm25_A_count", "pm25_B_count", "humidity_count", "temperature_count"),
                        .funs = sum) %>%
    # add in a datetime column to join with a daily column to flag missing data
    dplyr::mutate(datetime = MazamaCoreUtils::parseDatetime(.data$daystamp, timezone = timezone)) %>%
    dplyr::select("datetime", contains("count"))
  
  # join with empty daily column to flag missing days
  valid_tbl <- dplyr::left_join(days, valid_tbl, by = "datetime")
  
  # Add columns to the valid tbl to contain valid percentages
  valid_tbl <-
    valid_tbl%>%
    dplyr::mutate(pm25_A_pctValid = 
                    .data$pm25_A_count/baseline_tbl$pm25_A_count*100) %>%
    dplyr::mutate(pm25_B_pctValid = 
                    .data$pm25_B_count/baseline_tbl$pm25_B_count*100) %>%
    dplyr::mutate(humidity_pctValid = 
                    .data$humidity_count/baseline_tbl$humidity_count*100) %>%
    dplyr::mutate(temperature_pctValid = 
                    .data$temperature_count/baseline_tbl$temperature_count*100) %>%
    #dplyr::mutate(datetime = MazamaCoreUtils::parseDatetime(.data$daystamp, timezone = timezone)) %>%
    dplyr::select("datetime", contains("Valid"))
  
  # Replace inf with NA 
  is.na(valid_tbl)<-  do.call(cbind,lapply(valid_tbl, is.infinite))
  
  # ----- Return ---------------------------------------------------------------
  
  return(valid_tbl)
  
}


