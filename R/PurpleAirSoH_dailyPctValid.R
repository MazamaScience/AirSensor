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
#'   PurpleAirSoH_dailyPctValid() 
#' 
#' timeseriesTbl_multiplot(tbl, ylim = c(0,100), style = "line")

PurpleAirSoH_dailyPctValid <- function(
  pat = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  
  
  # ----- Create aggregation tbl -----------------------------------------------
  
  # Get full days in the local timezone
  timezone <- pat$meta$timezone
  localTime <- lubridate::with_tz(pat$dat$datetime, tzone = timezone)
  hour <- lubridate::hour(localTime)
  start <- localTime[ min(which(hour == 0)) ]
  end <- localTime[ max(which(hour == 23)) ]
  
  # NOTE:  pat_filterDate only goes to the beginning of enddate and we want it
  # NOTE:  to go to the end of enddate.
  
  # Filter the pat based on the times established above.
  pat <- pat_filterDate(
    pat, 
    startdate = start, 
    enddate = end + lubridate::ddays(1)
  )
  
  # Create hourly tibble based on daterange to join with the baseline_tbl
  # and flag missing data
  hours <- tibble(datetime = seq(start, end, by = "hour"))
  
  # NOTE:  seq.Date(..., by = "day") operates by repeatedly adding 24 hours
  # NOTE:  which means that when we switch to/from daylight savings we end up
  # NOTE:  no longer on the midnight local time day boundary. Hence the
  # NOTE:  following workaround
  
  datetime <- 
    seq(start, end, by = "day") %>% 
    strftime("%Y%m%d", tz = timezone) %>%
    MazamaCoreUtils::parseDatetime(timezone = timezone)
  
  # Create daily tibble based on daterange to join with the valid_tbl and 
  # flag missing data
  days <- tibble(datetime = datetime) 
  
  # ----- Cacluate dailyPctValid -----------------------------------------------
  
  # Calculate a baseline tbl that contains the count without removing entries 
  # containing NA or out of spec values
  baseline_tbl <-
    pat %>%
    pat_aggregateOutlierCounts(period = "1 hour") 
  
  # Put it on a local time axis and trim
  baseline_tbl$datetime <- lubridate::with_tz(baseline_tbl$datetime, tzone = timezone)
  baseline_tbl <- dplyr::filter(baseline_tbl, .data$datetime >= start & .data$datetime <= end)
  
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
  
  # add datetime and remove daystamp
  baseline_tbl$datetime <- MazamaCoreUtils::parseDatetime(baseline_tbl$daystamp, timezone = timezone)
  baseline_tbl$daystamp <- NULL

  # Calculate a tbl after removing entries containing NA and out of spec values
  valid_tbl <-
    pat %>%
    pat_qc()%>% # Remove NA and out of spec
    pat_aggregateOutlierCounts(period = "1 hour")
  
  # Put it on a local time axis and trim
  valid_tbl$datetime <- lubridate::with_tz(valid_tbl$datetime, tzone = timezone)
  valid_tbl <- dplyr::filter(valid_tbl, .data$datetime >= start & .data$datetime <= end)
  
  valid_tbl <-
    valid_tbl %>%
    # additional daily aggregation
    dplyr::mutate(daystamp = strftime(.data$datetime, "%Y%m%d", tz = timezone)) %>%
    dplyr::group_by(.data$daystamp) %>%
    dplyr::summarise_at(.vars = c("pm25_A_count", "pm25_B_count", "humidity_count", "temperature_count"),
                        .funs = sum)
    
  # add datetime and remove daystamp
  valid_tbl$datetime <- MazamaCoreUtils::parseDatetime(valid_tbl$daystamp, timezone = timezone)
  valid_tbl$daystamp <- NULL
  
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
    dplyr::select("datetime", contains("Valid")) %>%
    dplyr::mutate_all(function(x) { replace(x, is.infinite(x), NA) })
  
  # ----- Return ---------------------------------------------------------------
  
  return(valid_tbl)
  
}


