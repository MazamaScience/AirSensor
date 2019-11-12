#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains
#' 
#' @title Daily valid percentage
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' 
#' @description The number of valid (\emph{i.e.}, not NA or out-of-spec) sensor 
#' measurements are summed over the course of a calendar day, then divided by the 
#' total number of measurements the sensor actually recorded during that day 
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
  
  # Create hourly tibble based on daterange to join with the baseline_tbl
  # and flag missing data
  #hours <- tibble(datetime = seq(start, end, by = "hour"))
  
  # NOTE:  seq.Date(..., by = "day") operates by repeatedly adding 24 hours
  # NOTE:  which means that when we switch to/from daylight savings we end up
  # NOTE:  no longer on the midnight local time day boundary. Hence the
  # NOTE:  following workaround
  
  datetime <- 
    seq(start, end, by = "day") %>% 
    strftime("%Y%m%d", tz = timezone) %>%
    MazamaCoreUtils::parseDatetime(timezone = timezone)
  
  # Create daily tibble based on date range to join with the valid_tbl.
  # This will ensure that missing records from valid_tbl will have NA.
  days <- dplyr::tibble(
    # Special function to handle daylight savings transitions
    datetime = MazamaCoreUtils::dateSequence(start, end, timezone = timezone)
  )
  
  # ----- Cacluate dailyPctValid -----------------------------------------------
  
  result <- try({ 
    
    # Calculate a baseline tbl that contains the count without removing entries 
    # containing NA or out of spec values
    baseline_tbl <-
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
      dplyr::mutate(datetime = MazamaCoreUtils::parseDatetime(.data$daystamp, timezone = timezone))
    
    # Join with blank table to account for days where no data were recorded
    baseline_tbl <- dplyr::left_join(days, baseline_tbl, by = "datetime") 
    baseline_tbl <- 
      baseline_tbl %>%
      dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0))
    
    # Calculate a tbl after removing entries containing NA and out of spec values
    valid_tbl <-
      pat %>%
      pat_qc()# Remove NA and out of spec
    
    valid_tbl <- 
      valid_tbl$data %>%
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
      dplyr::mutate(datetime = MazamaCoreUtils::parseDatetime(.data$daystamp, timezone = timezone))
    
  }, silent = TRUE)
  
  # If successful, join with days
  if ( ! "try-error" %in% class(result) ) {
    result <- try({
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
      
    }, silent = TRUE)
  }
  
  # Handle either failure
  if ( "try-error" %in% class(result) ) {
    valid_tbl <- 
      days %>%
      dplyr::mutate(pm25_A_pctValid = as.numeric(NA)) %>%
      dplyr::mutate(pm25_B_pctValid = as.numeric(NA)) %>%
      dplyr::mutate(humidity_pctValid = as.numeric(NA)) %>%
      dplyr::mutate(temperature_pctValid = as.numeric(NA))
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(valid_tbl)
  
}


