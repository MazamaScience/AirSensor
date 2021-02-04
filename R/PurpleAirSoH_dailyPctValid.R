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
#' recorded measurements that are considered plausible. This metric utilizes the 
#' same bounds as the \code{pat_qc()} function to identify out-of-spec values.
#' 
#' @examples
#' library(AirSensor)
#'   
#' tbl <- 
#'   example_pat_failure_B %>%
#'   PurpleAirSoH_dailyPctValid() 
#' 
#' timeseriesTbl_multiPlot(tbl, ylim = c(0,100))

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
  
  # Create daily tibble based on date range to join with later.
  # This will ensure that missing records from valid_tbl will have NA.
  days <- dplyr::tibble(
    # Special function to handle daylight savings transitions
    datetime = MazamaCoreUtils::dateSequence(start, end - lubridate::ddays(1), timezone = timezone)
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


