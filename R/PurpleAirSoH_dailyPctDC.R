#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains
#' 
#' @title Daily DC Signal percentage
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' 
#' @description This function calculates the daily percentage of DC signal 
#' recorded by the \code{pm25_A}, \code{pm25_B}, \code{humidity}, and 
#' \code{temperature} channels. The data are flagged as DC signal when the 
#' standard deviation of an hour of data from each channel equals zero.
#' The number of hours with a DC signal are summed over the day and a daily DC 
#' percentage for each channel is returned. 
#' 
#' 
#' @examples  
#' tbl <- 
#'   example_pat_failure_A %>%
#'   PurpleAirSoH_dailyPctDC() 
#' 
#' timeseriesTbl_multiplot(tbl, ylim = c(0,100), style = "line")

PurpleAirSoH_dailyPctDC <- function(
  pat = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  

  # ----- Create daily tbl -----------------------------------------------------
  
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
  
  # ----- Calculate dailyPctDC -------------------------------------------------
  
  # Begin percent DC calculations:
  pct_DC_tbl <-
    pat$data %>%  
    
    # Group by local time daystamp and calculate the sd over the course of an hour
    dplyr::mutate(hourstamp = strftime(.data$datetime, "%Y%m%d%H", tz = timezone)) %>%
    dplyr::group_by(.data$hourstamp) %>%
    dplyr::summarise_at(
      .vars = c("pm25_A", "pm25_B", "temperature", "humidity"),
      .funs = function(x) { sd(x, na.rm = TRUE) }
    ) %>%
    dplyr::rename(
      pm25_A_sd = .data$pm25_A,
      pm25_B_sd = .data$pm25_B,
      temperature_sd = .data$temperature,
      humidity_sd = .data$humidity
    ) %>%
    
    # Tally the number of time segments for which the standard deviation is 0
    dplyr::mutate(datetime = MazamaCoreUtils::parseDatetime(.data$hourstamp, timezone = timezone)) %>%
    dplyr::group_by(.data$hourstamp) %>%
    dplyr::add_tally(.data$pm25_A_sd==0, name = "pm25_A_DCSignalCount") %>%
    dplyr::add_tally(.data$pm25_B_sd==0, name = "pm25_B_DCSignalCount") %>%
    dplyr::add_tally(.data$humidity_sd==0, name = "humidity_DCSignalCount") %>%
    dplyr::add_tally(.data$temperature_sd==0, name = "temperature_DCSignalCount") %>%
    
    # Add in and group by a daystamp column to sum the tally over the day
    dplyr::mutate(daystamp = strftime(.data$datetime, "%Y%m%d", tz = timezone)) %>%
    dplyr::group_by(.data$daystamp) %>%
    dplyr::summarise_at(.vars = c("pm25_A_DCSignalCount", "pm25_B_DCSignalCount", 
                                  "humidity_DCSignalCount", "temperature_DCSignalCount"), sum) %>%
    
    # Turn the hours of DC per day into a percentage
    dplyr::mutate(pm25_A_pctDC = .data$pm25_A_DCSignalCount/24*100) %>%
    dplyr::mutate(pm25_B_pctDC = .data$pm25_B_DCSignalCount/24*100) %>%
    dplyr::mutate(humidity_pctDC = .data$humidity_DCSignalCount/24*100) %>%
    dplyr::mutate(temperature_pctDC = .data$temperature_DCSignalCount/24*100) %>%
    
    # Add back in the datetime column that was removed during summarizing.
    dplyr::mutate(datetime = MazamaCoreUtils::parseDatetime(.data$daystamp, timezone = timezone)) %>%
    dplyr::select("datetime", contains("pctDC"))
  
  # join with empty daily column to flag missing days
  pct_DC_tbl <- dplyr::left_join(days, pct_DC_tbl, by = "datetime")
  
  # ----- Return ---------------------------------------------------------------
  
  return(pct_DC_tbl)
  
}






