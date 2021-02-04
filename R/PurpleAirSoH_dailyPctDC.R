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
#' This metric allows users to identify “sticky values”, or instances of a sensor 
#' continuously logging the same value. A high percent DC value indicates the 
#' likely occurrence of a “sticky value”, and a zero or low percent DC indicates 
#' that the sensor is recording dynamic data.
#' 
#' @examples  
#' library(AirSensor)
#' 
#' tbl <- 
#'   example_pat_failure_A %>%
#'   PurpleAirSoH_dailyPctDC() 
#' 
#' timeseriesTbl_multiPlot(tbl, ylim = c(0,100))

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
  
  # Create daily tibble based on date range to join later.
  # This will ensure that missing records from valid_tbl will have NA.
  days <- dplyr::tibble(
    # Special function to handle daylight savings transitions
    datetime = MazamaCoreUtils::dateSequence(start, end - lubridate::ddays(1), timezone = timezone)
  )
  
  
  # ----- Calculate dailyPctDC -------------------------------------------------
  
  result <- try({
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
    
  }, silent = TRUE)
  
  # If successful:
  if ( ! "try-error" %in% class(result) ) {
    result <- try({
      # join with empty daily column to flag missing days
      pct_DC_tbl <- dplyr::left_join(days, pct_DC_tbl, by = "datetime")
    }, silent = TRUE)
  }
  
  # Handle either failure
  if ( "try-error" %in% class(result) ) {
    pct_DC_tbl <- 
      days %>%
      dplyr::mutate(pm25_A_pctDC = as.numeric(NA)) %>%
      dplyr::mutate(pm25_B_pctDC = as.numeric(NA)) %>%
      dplyr::mutate(humidity_pctDC = as.numeric(NA)) %>%
      dplyr::mutate(temperature_pctDC = as.numeric(NA))
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(pct_DC_tbl)
  
}






