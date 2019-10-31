#' @export
#' @importFrom rlang .data
#' 
#' @title Daily reporting percentage
#' 
#' @param aggregationStats Dataframe of statistics as returned by
#' \code{pat_aggregate()}.
#' @param samplingFreq The number of samples measured per hour when the sensor 
#' is operating optimally. Note that currently the sensors measure 30 samples 
#' per hour but were previously sampling at a higher rate.
#' 
#' @description The number of sensor readings recorded per hour are summed over 
#' the course of a calendar day (24 hours unless a partial day is included in 
#' the data), then divided by the number of samples the sensor would record in 
#' an ideal day (30 samples/hour * 24 hours/day) to return a percentage of each 
#' day that the sensor is performing optimally.
#' 
#' @note Purple Air II sensors reporting after the June, 2019 firmware
#' upgrade report data every 2 minutes or 30 measurements per hour. 
#' 
#' @examples 
#' tbl <- 
#'   example_pat %>%
#'   pat_aggregateOutlierCounts() %>%
#'   SoH_dailyPctReporting() 
#' 
#' plot(tbl$day, tbl$pct_Reporting)
#' 
#' tbl <- 
#'   example_pat_failure_B %>%
#'   pat_aggregateOutlierCounts() %>%
#'   SoH_dailyPctReporting() 
#' 
#' plot(tbl$day, tbl$pct_Reporting)
#' 
#' 

SoH_dailyPctReporting <- function(
  aggregationStats = NULL,
  samplingFreq = 30
  
){
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(aggregationStats)
  
  # ----- SoH_pctRecording() ---------------------------------------------------

  samplingFreq <- 30 
  samplesPerDay <- samplingFreq*24
  tbl <- 
    aggregationStats %>%
    # Create a new column that indicates the day
    dplyr::mutate(day = as.Date(.data$datetime, format="%Y-%m-%d")) %>%
    # Group the data by the day
    dplyr::group_by(.data$day) %>%
    # Sum the number of samples per hour (the count per channel) in each day and
    # reduce the tbl down to just the grouping variable and the calculated 
    # variable (in this case, "daily_sum")
    dplyr::summarise(daily_sum = sum(.data$pm25_A_count)) %>%
    # Divide the total count per day by the number of samples in a day where the
    # sensor was working perfectly (30 samp/hr * 24 hr/day)*100 to make percent.
    dplyr::mutate(pct_Reporting = .data$daily_sum/samplesPerDay*100)
  
  # ----- Return ---------------------------------------------------------------
  
  return(tbl)
}


