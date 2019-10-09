#' @export
#' @importFrom rlang .data
#' 
#' @title Daily reporting percentage
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param samplingInterval The number of seconds between samples when the sensor 
#' is operating optimally.
#' 
#' @description The number of sensor readings recorded per hour are summed over 
#' the course of a calendar day (24 hours unless the \code{pat} datetime column
#' includes a partial day at the beginning or end). This ishen divided by the 
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
#'   SoH_pctReporting(80) 
#' 
#' timeseriesTbl_multiplot(tbl)

SoH_pctReporting <- function(
  pat = NULL,
  samplingInterval = 120
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  
  
  # ----- SoH_pctReporting() ---------------------------------------------------

<<<<<<< HEAD:R/SoH_pctReporting.R
  samplesPerDay <- samplingFreq*24
=======
  samplesPerDay <- 24 * 3600 / samplingInterval
  
>>>>>>> jon:R/SoH_metrics.R
  tbl <- 
    pat %>%
    # Calculate the aggregation statistics based on a day rather than hourly.
    pat_aggregateOutlierCounts(period = "day") %>%
    # Divide the total count per day by the number of samples in a day where the
    # sensor was working perfectly (30 samp/hr * 24 hr/day)*100 to make percent.
    dplyr::mutate(pctReporting_pm25_A =.data$pm25_A_count/samplesPerDay*100) %>%
    dplyr::mutate(pctReporting_pm25_B =.data$pm25_B_count/samplesPerDay*100) %>%
    dplyr::mutate(pctReporting_humidity =.data$humidity_count/samplesPerDay*100) %>%
    dplyr::mutate(pctReporting_temperature =.data$temperature_count/samplesPerDay*100) %>%
    dplyr::select(.data$datetime, 
                  .data$pctReporting_pm25_A, .data$pctReporting_pm25_B,
                  .data$pctReporting_humidity, .data$pctReporting_temperature)
    
  # ----- Return ---------------------------------------------------------------
  
  return(tbl)
  
}


