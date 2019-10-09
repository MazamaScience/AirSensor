#' @export
#' @importFrom rlang .data
#' 
#' @title Daily reporting percentage
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
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
#'   SoH_pctReporting() 
#' 
#' plot(tbl$datetime, tbl$pct_Reporting)
#' 
#' tbl <- 
#'   example_pat_failure_B %>%
#'   SoH_pctReporting() 
#' 
#' plot(tbl$datetime, tbl$pct_Reporting)
#' 
#' 

SoH_pctReporting <- function(
  pat = NULL,
  samplingFreq = 30
  
){
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  
  
  # ----- SoH_pctReporting() ---------------------------------------------------

  samplesPerDay <- samplingFreq*24
  tbl <- 
    pat %>%
    # Calculate the aggregation statistics based on a day rather than hourly.
    pat_aggregateOutlierCounts( period = "day") %>%
    # Divide the total count per day by the number of samples in a day where the
    # sensor was working perfectly (30 samp/hr * 24 hr/day)*100 to make percent.
    dplyr::mutate(pct_Reporting =.data$pm25_A_count/samplesPerDay*100)
  
  # ----- Return ---------------------------------------------------------------
  
  return(tbl)
}


