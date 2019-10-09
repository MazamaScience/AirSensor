#' @export
#' @importFrom rlang .data
#' 
#' @title Daily valid percentage
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param samplingFreq The number of samples measured per hour when the sensor 
#' is operating optimally. Note that currently the sensors measure 30 samples 
#' per hour but were previously sampling at a higher rate.
#' 
#' @description The number of valid (i.e., not NA or out of spec) sensor 
#' readings recorded per hour are summed over the course of a calendar day (24 
#' hours unless a partial day is included in the data), then divided by the 
#' total number of samples the sensor actually recorded in during that day 
#' (including NA and out of spec values) to return a percentage of each 
#' day that the sensor recorded valid measurements.
#' 
#' @note Purple Air II sensors reporting after the June, 2019 firmware
#' upgrade report data every 2 minutes or 30 measurements per hour. 
#' 
#' @examples  
#' tbl <- 
#'   example_pat_failure_B %>%
#'   SoH_pctValid() 
#' 

SoH_pctDC <- function(
  pat = NULL,
  aggregation_period = "30 min",
  parameter_sd = NULL
  
  
){
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  
  # ------
  
  if (aggregation_period == "30 min"){
    hourFactor <- 2
  }
  else if (aggregation_period == "hour"){ 
    hourFactor <- 1
  }
  else if (aggregation_period == "24 hour"){ 
    hourFactor <- 1/24
  }
  
  pct_DC_tbl <-
    pat %>%
    pat_aggregate(period = aggregation_period) %>%
    dplyr::mutate(day = as.Date(.data$datetime, format="%Y-%m-%d")) %>%
    dplyr::group_by(.data$day) %>% 
    dplyr::tally(.data[[parameter_sd]]==0)%>%
    dplyr::mutate(hourCount = n/hourFactor) %>%
    dplyr::mutate(pctDC = hourCount/24*100)
 
  #----- return
  return(pct_DC_tbl)
    
  

}






