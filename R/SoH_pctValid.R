#' @export
#' @importFrom rlang .data
#' 
#' @title Daily valid percentage
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' 
#' @description The number of valid (i.e., not NA or out of spec) sensor 
#' readings recorded per hour are summed over the course of a calendar day (24 
#' hours unless a partial day is included in the data), then divided by the 
#' total number of samples the sensor actually recorded in during that day 
#' (including NA and out of spec values) to return a percentage of each 
#' day that the sensor recorded valid measurements.
#' 
#' 
#' @examples  
#' tbl <- 
#'   example_pat_failure_B %>%
#'   SoH_pctValid() 
#' 

SoH_pctValid <- function(
  pat = NULL
){
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  
  
  # ----- SoH_pctValid() ---------------------------------------------------

  # Calculate a baseline tbl that contains the count without removing entries 
  # containing NA or out of spec values
  baseline_tbl <-
    pat %>%
    pat_aggregateOutlierCounts(period = "day")
  
  # Calculate a tbl after removing entries containing NA and out of spec values
  valid_tbl <-
    pat %>%
    pat_qc()%>%
    pat_aggregateOutlierCounts(period = "day") %>%
    # Add columns to the valid tbl to contain valid percentages
    dplyr::mutate(pm25_A_pctValid = 
             .data$pm25_A_count/baseline_tbl$pm25_A_count*100) %>%
    dplyr::mutate(pm25_B_pctValid = 
             .data$pm25_B_count/baseline_tbl$pm25_B_count*100) %>%
    dplyr::mutate(temperature_pctValid = 
             .data$temperature_count/baseline_tbl$temperature_count*100) %>%
    dplyr::mutate(humidity_pctValid = 
             .data$humidity_count/baseline_tbl$humidity_count*100) %>%
    dplyr::select("datetime", contains("Valid"))
  
  # ----- Return ---------------------------------------------------------------
  
  return(valid_tbl)
}


