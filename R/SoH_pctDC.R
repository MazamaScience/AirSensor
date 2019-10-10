#' @export
#' @importFrom rlang .data
#' 
#' @title Daily DC Signal percentage
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param aggregation_period The period to aggregate over. Choose from "30 min",
#' "1 hour", or "1 day". 
#' @param timezone Olson timezone at the location of interest
#' 
#' @description This function calculates the daily percentage of DC signal 
#' recorded by the \code{pm25_A}, \code{pm25_B}, \code{humidity}, and 
#' \code{temperature} channels. The data are flagged as DC signal when the 
#' standard deviation of the data from each channel equals zero over the 
#' \code{aggregation_period}. The number of DC hours are summed over the day and
#' a daily DC percentage is returned. 
#' 
#' 
#' @examples  
#' tbl <- 
#'   example_pat_failure_B %>%
#'   SoH_pctDC(aggregation_period = "30 min", timezone = "America/Los_Angeles") 
#' 

SoH_pctDC <- function(
  pat = NULL,
  aggregation_period = "30 min",
  timezone = NULL
  
  
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
  else if (aggregation_period == "1 hour"){ 
    hourFactor <- 1
  }
  else if (aggregation_period == "1 day"){ 
    hourFactor <- 1/24
  }
  
  pct_DC_tbl <-
    pat %>%
    pat_aggregate(period = aggregation_period) %>%
    
    # group by day so that the DC time segments can be summed over the day even
    # if the aggregation period is less than one day. Each day must be a complete
    # day to avoid tapering when diving by 24 hours later on.
    dplyr::mutate(daystamp = strftime(.data$datetime, "%Y%m%d", tz = timezone)) %>%
    dplyr::group_by(.data$daystamp) %>% 
    
    # tally the number of time segments the standard deviation is 0 for each channel
    dplyr::add_tally(.data$pm25_A_sd==0, name = "DCSignalCount_pm25_A") %>%
    dplyr::add_tally(.data$pm25_B_sd==0, name = "DCSignalCount_pm25_B") %>%
    dplyr::add_tally(.data$humidity_sd==0, name = "DCSignalCount_humidity") %>%
    dplyr::add_tally(.data$temperature_sd==0, name = "DCSignalCount_temperature") %>%
    
    # summarize each tally channel. Since, it's a tally per day, take the max each day
    # rather than sum.
    dplyr::summarise_at(.vars = c("DCSignalCount_pm25_A", "DCSignalCount_pm25_B", 
                           "DCSignalCount_temperature", "DCSignalCount_humidity"),max) %>%
    
    # turn the DC signal time segments into hours per day 
    dplyr::mutate(DC_PM25_A_hourCount = .data$DCSignalCount_pm25_A/hourFactor) %>%
    dplyr::mutate(DC_PM25_B_hourCount = .data$DCSignalCount_pm25_B/hourFactor ) %>%
    dplyr::mutate(DC_humidity_hourCount = .data$DCSignalCount_humidity/hourFactor ) %>%
    dplyr::mutate(DC_temperature_hourCount = .data$DCSignalCount_temperature/hourFactor ) %>%
    
    # turn the hours of DC per day into a percentage
    dplyr::mutate(pctDC_PM25_A = .data$DC_PM25_A_hourCount/24*100) %>%
    dplyr::mutate(pctDC_PM25_B = .data$DC_PM25_B_hourCount/24*100) %>%
    dplyr::mutate(pctDC_humidity = .data$DC_humidity_hourCount/24*100) %>%
    dplyr::mutate(pctDC_temperature = .data$DC_temperature_hourCount/24*100) %>%
    
    # add back in the datetime column that was removed during summarizing.
    dplyr::mutate(datetime = MazamaCoreUtils::parseDatetime(.data$daystamp, timezone = timezone))
    
  #----- return
  return(pct_DC_tbl)
    

}






