#' @keywords pa_timeseries
#' @export
#' @importFrom rlang .data
#' @title Time averages for PrupleAir time series
#' 
#' @param pat PurpleAir Timeseries "pat" object from \code{pat_load()}
#' @param parameter Data to display: "pm25", "humidity", "temperature"
#' @param period The time period to average to. Can be "sec", "min", "hour", 
#' "day", "DSTday", "week", "month", "quarter" or "year". A number can also
#'  precede these options followed by a space (i.e. "2 days" or "37 min").
#' @param stats The statistic to apply when aggregating the data; default is the 
#' mean. Can be one of "mean", "max", "min", "median", "frequency", 
#' "sd", "percentile".
#' @param dataThreshold A % of the data capture threshold. A value of 0 means 
#' that all data will be used in a particular period regardless of the number of
#' values avaliable. Conversely, a value of 100 means that all data will need to 
#' be present to proceed, else it is recorded as NA. 
#' @param fill When time series are expanded data are ‘padded out’ with NA. To 
#' ‘pad-out’ the additional data with the first row in each original time 
#' interval, choose fill = TRUE.
#' 
#' @description Function to flexibly aggregate or expand data frames by 
#' different time periods, calculating vector-averages for a PurpleAir time 
#' series object. \code{pat_timeAverage()} should be useful in many 
#' circumstances where it is necessary to work with different time average data. 
#' 
#' @return Returns a dataframe with a date in class POSIXct.
#' 
#' @examples
#' \dontrun{
#' pat_timeAverage(pat, "pm25_A", "1 hour")
#' }
#' \dontrun{
#' pat %>%
#'   pat_timeAverage("humidity", "12 hour", stats = "max")
#' }

pat_timeAverage <- function(
  pat = NULL, 
  parameter = "pm25_A",
  period = "10 min", 
  stats = "mean", 
  dataThreshold = 0,
  fill = FALSE
) { 
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !pat_isPat(pat) )
    stop("parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("parameter 'pat' has no data.") 
  
  # ----- Check case -----------------------------------------------------------
  
  tolower(parameter) -> parameter
  tolower(period) -> period
  tolower(stats) -> stats
 
  # ----- Wrapped Time Average Function ----------------------------------------
   
  timeAverage <- function(df, period, dataThreshold, stats, fill) {
    avg <- 
      df %>%
      openair::timeAverage(
        avg.time = period, 
        data.thresh = dataThreshold, 
        statistic = stats, 
        fill = fill ) 
    return(avg)
      
  }
  
  # ----- Handle Channels A,B seperately, otherwise -> generic function --------
  
  if ( parameter == "pm25_a"  ) {
    
    pm25_A <-
      pat$data %>%
      select(.data$datetime_A, .data$pm25_A) %>%
      rename(date = .data$datetime_A, pm25 = .data$pm25_A) %>%
      filter(!is.na(.data$pm25)) %>%
      timeAverage(
        period = period, 
        stats = stats, 
        fill = fill, 
        dataThreshold = dataThreshold
        )
    
    return(pm25_A)
  
  } else if ( parameter == "pm25_b" ) {
    
    pm25_B <-
      pat$data %>%
      select(.data$datetime_B, .data$pm25_B) %>%
      rename(date = .data$datetime_B, pm25 = .data$pm25_B) %>%
      filter(!is.na(.data$pm25)) %>% 
      timeAverage(
        period = period, 
        stats = stats, 
        fill = fill, 
        dataThreshold = dataThreshold
        ) 
    
    return(pm25_B)
    
  } else { 
    
    df <- 
      pat$data %>% 
      select(.data$datetime, .data[[parameter]]) %>% 
      rename(date = .data$datetime) %>% 
      filter(!is.na(parameter)) %>%
      timeAverage(
        period = period, 
        stats = stats, 
        fill = fill, 
        dataThreshold = dataThreshold
      ) 
    
    return(df)
      
  }

}
  
  