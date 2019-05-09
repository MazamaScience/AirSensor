#' @keywords pa_timeseries
#' @export
#' @importFrom rlang .data
#' @title Time averages for PrupleAir time series
#' 
#' @param pat PurpleAir Timeseries "pat" object from \code{pat_load()}
#' @param parameter The variable of timeseries data to aggergate, such as
#' "pm25_B", "temperature", etc.
#' @param period The time period to average to. Can be "sec", "min", "hour", 
#' "day", "DSTday", "week", "month", "quarter" or "year". A number can also
#'  precede these options followed by a space (i.e. "2 day" or "37 min").
#' @param stats The statistic to apply when aggregating the data; default is the 
#' mean. Can be one of "mean", "max", "min", "median", "frequency", 
#' "sd", "percentile".
#' @param dataThreshold A % of the data capture threshold. A value of 0 means 
#' that all data will be used in a particular period regardless of the number of
#' values avaliable. Conversely, a value of 100 means that all data will need to 
#' be present to proceed, else it is recorded as NA. 
#' @param pprobs 	numeric vector of probabilities with values in [0,1]. Only 
#' valid when \code{stats = "percentile"}
#' @param quickStats a logical that if \code{TRUE} will override \code{stats} 
#' parameter and return and a data frame of "mean", "sd", and "frequency". 
#' 
#' @description Function to flexibly aggregate or expand data frames by 
#' different time periods and calculating vector-averages for a PurpleAir time 
#' series object. This function should be useful in many 
#' circumstances where it is necessary to work with different time average data. 
#' 
#' @return Returns a dataframe with a date in class POSIXct.
#' 
#' @examples
#' \dontrun{
#' # Hourly mean of Channel A PM2.5 density measurement
#' pat_timeAverageDiagnostic(pat, "pm25_A", "1 hour")
#' }
#' 
#' \dontrun{
#' # Maximum weekly temperature
#' pat %>%
#'   pat_timeAverageDiagnostic("temperature", "1 week", stats = "max")
#' }

pat_timeAverageDiagnostics <- function(
  pat, 
  period = "15 min",
  stats = "mean",
  parameter = NULL,
  dataThreshold = NULL,
  pprobs = NULL,
  quickStats = FALSE,
  ...
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  tolower(period) -> period
  tolower(stats) -> stats
  
  if ( !pat_isPat(pat) )
    stop("parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("parameter 'pat' has no data.") 
  
  if ( !stats %in% c(
    "mean", 
    "median", 
    "frequency", 
    "max", 
    "min", 
    "sum",
    "sd", 
    "percentile", 
    "all"
  ) ) {
    stop("Statistic not recognized")
  } 
  
  if ( is.null(parameter) ) { 
    
    # Default parameters
    parameter = c(
      "pm25_A", 
      "pm25_B", 
      "humidity", 
      "temperature"
    )
    
    len <- length(parameter) + 1
    
  } else { 
    
    parameter = parameter
    len <- 2 # Cheap Hack.
    
  }
  
  # ----- Determine function --------------------------------------------------- 
  
  ffunc <- function(stats) {
    
    if ( stats == "mean" ) func <- function(x) mean(x, na.rm = TRUE)
    if ( stats == "median" ) func <- function(x) median(x, na.rm = TRUE) 
    if ( stats == "frequency" ) func <- function(x) length(na.omit(x))
    if ( stats == "sd" ) func <- function(x) sd(x, na.rm = TRUE)
    if ( stats == "sum" ) func <-  function(x) sum(na.omit(x))
    if ( stats == "max" ) func <- function(x) max(na.omit(x))
    if ( stats == "min" ) func <- function(x) min(na.omit(x))
    if ( stats == "percentile") func <- function(x) quantile(x, 
                                                             probs = pprobs, 
                                                             na.rm = TRUE)
    return(func)
    
  }
  
  # ----- Convert period to seconds --------------------------------------------
  
  by2 <- strsplit(period, " ", fixed = TRUE)[[1]]
  seconds <- 1
  
  if (length(by2) > 1) seconds <- as.numeric(by2[1])
  units <- by2[length(by2)]
  
  if (units == "sec") int <- 1
  if (units == "min") int <- 60
  if (units == "hour") int <- 3600
  if (units == "day") int <- 3600 * 24
  if (units == "week") int <- 3600 * 24 * 7
  if (units == "month") int <- 3600 * 24 * 31
  if (units == "quarter") int <- 3600 * 24 * 31 * 3
  if (units == "year") int <- 3600 * 8784 
  
  pseconds <- seconds * int 
  
  # ----- Aggregate the data ---------------------------------------------------
  
  pat_agg <- function(pat, stats, pseconds) {
    
    options(warn = -1)
    
    data <- 
      pat$data %>%  
      dplyr::select(parameter)
    
    datetime <- pat$data$datetime
    
    zz <- 
      zoo::zoo(
        data, 
        structure(
          datetime, 
          class = c("POSIXt", "POSIXct")
        )
      )
    
    df <- 
      aggregate(
        zz, 
        by = time(zz) - as.numeric(time(zz)) %% pseconds, 
        FUN = ffunc(stats), 
        simplify = TRUE
      ) %>%
      zoo::fortify.zoo(
        names = c(Index = "datetime")
      ) %>% 
      dplyr::rename_at(
        dplyr::vars(2:len),
        .funs = function(x) paste0(x, "_", stats)
      ) %>% 
      dplyr::as_tibble()
    
    options(warn = 0)
    
    return(df)
    
  }
  
  # ---- Check and return ------------------------------------------------------
  
  if ( quickStats ) {
    
    timeStats <- plyr::join_all(
      list(
        pat_agg(pat, "mean", pseconds), 
        pat_agg(pat, "sd", pseconds), 
        pat_agg(pat, "frequency", pseconds)
      ),
      by = "datetime"
    )
    
  } else { 
    
    timeStats <- 
      pat_agg(pat, stats, pseconds)
    
  }
  
  return(timeStats)
  
}