#' @keywords pa_timeseries
#' @export
#' @importFrom rlang .data
#' @importFrom stats aggregate median na.omit quantile sd t.test time

#' @title Time averages for PrupleAir time series
#' 
#' @param pat PurpleAir Timeseries "pat" object from \code{pat_loadLatest()}
#' @param parameter The variable of timeseries data to aggergate, such as
#' "pm25_B", "temperature", etc.
#' @param period The time period to average to. Can be "sec", "min", "hour", 
#' "day", "DSTday", "week", "month", "quarter" or "year". A number can also
#'  precede these options followed by a space (i.e. "2 day" or "37 min").
#' @param stats The statistic to apply when aggregating the data; default is the 
#' mean. Can be one of "mean", "max", "min", "median", "count", 
#' "sd", "percentile", "tstats". 
#' 
#' @param dataThreshold A % of the data capture threshold. A value of 0 means 
#' that all data will be used in a particular period regardless of the number of
#' values avaliable. Conversely, a value of 100 means that all data will need to 
#' be present to proceed, else it is recorded as NA. 
#' @param quickStats a logical that if \code{TRUE} will override \code{stats} 
#' parameter and return and a data frame of "mean", "sd", and "count". 
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
#' pat_aggregate(pat, "1 hour")
#' }
#' 
#' \dontrun{
#' # Maximum weekly temperature
#' pat %>%
#'   pat_aggregate("temperature", "1 week", stats = "max")
#' }

pat_aggregate <- function(
  pat, 
  period = "1 hour",
  stats = "mean",
  parameter = NULL,
  dataThreshold = 0,
  quickStats = TRUE
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
    "count", 
    "max", 
    "min", 
    "sum",
    "sd", 
    "tstats"
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
  
  # ----- Convert period to seconds --------------------------------------------
  
  by2 <- strsplit(period, " ", fixed = TRUE)[[1]]
  seconds <- 1
  
  if ( length(by2) > 1 ) seconds <- as.numeric(by2[1])
  units <- by2[length(by2)]
  
  if ( units == "sec"     ) int <- 1
  if ( units == "min"     ) int <- 60
  if ( units == "hour"    ) int <- 3600
  if ( units == "day"     ) int <- 3600 * 24
  if ( units == "week"    ) int <- 3600 * 24 * 7
  if ( units == "month"   ) int <- 3600 * 24 * 31
  if ( units == "quarter" ) int <- 3600 * 24 * 31 * 3
  if ( units == "year"    ) int <- 3600 * 8784 
  
  pseconds <- seconds * int 
  
  # ----- Aggregate the data ---------------------------------------------------
  
  pat_agg <- function(pat, stats, pseconds) {
    
    options(warn = -1)
    
    thresh <- function(x) { # handle data thresholding
      if( sum(is.na(x)) / length(x) >= 1 - (dataThreshold / 100) ) {
        return(NA)
      } else {
        return(x)
      }
    } 
    
    if ( stats == "mean"       ) func <- function(x) mean(thresh(x), na.rm = TRUE)
    if ( stats == "median"     ) func <- function(x) median(thresh(x), na.rm = TRUE) 
    if ( stats == "count"      ) func <- function(x) length(na.omit(x))
    if ( stats == "sd"         ) func <- function(x) sd(thresh(x), na.rm = TRUE)
    if ( stats == "sum"        ) func <- function(x) sum(na.omit(x))
    if ( stats == "max"        ) func <- function(x) max(na.omit(x))
    if ( stats == "min"        ) func <- function(x) min(na.omit(x))
    if ( stats == "tstats"     ) func <- function(x) x 

    if ( stats != "tstats" ) { # Handle ! test stats
      
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
          FUN = func, 
          simplify = TRUE
        ) %>%
        zoo::fortify.zoo(
          names = c(Index = "datetime")
        ) %>% 
        dplyr::rename_at(
          dplyr::vars(2:len),
          .funs = function(x) paste0(x, "_", stats) # functionally name
        ) %>% 
        dplyr::as_tibble()
      
      return(df)
      
    } else { # Handle test statistics
      
      data <- 
        pat$data %>% 
        dplyr::select("pm25_A", "pm25_B")
      
      datetime <- pat$data$datetime
      
      zz <-       
        zoo::zoo(
          data, 
          structure(
            datetime, 
            class = c("POSIXt", "POSIXct")
          )
        )
      
      bin <-
        aggregate(
          zz, 
          by = time(zz) - as.numeric(time(zz)) %% pseconds, 
          FUN = func, 
          simplify = TRUE
        ) %>% 
        zoo::fortify.zoo(
          names = c(Index = "datetime")
        )
      
      # NOTE: T-tests should only be used when n < 30. Any larger, the 
      #       distribution approches normal -> should use Z test when n > 30.
      #       n > 30 ~ period = 30 min 
      
      t_score <-  p_value <- df_value <-  list()
      
      # NOTE:  Handle:
      # NOTE:    Error in t.test.default(bin$pm25_A[[i]], bin$pm25_B[[i]]) : 
      # NOTE:    not enough 'x' observations
      
      for( i in 1:length(bin$datetime) ) { 
        
        result <- 
          try({
            stats <- 
              t.test(
                bin$pm25_A[[i]], 
                bin$pm25_B[[i]], 
                paired = TRUE
              )}, 
              silent = TRUE
            )
        
        if ( "try-error" %in% class(result) ) {
          
          t_score[[i]] <- NA
          p_value[[i]] <- NA 
          df_value[[i]] <- NA
          
        } else {
          
          t_score[[i]] <- stats$statistic
          p_value[[i]] <- stats$p.value 
          df_value[[i]] <- stats$parameter
          
        }
        
      }
      
      df <- 
        dplyr::tibble(
          datetime = bin$datetime, 
          pm25_t = unlist(t_score), 
          pm25_p = unlist(p_value),
          pm25_df = unlist(df_value)
        )
      
      # TODO: Z - Test
      
      options(warn=0)
      
      return(df)
      
    }
    
  }
  
  # ---- Check and return ------------------------------------------------------
  
  if ( quickStats ) { # Handle quickstats 
    
    timeStats <- 
      plyr::join_all(
        list(
          pat_agg(pat, "tstats", pseconds),
          pat_agg(pat, "mean", pseconds),
          pat_agg(pat, "median", pseconds),
          pat_agg(pat, "sd", pseconds), 
          pat_agg(pat, "min", pseconds), 
          pat_agg(pat, "max", pseconds),
          pat_agg(pat, "count", pseconds)
        ),
        by = "datetime"
      )  
    
    timeStats <- # Re-arrange order
      timeStats[,
                c(
                  "datetime", 
                  names(timeStats)[c(
                    grep("pm25_", names(timeStats)), 
                    grep("humid", names(timeStats)), 
                    grep("temp", names(timeStats))
                  )]
                )]
    
     } else { # Handle all else
    
    timeStats <- 
      pat_agg(pat, stats, pseconds)
    
  }
 
  return(timeStats)
  
}
