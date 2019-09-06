#' @title Aggreagte data with count of outliers in each bin 
#'
#' @param pat a PurpleAir Timeseries \emph{pat} object.
#' @param period time period to average to. Can be "sec", "min", "hour", 
#' "day", "DSTday", "week", "month", "quarter" or "year". A number can also
#'  precede these options followed by a space (i.e. "2 day" or "37 min").
#' @param windowSize the size of the rolling window
#' @param thresholdMin the minimum threshold value to detect outliers
#'
#' @return \code{data.frame} A data.frame with additional flag count vectors
#' @export
#' @seealso pat_aggregate
#' @examples
#' \donttest{
#' 
#' df <- 
#'   pat_aggregateOutlierCounts(example_pat_failure_A)
#' 
#' library(ggplot2)
#' # Plot the counts 
#' multi_ggplot(
#'   # A Channel
#'   ggplot(df, aes(x = datetime, y = pm25_A_outlierCount)) + geom_point(),
#'   # B Channel
#'   ggplot(df, aes(x = datetime, y = pm25_B_outlierCount)) + geom_point(),
#'   # Humidity
#'   ggplot(df, aes(x = datetime, y = humidity_outlierCount)) + geom_point(),
#'   # Temperature 
#'   ggplot(df, aes(x = datetime, y = temperature_outlierCount)) + geom_point()
#' )
#' 
#' }
#' 

pat_aggregateOutlierCounts <- function(
  pat = NULL,
  period = "1 hour",
  windowSize = 23,
  thresholdMin = 8
) { 
  
  # ----- Validate parameters --------------------------------------------------

  period <- tolower(period)
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Required parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Required parameter 'pat' has no data.") 
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  # ----- Convert period to seconds --------------------------------------------
  
  periodParts <- strsplit(period, " ", fixed = TRUE)[[1]]
  
  if ( length(periodParts) == 1 ) {
    periodCount <- 1
    units <- periodParts[1]
  } else {
    periodCount <- as.numeric(periodParts[1])
    units <- periodParts[2]
  }
  
  if ( units == "sec"     ) unitSecs <- 1
  if ( units == "min"     ) unitSecs <- 60
  if ( units == "hour"    ) unitSecs <- 3600
  if ( units == "day"     ) unitSecs <- 3600 * 24
  if ( units == "week"    ) unitSecs <- 3600 * 24 * 7
  if ( units == "month"   ) unitSecs <- 3600 * 24 * 31
  if ( units == "quarter" ) unitSecs <- 3600 * 24 * 31 * 3
  if ( units == "year"    ) unitSecs <- 3600 * 8784 
  
  periodSeconds <- periodCount * unitSecs 
  
  # Name of applicable vectors
  names2count <- 
    list(
      "pm25_A", 
      "pm25_B", 
      "humidity", 
      "temperature"
    )
  
  # Create df to use functionally
  df2count <- 
    list(
      data.frame("pm25_A" = pat$data$pm25_A), 
      data.frame("pm25_B" = pat$data$pm25_B), 
      data.frame("humidity" = pat$data$humidity),
      data.frame("temperature" = pat$data$temperature)
    )
  
  # map .flagOutliers to all applicable vectors
  flagged_outliers<-
    purrr::map2(
      df2count, 
      names2count, 
      .flagOutliers, 
      windowSize, 
      thresholdMin
    )
  # lapply function to grab flag vectors -> binds columns
  flags <- 
    do.call(
      "cbind",
      lapply(
        flagged_outliers, 
        FUN = function(x) x[2]
      )
    )
  
  pat[["data"]] <- cbind(pat[["data"]], flags)
  
  counts <- 
    .pat_agg(
      pat = pat, 
      stat = "sum", 
      periodSeconds = periodSeconds, 
      parameters = 
        names(pat[["data"]])
      [which(stringr::str_detect(names(pat[["data"]]), "flag_outliers_"))]
    )
  
  # Rename 
  names(counts) <- 
    c("datetime", 
      "pm25_A_outlierCount",
      "pm25_B_outlierCount",
      "humidity_outlierCount", 
      "temperature_outlierCount")
  
  agg <- 
    dplyr::left_join(
      pat_aggregate(pat, period), 
      counts, 
      by = "datetime"
    )
  
  # ----- Return ---------------------------------------------------------------
  
  return(agg)
  
}
