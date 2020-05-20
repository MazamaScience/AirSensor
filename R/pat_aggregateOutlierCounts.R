#' @export
#' @title Aggregate data with count of outliers in each bin 
#'
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param unit Character string specifying temporal units for binning.
#' #' @param count Number of units per bin.
#' @param windowSize the size of the rolling window. Must satisfy windowSize <= count.
#' @param thresholdMin the minimum threshold value to detect outliers via hampel filter
#'
#' @return \code{data.frame} A data.frame with flag counts per bin.
#' 
#' @seealso pat_aggregateData
#' 
#' @examples
#' \donttest{
#' library(AirSensor)
#' library(ggplot2)
#' 
#' df <- 
#'   pat_aggregateOutlierCounts(example_pat_failure_A)
#' 
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
  unit = "minutes",
  count = 60,
  windowSize = 23,
  thresholdMin = 8
) { 
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Required parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Required parameter 'pat' has no data.") 
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  # Find the outliers according to specified window on full datetime axis
  # NOTE: Independent of aggregation period 
  
  # PM25 A
  tryCatch(
    expr = {
      pm25_A_outliers <- seismicRoll::roll_hampel(
        pat$data$pm25_A, 
        n = windowSize ) > thresholdMin
    }, 
    error = function(e) {
      pm25_A_outliers <- NA
    })
  
  # PM25 B
  tryCatch(
    expr = {
      pm25_B_outliers <- seismicRoll::roll_hampel(
        pat$data$pm25_B, 
        n = windowSize ) > thresholdMin
    }, 
    error = function(e) {
      pm25_B_outliers <- NA
    })
  
  # Temperature
  tryCatch(
    expr = {
      temp_outliers <- seismicRoll::roll_hampel(
        pat$data$temperature, 
        n = windowSize ) > thresholdMin
    }, 
    error = function(e) {
      temp_outliers <- NA
    })
  
  # Humidity
  tryCatch(
    expr = {
      humidity_outliers <- seismicRoll::roll_hampel(
        pat$data$humidity, 
        n = windowSize ) > thresholdMin
      
    }, 
    error = function(e) {
      humidity_outliers <- NA
    })
  
  
  pat$data$pm25_A_outlierCount <- ifelse(pm25_A_outliers, 1, 0)
  pat$data$pm25_B_outlierCount <- ifelse(pm25_B_outliers, 1, 0)
  pat$data$humidity_outlierCount <- ifelse(humidity_outliers, 1, 0)
  pat$data$temperature_outlierCount <- ifelse(temp_outliers, 1, 0)
  
  # Aggregate
  # NOTE: Count by summing boolean outlier values
  tryCatch(
    expr = {
      data <- patData_aggregate(
        df = pat$data,
        unit = unit, 
        count = count,
        FUN = function(df) {
          pm25_A_outlierCount <- sum(df$pm25_A_outlierCount, na.rm = TRUE)
          pm25_B_outlierCount <- sum(df$pm25_B_outlierCount, na.rm = TRUE)
          humidity_outlierCount <- sum(df$humidity_outlierCount, na.rm = TRUE)
          temperature_outlierCount <- sum(df$temperature_outlierCount, na.rm = TRUE)
          counts <- cbind(
            pm25_A_outlierCount, 
            pm25_B_outlierCount, 
            humidity_outlierCount, 
            temperature_outlierCount
          )
          return(counts)
        }
      )
    }, error = function(e) {
      stop('Outlier Aggregation count failed.')
    }
  )
  
  return(data)
  
}