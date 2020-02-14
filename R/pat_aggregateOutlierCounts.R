#' @title Aggregate data with count of outliers in each bin 
#'
#' @param pat a PurpleAir Timeseries \emph{pat} object.
#' @param period time period to average over. Can be "sec", "min", "hour", 
#' "day", "DSTday", "week", "month", "quarter" or "year". A number can also
#'  precede these options followed by a space (i.e. "2 day" or "37 min").
#' @param windowSize the size of the rolling window
#' @param thresholdMin the minimum threshold value to detect outliers
#' @param replace string vector specifying measurements for which outliers should be replaced before aggregating values.
#'  Can be any combination of "pm25_A", "pm25_B", "temperature", "humidity".
#'  Default value is NULL, indicating no replacements.
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
  thresholdMin = 8,
  replace = NULL
) { 
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Required parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Required parameter 'pat' has no data.") 
  
  if ( !is.character(replace) & !is.null(replace) )
    stop("replace must be a character string, vector of strings, or NULL.")
  
  if ( length(replace) > 4)
    stop("If not NULL, replace must be a character string holding between 1 and 4 values.")
  
  if (!is.null(replace) & any(!replace %in% c("pm25_A", "pm25_B", "temperature", "humidity") ) )
    stop("replace can only accept strings containing 'pm25_A', 'pm25_B', 'temperature', and 'humidity' as arguments.")
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  period <- tolower(period)
  
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
  
  # Create df to use functionally
  df2count <- 
    list(
      "pm25_A" = data.frame("datetime" = pat$data$datetime,
                            "pm25_A" = pat$data$pm25_A), 
      "pm25_B" = data.frame("datetime" = pat$data$datetime,
                            "pm25_B" = pat$data$pm25_B), 
      "humidity" = data.frame("datetime" = pat$data$datetime,
                              "humidity" = pat$data$humidity),
      "temperature" = data.frame("datetime" = pat$data$datetime,
                                 "temperature" = pat$data$temperature)
    )
  
  df2count_data <- purrr::map(df2count, ~ .x[!is.na(.x[, 2]),])
  df2count_missing <- purrr::map(df2count, ~ .x[is.na(.x[, 2]),])
  
  # map .flagOutliers to all applicable vectors
  # Ian: Why are we not filtering NA's before running through .flagOutlier? It appears to change behavior compared to pat_outliers.
  flagged_outliers<-
    purrr::map2(
      df2count_data, 
      names(df2count), 
      .flagOutliers, 
      windowSize, 
      thresholdMin
    ) 
  
  # Create median-fixed replacement values.
  if ( !is.null(replace) ) {
    replaced <- 
      purrr::map2(
        df2count_data[replace],
        replace,
        .replaceOutliers,
        medWin = windowSize,
        thresholdMin = thresholdMin
      )
    
    df2count_data[replace] <- replaced
  }
  
  # Binding flagged_outliers columns df2count_data.
  df_counted <-
    purrr::map2(
      df2count_data,
      flagged_outliers,
      .f = ~ dplyr::bind_cols(.x, .y[, 3, drop = FALSE])
    )  %>% 
    # Binding missing columns back to data columns.
    purrr::map2(
      .y = df2count_missing,
      .f = dplyr::bind_rows
    ) %>%
    # Replacing NA's flagged column with FALSE.
    purrr::map(
      .f = function(x) dplyr::mutate_at(x, .vars = 3,
                                        ~ifelse(is.na(.), FALSE, .))
    ) %>% 
    # Joining everything by datetime.
    purrr::reduce(
      full_join,
      by = "datetime"
    )
  
  # Adding new columns to pat.
  pat[["data"]] <- select(pat[["data"]], -c("pm25_A", "pm25_B", "temperature", "humidity")) %>% 
    full_join(y = df_counted, by = "datetime")
  
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