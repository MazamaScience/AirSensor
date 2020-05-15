#' @title Aggregate data with count of outliers in each bin 
#'
#' @param pat a PurpleAir Timeseries \emph{pat} object.
#' @param period time period to average over. Can be "sec", "min", "hour", 
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

  # TODO: Reimplement .pat_agg and .flagOutliers into something better
  
  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Required parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Required parameter 'pat' has no data.") 
  
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
      pat_aggregate(pat)$data, 
      counts, 
      by = "datetime"
    )
  
  # ----- Return ---------------------------------------------------------------
  
  return(agg)
  
}

# ===== INTERNAL FUNCTION ======================================================

#' @keywords internal
#'
.pat_agg <- function(pat, stat, periodSeconds, parameters) {

  options(warn = -1) # Ignore all warnings

  if ( stat == "mean"       ) func <- function(x) mean(x, na.rm = TRUE)
  if ( stat == "median"     ) func <- function(x) median(x, na.rm = TRUE)
  if ( stat == "count"      ) func <- function(x) length(na.omit(x))
  if ( stat == "sd"         ) func <- function(x) sd(x, na.rm = TRUE)
  if ( stat == "sum"        ) func <- function(x) sum(na.omit(x))
  if ( stat == "max"        ) func <- function(x) max(na.omit(x))
  if ( stat == "min"        ) func <- function(x) min(na.omit(x))
  if ( stat == "tstats"     ) func <- function(x) list(x)

  # Remove duplicated time entries and create datetime axis (if any)
  datetime <-
    pat$data$datetime[which(!duplicated.POSIXlt(pat$data$datetime))]

  # Create data frame
  data <- data.frame(pat$data)[, parameters]

  # zoo with datetime index
  zz <- zoo::zoo(data, order.by = datetime)

  # NOTE:  Calling aggregate() will dispatch to aggregate.zoo() because the
  # NOTE:  argument is a "zoo" object. Unfortunately, we cannot call
  # NOTE:  zoo::aggregate.zoo() explicitly because this function is not
  # NOTE:  exported by the zoo package.

  # Aggregate
  zagg <-
    aggregate(
      zz,
      by = time(zz) - as.numeric(time(zz)) %% periodSeconds,
      FUN = func
    )

  # ----- !T-test --------------------------------------------------------------

  if ( stat != "tstats" ) {

    # Fortify to data.frame
    tbl <- zoo::fortify.zoo(zagg, names = "datetime")

    # Rename
    colnames(tbl)[-1] <- paste0(colnames(tbl)[-1], "_", stat)

    return(tbl)

  }

  # ----- T-test ---------------------------------------------------------------

  if ( stat == "tstats" ) {

    # Internal t.test function that will always respond, even in the face of
    # problematic data.
    .ttest <- function(x, y) {

      if ( length(na.omit(x)) <= 2 || length(na.omit(y)) <= 2 ) {
        # Not enough valid data
        return(t.test(c(0,0,0), c(0,0,0)))
      } else if ( sd(na.omit(x)) == 0 || sd(na.omit(y)) == 0 ) {
        # DC signal in at least one channel
        return(t.test(c(0,0,0), c(0,0,0)))
      } else {
        # Looking good -- calculate t.test
        return(t.test(x, y, paired = FALSE))
      }

    }

    # NOTE:  X below ends up with a two-column matrix where each cell contains an
    # NOTE:  unnamed List of length one containing a numeric vector. Each
    # NOTE:  row of the matrix thus contains a vector of pm25_A and pm25_B
    # NOTE:  in columns 1 and 2.

    # Map/Reduce t.test() to nested bins in matrix rows
    tt <-
      apply(
        X = zoo::coredata(zagg),
        MARGIN = 1,
        FUN = function(x) Reduce(.ttest, x)
      )

    # Create and fill stats lists
    t_score <-  p_value <- df_value <- vector("list", length(names(tt)))

    for ( i in names(tt) ) {

      val <- tt[[i]]
      ind <- which(names(tt) == i)

      t_score[[ind]] <- val[["statistic"]]
      p_value[[ind]] <- val[["p.value"]]
      df_value[[ind]] <- val[["parameter"]]

    }

    # Bind unlisted stats ->
    # Create zoo with aggregated datetime index ->
    # Fortify to data.frame
    tbl <-
      zoo::fortify.zoo(
        zoo::zoo(
          cbind(
            "pm25_t" = unlist(t_score),
            "pm25_p" = unlist(p_value),
            "pm25_df" = unlist(df_value)
          ),
          order.by = zoo::index(zagg)
        ),
        names = "datetime"
      )

    return(tbl)

  }

}
