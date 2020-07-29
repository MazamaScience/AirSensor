#' @keywords pa_timeseries
#' @export
#' @importFrom rlang .data
#' @importFrom stats aggregate median na.omit quantile sd t.test time
#'
#' @title Aggregate PurpleAir Timeseries Data
#'
#' @param df Timeseries \emph{pat} data, or timeseries data.frame with valid
#' \emph{datetime} column.
#' @param FUN The function to be applied to each vector of numeric \code{df}.
#' @param unit Character string specifying temporal units for binning.
#' @param count Number of units per bin.
#'
#' @description Aggregate a dataframe into temporal bins and apply a function.
#' Temporal aggregation involves splitting a dataframe into separate bins along
#' its \code{datetime} axis. \code{FUN} is mapped to the \code{df}
#' dataframe records in each bin which are then recombined into an aggregated
#' dataframe.
#'
#' @details This function is intended for advanced users who wish to have more
#' flexibility than the standard \emph{pat_aggregate()} while aggregating
#' timeseries data. \code{FUN} can operate and access all numeric vectors
#' within the data frame \code{df} and must return a matrix or tibble of numeric 
#' values.
#'
#' @return Returns an aggregated \emph{data.frame} object.
#'
#' @examples
#' library(AirSensor)
#'
#' # Single day subset
#' pat <-
#'   example_pat %>%
#'   pat_filterDate(20180813, 20180814)
#'
#' # Two Sample Student T-Test (advanced users only - see details.)
#' FUN_ttest <- function(x) {
#'   htest <- stats::t.test(x$pm25_A, x$pm25_B, paired = FALSE)
#'   tbl <- dplyr::tibble(
#'     t_score = as.numeric(htest$statistic),
#'     p_value = as.numeric(htest$p.value),
#'     df_value = as.numeric(htest$parameter)
#'   )
#'   return(tbl)
#' }
#'
#' t.testStats <-
#'   pat %>%
#'   pat_extractData() %>% # Note: Extract the timeseries data.frame
#'   patData_aggregate(FUN_ttest)
#'   
#' head(t.testStats)

patData_aggregate <- function(
  df,
  FUN = function(df) { mean(df$pm25_A + df$pm25_B, na.rm = TRUE) },
  unit = 'minutes',
  count = 60
) {

  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(df)
  MazamaCoreUtils::stopIfNull(FUN)
  MazamaCoreUtils::stopIfNull(unit)
  MazamaCoreUtils::stopIfNull(count)
  
  if ( !"datetime" %in% names(df) )
    stop("Column 'datetime' is is missing from 'df'.")
  
  if ( nrow(df) == 0 )
    stop("Parameter 'df' has no data.")
  
  # Remove any duplicate data records
  df <- dplyr::distinct(df)
  
  # Create break units from count and unit params
  if ( stringr::str_detect(unit, 'minutes') ) {
    lubridateBreakUnit <- paste(count, unit, sep = ' ')
    seqBreakUnit <- paste(count, 'mins', sep = ' ')
  } else if (stringr::str_detect(unit, 'hour') ) {
    lubridateBreakUnit <- paste(count, unit, sep = ' ')
    seqBreakUnit <- paste(count, unit, sep = ' ')
  } else {
    stop('Only hours and minutes are currently supported units.')
  }
  
  # ----- Aggregate Data -------------------------------------------------------

  # Only use numeric columns for aggregation matrix
  numeric_cols <- which(unlist(lapply(df, is.numeric)))

  # Convert to eXtensible Time Series (xts) data.frame
  # Separate only useful data for calculation (i.e. only numeric)
  df <- xts::xts(
    x = df[numeric_cols],
    order.by = df$datetime,
    unique = TRUE,
    tzone = 'UTC'
  )

  # Split the xts into a list of binned xts matrices
  df_bins <- xts::split.xts(
    df,
    f = unit,
    drop = FALSE,
    k = count
  )

  # ----- Datetime Axis --------------------------------------------------------
  
  # Get the first index of aligned time for future use.
  datetime <- as.numeric(
    lapply(
      X = df_bins, 
      # Select first datetime index in bin to use as aggregated datetime axis
      FUN = function(x) lubridate::floor_date(zoo::index(x)[1], unit = lubridateBreakUnit) ## First # [nrow(x)] ## Last
    )
  )
  # Convert saved datetime vector back to POSIX* from int
  class(datetime) <- c("POSIXct", "POSIXt")
  attr(datetime, 'tzone') <- 'UTC'
  
  dateRange <- range(datetime)
  starttime <- MazamaCoreUtils::parseDatetime(dateRange[1], timezone = "UTC")
  endtime <- MazamaCoreUtils::parseDatetime(dateRange[2], timezone = "UTC")

  # Create dataframe with continuous axis
  datetimeAxis <- dplyr::tibble('datetime' = seq(starttime, endtime, by = seqBreakUnit))
  
  # ----- Assemble 'data' ------------------------------------------------------
  
  # Map each binned hourly data.frame to the user defined lambda-like 
  # function f applied via apply to each vector in the mapped data.frame
  mapped <- base::Map(
    df_bins,
    f = function(df, f = FUN) { f(df) }
  )
  
  hourlyDataMatrix <-
    do.call(rbind, mapped)
    
  # Add mapped data to pa_timeseries object with aggregate datetime axis
  data <- 
    data.frame(
      'datetime' = datetime, 
      hourlyDataMatrix, 
      'datetime_A' = datetime, 
      'datetime_B' = datetime
    ) %>%
    # Cleanup any NaN or Inf that might have snuck in
    dplyr::mutate_all( function(x) replace(x, which(is.nan(x)), NA) ) %>%
    dplyr::mutate_all( function(x) replace(x, which(is.infinite(x)), NA) )
  
  data <- dplyr::left_join(datetimeAxis, data, by = 'datetime', copy = TRUE)

  # ----- Return ---------------------------------------------------------------

  return(data)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  library(AirSensor)
  pas <- pas_load()
  pat <- pat_createNew(label = "SCEM_07", pas = pas)
  
  # Check out this pat
  dim(pat$data)
  pat_multiplot(pat)
  
  # Now debug by stepping into this function
  df <- pat$data
  FUN <- function(x) {
    htest <- stats::t.test(x$pm25_A, x$pm25_B, paired = FALSE)
    mat <- dplyr::tibble(
      t_score = as.numeric(htest$statistic),
      p_value = as.numeric(htest$p.value),
      df_value = as.numeric(htest$parameter)
    )
    return(mat)
  }
  unit <- 'minutes'
  count <- 60

  # Trying lines 90-93 from PurpleAirQC_hourly_AB_01.R
  ttestData <-
    pat %>%
    pat_extractData() %>%
    patData_aggregate(FUN)
  
  
}
