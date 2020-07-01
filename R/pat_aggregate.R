#' @keywords pa_timeseries
#' @export
#' @importFrom rlang .data
#' @importFrom stats aggregate median na.omit quantile sd t.test time
#'
#' @title Aggregate PurpleAir Timeseries Object
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param FUN The function to be applied to each vector of numeric \code{pat} data.
#' @param unit Character string specifying temporal units for binning.
#' @param count Number of units per bin.
#'
#' @description Aggregate PurpleAir timeseries (\emph{pat}) objects along its
#' datetime axis. Temporal aggregation involves splitting a \emph{pat} object into
#' separate bins along its datetime axis. \code{FUN} is mapped to the \emph{pat}
#' numeric variables in each bin, which are then recombined into an aggregated
#' \emph{pat} object containing the same metadata as the incoming \code{pat}.
#'
#' @details \code{FUN} must operate on univariate numeric vectors and return a
#' scalar value. Besides the data variable, no additional arguments will be
#' provided to this function. This means that functions like \code{mean} and
#' \code{max} will need to be wrapped in a function that specifies
#' \code{na.rm = TRUE}. See the examples below.
#'
#' @return Returns an aggregated \emph{pat} object.
#' 
#' @examples
#' library(AirSensor)
#' 
#' # Single day subset
#' pat <- 
#'   example_pat %>% 
#'   pat_filterDate(20180813, 20180814)
#' 
#' # Create aggregation functions
#' FUN_mean <- function(x) mean(x, na.rm = TRUE)
#' FUN_max <- function(x) max(x, na.rm = TRUE)
#' FUN_count <- function(x) length(na.omit(x))
#' 
#' # Hourly means
#' pat %>%
#'   pat_aggregate(FUN_mean) %>% 
#'   pat_extractData() %>% 
#'   dplyr::select(1:9)
#'
#' # Hourly maxes
#' pat %>%
#'   pat_aggregate(FUN_max) %>% 
#'   pat_extractData() %>% 
#'   dplyr::select(1:9)
#'
#' # Hourly counts
#' pat %>%
#'   pat_aggregate(FUN_count) %>% 
#'   pat_extractData() %>% 
#'   dplyr::select(1:9)
#'
#' # Alternative 10 minute aggregation (advanced users only - see details.)
#' pat %>%
#'   pat_aggregate(FUN_max, unit = "minutes", count = 10) %>%
#'   pat_extractData() %>%
#'   dplyr::select(1:9) %>%
#'   dplyr::slice(1:6)

pat_aggregate <- function(
  pat, 
  FUN = function(x) { mean(x, na.rm = TRUE) }, 
  unit = "minutes",
  count = 60
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  MazamaCoreUtils::stopIfNull(FUN)
  MazamaCoreUtils::stopIfNull(unit)
  MazamaCoreUtils::stopIfNull(count)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
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
  numeric_cols <- which(unlist(lapply(pat$data, is.numeric)))
  
  # Convert to eXtensible Time Series (xts) data.frame
  # Separate only useful data for calculation (i.e. only numeric)
  patData <- xts::xts(
    x = pat$data[numeric_cols], 
    order.by = pat$data$datetime, 
    unique = TRUE, 
    tzone = 'UTC'
  )
  
  # Split the xts into a list of binned xts matrices  
  patData_bins <- xts::split.xts(
    patData, 
    f = unit, 
    drop = FALSE, 
    k = count
  )
  
  # ----- Datetime Axis --------------------------------------------------------
  
  # Get the first index of aligned time for future use.
  datetime <- as.numeric(
    lapply(
      X = patData_bins, 
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
  # TODO: Currently hard-coded to only support hours. Update to parse count and unit. 
  # Create dataframe with continuous axis
  datetimeAxis <- dplyr::tibble('datetime' = seq(starttime, endtime, by = seqBreakUnit))

  # ----- Assemble 'data' ------------------------------------------------------
  
  # Map each binned hourly data.frame to the user defined lambda-like 
  # function f applied via apply to each vector in the mapped data.frame
  mapped <- base::Map(
    patData_bins, 
    f = function(patData, f = FUN) { apply(patData, 2, f) } 
  )
  
  hourlyDataMatrix <-
    do.call(rbind, mapped) %>%
    round(1) 
    
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
  
  pat$data <- data
  
  return(pat)
  
}

