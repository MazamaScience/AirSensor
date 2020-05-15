#' @keywords pat
#' @export
#' 
#' @title Test for correct structure in a \emph{pat} object
#' 
#' @param pat \emph{pat} object
#' 
#' @return \code{TRUE} if \code{pat} has the correct structure, \code{FALSE} otherwise.
#' 
#' @description The \code{pat} is checked for the 'pat' class name
#' and presence of core metadata columns:
#' \itemize{
#'   \item{ID -- Purple Air ID}
#'   \item{label -- location label}
#'   \item{sensorType -- PurpleAir sensor type}
#'   \item{longitude -- decimal degrees E}
#'   \item{latitude -- decimal degrees N}
#'   \item{timezone -- Olson timezone}
#'   \item{countryCode -- ISO 3166-1 alpha-2}
#'   \item{stateCode -- ISO 3166-2 alpha-2}
#'   \item{pwfsl_closestDistance -- distance in meters from an official monitor}
#'   \item{pwfsl_closestMonitorID -- identifer for the nearest official monitor}
#' }
#' 
#' The "pwfsl", official, monitors are obtained from the USFS AirFire site 
#' using the \pkg{PWFSLSmoke} R package.
#' 
#' @examples
#' pat_isPat(example_pat)
#'
pat_isPat <- function(
  pat = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  # Test a variety of things that could go wrong
  if ( is.null(pat) ) return(FALSE)
  if ( !"pa_timeseries" %in% class(pat) ) return(FALSE)
  
  if ( !"meta" %in% names(pat) ) return(FALSE)
  if ( !"data" %in% names(pat) ) return(FALSE)
  
  metaParameters <- c(
    'ID', 'label', 'sensorType',
    'longitude', 'latitude', 
    'timezone', 'countryCode', 'stateCode',
    'pwfsl_closestDistance',
    'pwfsl_closestMonitorID'
  )
  
  if ( !all(metaParameters %in% names(pat$meta)) ) return(FALSE)
  
  dataParameters <- c(
    "datetime", "pm25_A", "pm25_B",
    "temperature", "humidity",
    "uptime", "adc0", "rssi",
    "datetime_A", "datetime_B" 
  )
  
  if ( !all(dataParameters %in% names(pat$data)) ) return(FALSE)
  
  # Nothing failed so return TRUE
  return(TRUE)
  
}


#' @export
#' 
#' @title Test for an empty \emph{pat} object
#' 
#' @param pat \emph{pat} object
#' @return \code{TRUE} if no data exist in \code{pat}, \code{FALSE} otherwise.
#' @description Convenience function for \code{nrow(pat$data) == 0}.
#' This makes for more readable code in functions that need to test for this.
#' @examples
#' pat_isEmpty(example_pat)
#'
pat_isEmpty <- function(pat) {
  if (!pat_isPat(pat)) stop("Not a valid 'pat' object.")
  return( nrow(pat$data) == 0 )
}


#' @export
#' 
#' @title Retain only distinct data records in pat$data
#' 
#' @param pat \emph{pat} object
#' 
#' @return A \emph{pat} object with no duplicated data records.
#' 
#' @description Convenience wrapper for 
#' \code{pat$data <- dplyr::distinct(pat$data)}.
#' 
pat_distinct <- function(pat) {
  if (!pat_isPat(pat)) stop("Not a valid 'pat' object.")
  pat$data <- dplyr::distinct(pat$data)
  return( pat )
}


#' @title Extract dataframes from \emph{pat} objects
#'
#' @description
#' These functions are convenient wrappers for extracting the dataframes that
#' comprise a \emph{pat} object. These functions are designed to be useful when
#' manipulating data in a pipeline chain using \code{\%>\%}.
#'
#' Below is a table showing equivalent operations for each function.
#'
#' \tabular{ll}{
#'   \strong{Function} \tab \strong{Equivalent Operation}\cr
#'   \code{pat_extractData(pat)} \tab \code{pat[["data"]]}\cr
#'   \code{pat_extractMeta(pat)} \tab \code{pat[["meta"]]}
#' }
#'
#' @param pat \emph{pat} object to extract dataframe from.
#'
#' @return A dataframe from the given \emph{pat} object
#'
#' @name pat_extractDataFrame
#' @aliases pat_extractData pat_extractMeta
#'
NULL


#' @export
#' @rdname pat_extractDataFrame
#'
pat_extractData <- function(pat) {
  if (!pat_isPat(pat)) stop("Not a valid 'pat' object.")
  return(pat[["data"]])
}


#' @export
#' @rdname pat_extractDataFrame
#'
pat_extractMeta <- function(pat) {
  if (!pat_isPat(pat)) stop("Not a valid 'pat' object.")
  return(pat[["meta"]])
}

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
#' @description Aggregate a data.frame along its \emph{datetime} axis.
#' Temporal aggregation involves splitting a \emph{data.frame} object into
#' separate bins along its datetime axis. \code{FUN} is mapped to the \emph{df}
#' numeric variables in each bin, which are then recombined into an aggregated
#' data.frame.
#'
#' @details This function intended for advanced users who wish to have more
#' flexibility than the standard \emph{pat_aggregate()} while aggregating
#' timeseries data. \code{FUN} can operate and access all numeric vectors
#' within the data.frame \emph{df}.
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
#' FUN_ttest <- function(data) {
#'   t.test(data$pm25_A, data$pm25_B)
#' }
#'
#' pat %>%
#'   pat_extractData() %>% # Note: Extract the timeseries data.frame
#'   patData_aggregate(FUN_ttest)
patData_aggregate <- function(
  df,
  FUN = function(df) { mean(df$pm25_A + df$pm25_B, na.rm = TRUE) },
  unit = 'minutes',
  count = 60
) {

  MazamaCoreUtils::stopIfNull(df)
  MazamaCoreUtils::stopIfNull(FUN)
  MazamaCoreUtils::stopIfNull(unit)
  MazamaCoreUtils::stopIfNull(count)

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

  # Get the first index of aligned time for future use.
  datetime <- as.numeric(
    lapply(
      X = df_bins,
      # Select first datetime index in bin to use as aggregated datetime axis
      FUN = function(x) zoo::index(x)[1] ## First # [nrow(x)] ## Last
    )
  )
  # Convert saved datetime vector back to posix* from int
  class(datetime) <- c("POSIXct", "POSIXt")
  attr(datetime, 'tzone') <- 'UTC'

  # Map the function FUN to each bin data.frame.
  mapped <- base::Map(
    df_bins,
    f = function(df, f = FUN) { f(df) }
  )

  # Return a data.frame of aggregate data
  aggData <- data.frame(
    'datetime' = datetime,
    do.call(rbind, mapped),
    fix.empty.names = FALSE,
    check.rows = FALSE,
    check.names = FALSE
  )

  return(aggData)
}
