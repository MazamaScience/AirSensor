#' @keywords pa_timeseries
#' @export
#' @importFrom rlang .data
#' @importFrom stats aggregate median na.omit quantile sd t.test time
#'
#' @title Aggregate PurpleAir Timeseries objects
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param FUN The function to be applied to each vector of numeric \code{pat} data.
#' @param unit Character string specifying temporal units for binning.
#' @param count Number of units per bin.
#' @param level Context level to evaluate FUN on. 
#' 
#' @description Aggregate PurpleAir timeseries (\emph{pat}) objects along its 
#' datetime axis. Temporal aggregation involves splitting a \emph{pat} object into
#' separate bins along its datetime axis, applying a function to the split data, 
#' and then recombining the data along a new aggregate datetime axis.
#' 
#' The recommended and default level (\code{level = 2}) 
#' \code{FUN} is mapped to the \code{pat} _numeric_ variables in each bin, 
#' which are then recombined into an aggregated \emph{pat} object containing 
#' the same metadata as the incoming \code{pat}. The returned object is 
#' effectively a \emph{pa_timeseries} object that will behave similarly to 
#' un-aggregated \emph{pat} objects throughout the package. \code{level = 2} is 
#' the recommended context level to operate on if preforming quick vectorised 
#' numeric operations (e.g. \code{mean}, \code{min}, \code{var}, etc.). 
#' 
#' To operate on the incoming \code{pat} data on a self-object _level_, set \code{level = 1}. 
#' This more advanced option is provided to allow explicit \emph{pa_timeseries} 
#' object data manipulation while aggregating. Setting \code{level = 1} allows 
#' \code{FUN} to access and operate on all numeric \code{pat} data. Setting \code{level = 1} requires 
#' explicit function calls and return operations, and therefore will always 
#' \code{FUN} execution call with an attached aggregate datetime axis, 
#'  _not_ a \emph{pa_timeseries} object. 
#' 
#' @details \code{FUN} must operate on univariate numeric vectors and return a 
#' scalar value. Besides the data variable, no additional arguments will be 
#' provided to this function. This means that functions like \code{mean} and
#' \code{max} will need to be wrapped in a function that specifies 
#' \code{na.rm = TRUE}. See the examples below.
#' 
#' \code{level} specifies the context level for \code{FUN} to operate on. To 
#' aggregate \code{pat} data where \code{FUN} must operate of access multiple 
#' columns (unvectorised) use \code{level = 1}. Otherwise, use vecotrised method, 
#' \code{level = 2}.
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
#'   
#' # Two Sample Student T-Test (advanced users only - see details.)
#' FUN_ttest <- function(x) {
#'   t.test(x$pm25_A, x$pm25_B)
#' }
#' 
#' pat %>% 
#'   pat_aggregate(FUN_ttest, level = 1) 
#'   

pat_aggregate <- function(
  pat, 
  FUN = function(x) { mean(x, na.rm = TRUE) }, 
  unit = "minutes",
  count = 60, 
  level = 2
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  MazamaCoreUtils::stopIfNull(FUN)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
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
  
  # Get the first index of aligned time for future use.
  datetime <- as.numeric(
    lapply(
      X = patData_bins, 
      # Select first datetime index in bin to use as aggregated datetime axis
      FUN = function(x) zoo::index(x)[1] ## First # [nrow(x)] ## Last
    )
  )
  # Convert saved datetime vector back to posix* from int
  class(datetime) <- c("POSIXct", "POSIXt")
  attr(datetime, 'tzone') <- 'UTC'
  
  # Determine level FUN execution context.
  # NOTE: level = 1 is to operate on the patData dataframe, with access 
  # NOTE: to it's object self. 
  # NOTE: level = 2 is operate vectorised data (each col of patData) individually.
  # NOTE:  See ?Map and also ?mapply to understand the mapping below.

  if ( level == 1L ) { # Advanced 
    
    # Map the function FUN to each bin data.frame. 
    mapped <- base::Map(
      patData_bins,
      f = function(patData, f = FUN) { f(patData) }
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
    
  } else if ( level == 2L ) { # Default
    
    # Map each binned hourly data.frame to the user defined lambda-like 
    # function f applied via apply to each vector in the mapped data.frame
    mapped <- base::Map(
      patData_bins, 
      f = function(patData, f = FUN) { apply(patData, 2, f) } 
    )
    
    # Add mapped data to pa_timeseries object with aggrgeate datetime axis
    pat$data <- data.frame(
      'datetime' = datetime, 
      do.call(rbind, mapped), 
      'datetime_A' = datetime, 
      'datetime_B' = datetime
    )
    
    return(pat)
    
  } else {
    
    stop('Invalid level for FUN.')
    
  }

}
