#' @keywords pa_timeseries
#' @export
#' @importFrom rlang .data
#' @importFrom stats aggregate median na.omit quantile sd t.test time
#'
#' @title Aggregation statistics for PurpleAir Timeseries objects
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param FUN The function to be applied to each vector of numeric \code{pat} data.
#' @param ... (optional) Additional arguments for \code{FUN}.
#' 
#' @description Calculates statistics associated with the aggregation of raw 
#' PurpleAir Timeseries data onto a regular time axis.
#' 
#' Temporal aggregation involves creating time period bins defined by
#' \code{period} and then calculating the statistics associated with the raw
#' data measurements that fall within each bin. The result is a dataframe with
#' a regular time axis and multiple columns of output for every column of
#' input.
#' 
#' 
#' FUN is found by a call to match.fun and typically is specified as a function 
#' or a symbol (e.g., a backquoted name) or a character string specifying a 
#' function to be searched for from the environment of the call to.
#' 
#' @return Returns a dataframe with aggregation statistics.
#' 
#' @examples
#' \dontrun{
#' avg_pat <- pat_aggregate(pat, mean, na.rm = TRUE)
#' }

pat_aggregate <- function(
  pat, 
  FUN = mean, 
  ...
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
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
  df <- xts::xts(
    x = pat$data[numeric_cols], 
    order.by = pat$data$datetime, 
    unique = TRUE, 
    tzone = 'UTC'
  )

  # Split the xts into list of hourly binned xts matrices  
  # NOTE: By not being explicit about period parameter f we can allow the use 
  # NOTE: of ... to add more flexibility when aggregating. For example, if an 
  # NOTE: advanced/clever user wants to aggregate by 30 min they can explicitly 
  # NOTE: provide f = 'minutes' and k = 30 as parameters.
  df_bins <- xts::split.xts(df, 'hours', drop = FALSE, ...)
  
  # Align the binned data frames to its rounded next hour period (60 * 60 sec)
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
  
  # Map each binned hourly data.frame to the user defined lambda-like 
  # function f applied via apply to each vector in the mapped data.frame
  mapped <- Map( 
    df_bins, 
    f = function(x, f = FUN) {
      apply(
        X = x, 
        MARGIN = 2, 
        FUN = function(x_, ...) { f(x_, ...) }
      )
    }
  )
  
  # ----- Bind & Return PAT ----------------------------------------------------
  
  # Add the rbind mapped data back to the pat object to preserve flexibility 
  # and consistency among the package. 
  pat$data <- data.frame(
    'datetime' = datetime, 
    do.call(rbind, mapped), 
    'datetime_A' = datetime, 
    'datetime_B' = datetime
  )
  
  # Return 
  return(pat)
}
