#' @export
#' @importFrom rlang .data
#' 
#' @title Trim a PurpleAir Timeseries object to full days
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' 
#' @description Trims the date range of a \emph{pat} object to local time date
#' boundaries which are \emph{within} the range of data. This has the effect
#' of removing partial-day data records and is useful when calculating
#' full-day statistics.
#' 
#' @return A subset of the given \emph{pat} object.
#' 
#' @examples
#' library(AirSensor)
#' 
#' UTC_week <- pat_filterDate(example_pat, 
#'                            startdate = 20180808, 
#'                            enddate = 20180815,
#'                            timezone = "UTC")
#' pat_multiplot(UTC_week)
#'                               
#' local_week <- pat_trimDate(UTC_week)
#' pat_multiplot(local_week)
#'

pat_trimDate <- function(
  pat = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  # ----- Get the start and end times ------------------------------------------
  
  timeRange <- range(pat$data$datetime)
  timezone <- pat$meta$timezone
  
  # NOTE:  The dateRange() function will automatically floor() both the enddate 
  # NOTE:  and the startdate. This is appropriate for enddate but we now have
  # NOTE:  to add a day to startdate to get it 'inside' the data range.
  
  dateRange <-
    MazamaCoreUtils::dateRange(timeRange[1], timeRange[2], timezone = timezone)
  dateRange[1] <- dateRange[1] + lubridate::ddays(1)

  # ----- Subset the "pat" object ----------------------------------------------
  
  data <- 
    pat$data %>%
    dplyr::filter(.data$datetime >= dateRange[1]) %>%
    dplyr::filter(.data$datetime < dateRange[2])
  
  pat$data <- data
  
  # ----- Return ---------------------------------------------------------------
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  return(pat)
  
}
