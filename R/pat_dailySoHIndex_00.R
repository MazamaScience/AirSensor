#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains 
#' 
#' @title State of Health index plot
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param minPctReporting Percent reporting threshold for A and B channels.
#' @param breaks Breaks used to convert index values into index bins.
#' 
#' @description This function calculates the \code{pat_dailySoH} function and 
#' returns a tibble containing a state of health index for each day of the 
#' \code{pat} provided. The returned tibble contains columns: \code{datetime}, 
#' \code{index}, and \code{index_bin}.
#' 
#' The \code{index} column contains a value normalized between 
#' 0 and 1 where 0 represents low confidence in the sensor data and 1 represents 
#' high confidence. The \code{index_bin} is one of 1, 2, or 3 and represents
#' poor, fair, and good data respectively. 
#' 
#' The \code{index} is calculated in the following manner:
#' 
#' \enumerate{
#' \item{If the A or B channel percent reporting is < \code{minPctReporting}, \code{index = 0}}
#' \item{Otherwise, \code{index = pm25_A_pm25_B_rsquared}}
#' }
#' 
#' The \code{breaks} are used to convert \code{index} into the \code{indenx_bin}
#' poor-fair-good values.
#' 
#' @examples  
#' library(AirSensor)
#' 
#' tbl <- 
#'   example_pat_failure_A %>%
#'   pat_dailySoHIndex_00() 
#'   
#' head(tbl)
#'   

pat_dailySoHIndex_00 <- function(
  pat = NULL,
  minPctReporting = 50,
  breaks = c(0, .2, .8, 1)
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  
  # ----- Prepare data ---------------------------------------------------------
  
  # Get full days in the local timezone
  timezone <- pat$meta$timezone
  localTime <- lubridate::with_tz(pat$data$datetime, tzone = timezone)

  range <- range(localTime)
  start <- lubridate::floor_date(range[1], unit = "day")
  end <- lubridate::ceiling_date(range[2], unit = "day")
  
  # Trim the pat object to local time complete days
  pat <- pat_filterDate(
    pat, 
    startdate = start, 
    enddate = end,
    timezone = timezone
  )
  
  # Calculate State-of-Health metrics
  SoH <- pat_dailySoH(pat)
  
  # ----- Create the SoHIndex tibble -------------------------------------------
  
  SoHIndex <- PurpleAirSoH_dailyToIndex_00(SoH, minPctReporting, breaks)

  # ----- Return ---------------------------------------------------------------
  
  return(SoHIndex)
  
}






