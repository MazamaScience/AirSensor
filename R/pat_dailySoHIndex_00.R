#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains 
#' 
#' @title Daily State of Health indexing 
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param breaks Breaks used to convert index values into index bins.
#' 
#' @description This function calculates the \code{pat_dailySoH} function and 
#' returns a tibble containing a state of health index for each day of the 
#' \code{pat} provided. The returned tibble contains columns: \code{datetime}, 
#' \code{SoH_index}, and \code{SoH_index_bin}.
#' 
#' The \code{SoH_index} is a value normalized between 
#' 0 and 1 where 0 represents low confidence in the sensor data and 1 represents 
#' high confidence. The \code{SoH_index_bin} is either 1, 2, or 3 representing
#' poor, fair, and good data respectively. 
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
  
  # The 00 version is based on AB r squared
  SoHIndex <- 
    SoH %>%
    dplyr::select(.data$datetime, .data$pm25_A_pm25_B_rsquared) %>%
    dplyr::rename("index" = "pm25_A_pm25_B_rsquared") %>%
    # Replace NA with 0
    dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
  # Mark any days with < 30% reporting as poor
  mask <- SoH$pm25_A_pctReporting < 30 | SoH$pm25_B_pctReporting < 30
  SoHIndex$index[mask] <- 0

  # Use breaks to create index_bin
  SoHIndex$index_bin <- .bincode(SoHIndex$index, breaks, include.lowest = TRUE)

  # ----- Return ---------------------------------------------------------------
  
  return(SoHIndex)
  
}






