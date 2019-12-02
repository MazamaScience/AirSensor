#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains 
#' 
#' @title Daily State of Health indexing 
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' 
#' @description This function calculates the \code{pat_dailySoH} function and 
#' returns a tibble containing a state of health index for each day of the 
#' \emph{pat} provided. The tibble contains a \emph{datetime}, \emph{SoH_index}, 
#' and \emph{SoH_index_bin}. The \emph{SoH_index} is a value normalized between 
#' 0 and 1 where 0 represents low confidence in the sensor data and 1 represents 
#' high confidence. The \emph{SoH_index_bin} is either 0, 1, or 2 representing
#' poor, fair, and good data respectively. 
#' 
#' @examples  
#' tbl <- 
#'   example_pat_failure_A %>%
#'   pat_dailySoHIndex() 
#'   
#' 
#' 


pat_dailySoHIndex <- function(
  pat = NULL
) {
  
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  
  
  # ----- Create daily tbl -----------------------------------------------------
  
  # Get full days in the local timezone
  timezone <- pat$meta$timezone
  localTime <- lubridate::with_tz(pat$data$datetime, tzone = timezone)
  hour <- lubridate::hour(localTime)
  
  range <- range(localTime)
  start <- lubridate::floor_date(range[1], unit = "day")
  end <- lubridate::ceiling_date(range[2], unit = "day")
  
  pat <- pat_filterDate(
    pat, 
    startdate = start, 
    enddate = end,
    timezone = timezone
  )
  
  
  # Create daily tibble based on date range to join later.
  # This will ensure that missing records from valid_tbl will have NA.
  index_tbl <- dplyr::tibble(
    # Special function to handle daylight savings transitions
    datetime = MazamaCoreUtils::dateSequence(start, end - lubridate::ddays(1), timezone = timezone)
  ) 
  # ----- Create the SoH object -----------------------------------------------
  
  # calculate the SoH 
  SoH <- pat_dailySoH(pat) 
  
  # ------ Conditions for indexing --------------------------------------------
  
  # set the SoH_index equal to the r-squared
  SoH <- 
    SoH %>%
    dplyr::mutate(SoH_index = .data$pm25_A_pm25_B_rsquared)
  
  # set mask for when sensor is off and not recording
  mask <- SoH$pm25_A_pctReporting <= 30 & SoH$pm25_B_pctReporting >= 30 
  
  SoH$SoH_index[mask] <- 0
  
  # set bounds for bins and categorize each day accordingly
  SoH <- 
    SoH %>%
    dplyr::mutate(SoH_index_bin = case_when(
      .data$SoH_index <= 0.2 ~ 0,
      .data$SoH_index > 0.2 & .data$SoH_index < 0.8 ~ 1,
      .data$SoH_index > 0.8 ~ 2))
  
  # add the indexing columns to the index_tbl
  index_tbl$SoH_index <- SoH$SoH_index
  index_tbl$SoH_index_bin <- SoH$SoH_index_bin
  
  
  # ----- Return --------------------------------------------------------------
  return(index_tbl)
  
}






