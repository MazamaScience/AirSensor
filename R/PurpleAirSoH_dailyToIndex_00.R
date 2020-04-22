#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains
#' 
#' @title Daily state of health index
#' 
#' @param SoH PurpleAir daily State-of-Health dataframe.
#' @param minPctReporting Percent reporting threshold for A and B channels.
#' @param breaks Breaks used to convert index values into index bins.
#' 
#' @description This function calculates a multi-metric index based on the data
#' in \code{SoH} dataframe passed in. A tibble is returned containing a state of 
#' health index for each day. The returned tibble contains columns: 
#' \code{datetime}, \code{index}, and \code{index_bin}.
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
#'   pat_dailySoH() %>%
#'   PurpleAirSoH_dailyToIndex_00() 
#'   
#' head(tbl)

PurpleAirSoH_dailyToIndex_00 <- function(
  SoH = NULL,
  minPctReporting = 50,
  breaks = c(0, .2, .8, 1)
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(SoH)
  
  # ----- Create the SoHIndex tibble -------------------------------------------
  
  # The 00 version is based on AB r squared
  SoHIndex <- 
    SoH %>%
    dplyr::select(.data$datetime, .data$pm25_A_pm25_B_rsquared) %>%
    dplyr::rename("index" = "pm25_A_pm25_B_rsquared") %>%
    # Replace NA with 0
    dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
  # Mark any days with < minPctReporting as poor
  mask <- SoH$pm25_A_pctReporting < minPctReporting | SoH$pm25_B_pctReporting < minPctReporting
  SoHIndex$index[mask] <- 0
  
  # Use breaks to create index_bin
  SoHIndex$index_bin <- .bincode(SoHIndex$index, breaks, include.lowest = TRUE)
  
  # ----- Return ---------------------------------------------------------------
  
  return(SoHIndex)
  
}












