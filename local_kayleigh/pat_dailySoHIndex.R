#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains 
#' 
#' @title Daily State of Health metric plot
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' 
#' @description This function plots as subset of the most useful State of Health 
#' metrics calculated by the \code{pat_dailySoH} function. The function 
#' runs \code{pat_dailySoH} internally and uses the output to create 
#' the plot.
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
  localTime <- lubridate::with_tz(pat$dat$datetime, tzone = timezone)
  hour <- lubridate::hour(localTime)
  
  # NOTE:  Trim to full days unless we have less than a day or intermittent.
  # NOTE:  In that case, expand to encompass all data.
  
  if ( any(hour == 0) ) {
    start <- lubridate::floor_date(localTime[ min(which(hour == 0)) ], unit = "hour")
  } else {
    start <- lubridate::floor_date(localTime[1], unit = "day")
  }
  
  if ( any(hour == 23) ) {
    end <- lubridate::floor_date(localTime[ max(which(hour == 23)) ], unit = "hour")
  } else {
    end <- lubridate::ceiling_date(localTime[length(localTime)], unit = "day")
  }
  
  endtime = end + lubridate::dhours(1)
  
  # NOTE:  pat_filterDate only goes to the beginning of enddate and we want it
  # NOTE:  to go to the end of enddate.
  
  # Filter the pat based on the times established above.
  pat <- pat_filterDate(
    pat,
    startdate = start,
    enddate = end + lubridate::ddays(1)
  )
  
  # NOTE:  seq.Date(..., by = "day") operates by repeatedly adding 24 hours
  # NOTE:  which means that when we switch to/from daylight savings we end up
  # NOTE:  no longer on the midnight local time day boundary. Hence the
  # NOTE:  following workaround
  
  datetime <- 
    seq(start, end, by = "day") %>% 
    strftime("%Y%m%d", tz = timezone) %>%
    MazamaCoreUtils::parseDatetime(timezone = timezone)
  
  # Create daily tibble based on date range to join with the valid_tbl.
  # This will ensure that missing records from valid_tbl will have NA.
  index_tbl <- dplyr::tibble(
    # Special function to handle daylight savings transitions
    datetime = MazamaCoreUtils::dateSequence(start, end, timezone = timezone)
  ) %>%
    mutate(SoH_index = as.numeric(NA))
  
  # ----- Create the SoH object -----------------------------------------------
  
  # calculate the SoH 
  SoH <- pat_dailySoH(pat) %>%
    mutate(SoH_index = as.numeric(NA))
  
  
  # ------ Conditions for indexing --------------------------------------------
  
  
  SoH <-
    SoH %>%
    dplyr::mutate(SoH_index = case_when(
      pm25_A_pctReporting <= 10 & pm25_B_pctReporting <= 10 ~ 0, # case 1 - dead
      pm25_A_pm25_B_p_value <= 1e-80 & pm25_A_pm25_B_rsquared <= 0.1 ~ 0,
      pm25_A_pctReporting >= 95 & pm25_A_pctReporting >= 95 &
        pm25_A_pm25_B_p_value < 1e-20 & pm25_A_pm25_B_rsquared >= 0.5 ~ 1, # case 2 - great
      
      TRUE ~ as.numeric(.5))) #the "else" condition, make average.
  
  index_tbl$SoH_index <- SoH$SoH_index
  # 
  
  # ----- Return --------------------------------------------------------------
  return(index_tbl)
  
}






