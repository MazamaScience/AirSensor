#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains 
#' 
#' @title Daily linear model fit values
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' 
#' @description This function calculates daily linear model values between
#' the \code{pm25_A} and \code{pm25_B} channels. A daily r-squared value is
#' returned in addition to the coefficients of the linear fit (slope and 
#' intercept) 
#' 
#' 
#' @examples  
#' tbl <- 
#'   example_pat_failure_A %>%
#'   PurpleAirSoH_dailyABFit() 
#'   
#' timeseriesTbl_multiplot(
#'   tbl, 
#'   ylim = c(-1,1), 
#'   style = "line"
#' )
#' 


PurpleAirSoH_dailyABFit <- function(
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
  start <- lubridate::floor_date(localTime[ min(which(hour == 0)) ], unit = "hour")
  end <- lubridate::floor_date(localTime[ max(which(hour == 23)) ], unit = "hour")
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
  # NOTE:  following workaround:
  
  datetime <- 
    seq(start, end, by = "day") %>% 
    strftime("%Y%m%d", tz = timezone) %>%
    MazamaCoreUtils::parseDatetime(timezone = timezone)
  
  # Create daily tibble based on daterange to join with the valid_tbl and 
  # flag missing data
  days <- tibble(datetime = datetime) 
  
  # ----- Calculate linear models ----------------------------------------------
  
  # Begin percent DC calculations:
  pct_tbl <-
    pat$data #%>%  
    
  # Put it on a local time axis and trim
  pct_tbl$datetime <- lubridate::with_tz(pct_tbl$datetime, tzone = timezone)
  pct_tbl <- dplyr::filter(pct_tbl, .data$datetime >= start & .data$datetime <= endtime)
  
  pct_tbl <-
    pct_tbl %>%
    # Group by day so that the linear models can be applied to a local 24 hour day
    dplyr::mutate(localDaystamp = strftime(.data$datetime, "%Y%m%d", 
                                           tz = timezone)) 
  
  # Preallocate a list
  ABFit_list <- list()
  
  # Loop through each unique day in the dataset
  for ( day in unique(pct_tbl$localDaystamp) ) {
    
    # pull out the data associated with one day at a time
    day_tbl <- 
      dplyr::filter(pct_tbl, .data$localDaystamp == day)
    
    ABFit_list[[day]] <- day_tbl
    
    # calculate the ab slope and intercept
    model <- lm(day_tbl$pm25_A ~ day_tbl$pm25_B, subset = NULL, weights = NULL)
    model_summary <- summary(model)
    pm25_A_pm25_B_rsquared <- as.numeric(model_summary$r.squared)
    pm25_A_pm25_B_slope <- as.numeric(model$coefficients[2])  # as.numeric() to remove name
    pm25_A_pm25_B_intercept <- as.numeric(model$coefficients[1])
    
    # add the linear model per day, per variable comparison to a list
    ABFit_list[[day]] <- list(
      pm25_A_pm25_B_rsquared = pm25_A_pm25_B_rsquared,
      pm25_A_pm25_B_slope = pm25_A_pm25_B_slope,
      pm25_A_pm25_B_intercept = pm25_A_pm25_B_intercept
    )
    
  }
  
  # TODO:  The following is messy and not the most efficient way to convert 
  # TODO:  from a list to a dataframe and certainly could be improved.
  
  # reformat the list as a tibble
  int_ABFit_tbl <- dplyr::as_tibble(ABFit_list)
  
  #  transpose and reformat as a matrix in order to have the desired outcome after transpose
  ABFit_matrix <- t(as.matrix(int_ABFit_tbl))
  
  # reformat as a data.frame in order to change datetime to POSIXCT and add the colnames
  ABFit_df <- data.frame(ABFit_matrix)
  
  # reformat to tibble to change the datetime from rownames to an actual column of data
  ABFit_df <- tibble::rownames_to_column(ABFit_df, var="datetime") 
  
  # change datetime into a POSIXCT 
  ABFit_df$datetime <- MazamaCoreUtils::parseDatetime(ABFit_df$datetime, 
                                                            timezone = timezone)
  
  # add column names
  colnames <- c( "datetime","pm25_A_pm25_B_rsquared","pm25_A_pm25_B_slope", "pm25_A_pm25_B_intercept")
  colnames(ABFit_df) <-colnames
  
  # re-define each of the columns as numeric rather than lists for easier plotting in ggplot
  ABFit_df$pm25_A_pm25_B_rsquared <- as.numeric(ABFit_df$pm25_A_pm25_B_rsquared)
  ABFit_df$pm25_A_pm25_B_slope <- as.numeric(ABFit_df$pm25_A_pm25_B_slope)
  ABFit_df$pm25_A_pm25_B_intercept <- as.numeric(ABFit_df$pm25_A_pm25_B_intercept)

  
  # join with empty daily column to flag missing days
  ABFit_df <- dplyr::left_join(days, ABFit_df, by = "datetime")
  
  # ----- Return ---------------------------------------------------------------
  
  return(ABFit_df)
  
}






