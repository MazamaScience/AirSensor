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
#' library(AirSensor)
#' 
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
  
  # Grab the first day with data and end on the last day with data, partial days
  # will have tapered results. Either provide full days or trim to full days
  # after the fact.
  timezone <- pat$meta$timezone
  localTime <- lubridate::with_tz(pat$data$datetime, tzone = timezone)
  hour <- lubridate::hour(localTime)
  
  range <- range(localTime)
  start <- lubridate::floor_date(range[1], unit = "day")
  end <- lubridate::ceiling_date(range[2], unit = "day")
  
  
  # Filter the pat based on the times established above.
  pat <- pat_filterDate(
    pat, 
    startdate = start, 
    # enddate = end + lubridate::ddays(1),
    enddate = end,
    timezone = timezone
  )

  
  # Create daily tibble based on date range to join later.
  # This will ensure that missing records from valid_tbl will have NA.
  days <- dplyr::tibble(
    # Special function to handle daylight savings transitions
    datetime = MazamaCoreUtils::dateSequence(start, end - lubridate::ddays(1), timezone = timezone)
  )
  # ----- Calculate linear models ----------------------------------------------
  
  # Begin calculations:
  tbl <-
    pat$data 
  
  tbl <-
    tbl %>%
    # Group by day so that the linear models can be applied to a local 24 hour day
    dplyr::mutate(localDaystamp = strftime(.data$datetime, "%Y%m%d", 
                                           tz = timezone)) 
  
  # Preallocate a list
  ABFit_list <- list()
  
  # Pat try block
  result <- try({
    # Loop through each unique day in the dataset
    for ( day in unique(tbl$localDaystamp) ) {
      
      # pull out the data associated with one day at a time
      day_tbl <- 
        dplyr::filter(tbl, .data$localDaystamp == day)
      
      # Daily try block to try to test if fits can be calculated on a daily basis 
      result <- try({
        # calculate the ab slope and intercept
        model <- lm(day_tbl$pm25_A ~ day_tbl$pm25_B, subset = NULL, weights = NULL)
        model_summary <- summary(model)
        pm25_A_pm25_B_rsquared <- as.numeric(model_summary$r.squared)
        pm25_A_pm25_B_slope <- as.numeric(model$coefficients[2])  # as.numeric() to remove name
        pm25_A_pm25_B_intercept <- as.numeric(model$coefficients[1])
        
      }, silent = TRUE)
      
      # If the daily try block comes back error free, fill day with the fit values
      if ( ! "try-error" %in% class(result) ) {
        result <- try({
          # add the linear model per day, per variable comparison to a list
          ABFit_list[[day]] <- list(
            pm25_A_pm25_B_rsquared = pm25_A_pm25_B_rsquared,
            pm25_A_pm25_B_slope = pm25_A_pm25_B_slope,
            pm25_A_pm25_B_intercept = pm25_A_pm25_B_intercept
          )
        }, silent = TRUE)
      }
      
      # If the daily try block comes back with error, fill day with NA
      if ( "try-error" %in% class(result) ) {
        ABFit_list[[day]] <- list(
          pm25_A_pm25_B_rsquared = as.numeric(NA),
          pm25_A_pm25_B_slope = as.numeric(NA),
          pm25_A_pm25_B_intercept = as.numeric(NA)
        )
      }
      
    }
  }, silent = TRUE)
  
  # If the pat try block comes back error free, continue calculations
  if ( ! "try-error" %in% class(result) ) {
    result <- try({
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
      
    }, silent = TRUE)
  }
  
  # If the pat try block comes back with errors, fill with NA's
  if ( "try-error" %in% class(result) ) {
    ABFit_df <- 
      days %>%
      dplyr::mutate(pm25_A_pm25_B_rsquared = as.numeric(NA)) %>%
      dplyr::mutate(pm25_A_pm25_B_slope = as.numeric(NA)) %>%
      dplyr::mutate(pm25_A_pm25_B_intercept = as.numeric(NA)) 
  }
  # ----- Return ---------------------------------------------------------------
  
  return(ABFit_df)
  
}






