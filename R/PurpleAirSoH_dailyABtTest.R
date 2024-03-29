#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains 
#' 
#' @title Daily t-test
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' 
#' @description This function calculates a t-test between
#' the \code{pm25_A}, \code{pm25_B}. A t-statistic and a p-value will be 
#' returned for each day. All returned values are expected to hover near 0 for a 
#' properly functioning sensor. The t-statistic and p-value serve to test whether 
#' or not the \code{pm25_A} and \code{pm25_B} data are significantly different 
#' based on a student’s t-test.
#' 
#' @examples  
#' library(AirSensor)
#' 
#' tbl <- 
#'   example_pat %>%
#'   PurpleAirSoH_dailyABtTest() 
#'   
#' timeseriesTbl_multiPlot(tbl)

PurpleAirSoH_dailyABtTest <- function(
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
  localTime <- lubridate::with_tz(pat$dat$datetime, tzone = timezone)
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
  
  
  # ----- Calculate dailyABtTest -------------------------------------------
  
  # Notes on t-testing:
  #   Paired t-testing is when you compare the means of the SAME population, 
  # twice, once before and once after some change. An example of this scenario 
  # would be to test if the same group of people ran faster in pants or shorts.
  #   Two sample t-test is when you are comparing the means of two DIFFERENT 
  # populations. This is what we want, an example of this would be our pm25 
  # scenario.
  #   P-values: If we are testing that pm25A and pm25B are the same, we want the 
  # p-value to be high. For example, a typical p-value of say, p = 0.05,  would 
  # suggest that there is a 5% chance that the two populations are different 
  # purely by chance, which indicates that there is some substantial reason that 
  # the means of the two populations are different. Normally, this is what 
  # scientists are looking for; a reason that explains differences. But we want 
  # the opposite here. The higher the p-value, the higher the probability that 
  # the difference in means is due to chance and not due to some interference, 
  # which is what we want in this scenario.
  
  
  # Begin calculations:
  tbl <-
    pat$data  
  
  
  tbl <-
    tbl %>%
    # Group by day so that the t-test can be applied to a local 24 hour day
    dplyr::mutate(localDaystamp = strftime(.data$datetime, "%Y%m%d", 
                                           tz = timezone))
  
  # Preallocate a list
  ttest_list <- list()
  
  # Pat try block
  result <- try({
    # Loop through each unique day in the dataset
    for ( day in unique(tbl$localDaystamp) ) {
      
      # pull out the data associated with one day at a time
      day_tbl <- 
        dplyr::filter(tbl, .data$localDaystamp == day)
      
      # Daily try block to try to test if tests can be calculated on a daily basis 
      result <- try({
        tlist <- t.test(day_tbl$pm25_A, day_tbl$pm25_B, paired = FALSE)
        pm25_A_pm25_B_p_value <- as.numeric(tlist$p.value)
        
      }, silent = TRUE)
      
      # If the daily try block comes back error free, fill day with the fit values
      if ( ! "try-error" %in% class(result) ) {
        result <- try({
          # add the r-squared per day, per variable comparison to a list
          ttest_list[[day]] <- list(
            pm25_A_pm25_B_p_value = pm25_A_pm25_B_p_value
          )
        }, silent = TRUE)
      }
      
      # If the daily try block comes back with error, fill day with NA
      if ( "try-error" %in% class(result) ) {
        ttest_list[[day]] <- list(
          pm25_A_pm25_B_p_value = as.numeric(NA)
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
      int_ttest_tbl <- dplyr::as_tibble(ttest_list)
      
      #  transpose and reformat as a matrix in order to have the desired outcome after transpose
      ttest_matrix <- t(as.matrix(int_ttest_tbl))
      
      # reformat as a data.frame in order to change datetime to POSIXCT and add the colnames
      ttest_df <- data.frame(ttest_matrix)
      
      # reformat to tibble to change the datetime from rownames to an actual column of data
      ttest_df <- tibble::rownames_to_column(ttest_df, var="datetime") 
      
      # change datetime into a POSIXCT 
      ttest_df$datetime <- MazamaCoreUtils::parseDatetime(ttest_df$datetime, 
                                                             timezone = timezone)
      
      # add column names
      colnames <- c( "datetime", "pm25_A_pm25_B_p_value")
      
      colnames(ttest_df) <-colnames
      
      # re-define each of the columns as numeric rather than lists for easier plotting in ggplot
      ttest_df$pm25_A_pm25_B_p_value <- as.numeric(ttest_df$pm25_A_pm25_B_p_value)
      ttest_df <- 
        ttest_df %>%
        dplyr::mutate_if(is.numeric, ~replace(., is.nan(.), as.numeric(NA)))
      
      # join with empty daily column to flag missing days
      ttest_df <- dplyr::left_join(days, ttest_df, by = "datetime")
    
    }, silent = TRUE)
  }
  
  # If the pat try block comes back with errors, fill with NA's
  if ( "try-error" %in% class(result) ) {
    ttest_df <- 
      days %>%
      dplyr::mutate(pm25_A_pm25_B_p_value = as.numeric(NA))
  }
  # ----- Return ---------------------------------------------------------------
  
  return(ttest_df)
  
}






