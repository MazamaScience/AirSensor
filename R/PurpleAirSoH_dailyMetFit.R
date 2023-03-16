#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains 
#' 
#' @title Daily fit values
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' 
#' @description This function calculates a daily linear model between
#' the \code{pm25_A}, \code{pm25_B}, \code{humidity}, and \code{temperature} 
#' channels. One r-squared value for each channel pair except \code{pm25_A}, 
#' \code{pm25_B}, and \code{humidity}, \code{temperature} will be returned for 
#' each day. All returned values are expected to hover near 0 for a properly
#' functioning sensor.
#' 
#' 

PurpleAirSoH_dailyMetFit <- function(
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
  
  # ----- Calculate dailyMetFit -------------------------------------------
  
  # Note: This function uses a combination of lm() and summary() to calculate 
  # the r-squared between several channels rather than using stats::cor() 
  # because lm() has a built-in method for handling incomplete pairing between
  # channels. To ground truth that this method produces the same results as 
  # stats::cor(), run this example:
  # setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
  # pas <- pas_load(archival = TRUE)
  # pat <- pat_createNew(pas, "SCAP_19",
  #                      startdate = "2019-09-10",
  #                      enddate = "2019-09-11",
  #                      timezone = "America/Los_Angeles")
  # cor_stats <- (stats::cor(pat$data$pm25_A, pat$data$temperature, use = "pairwise.complete.obs"))^2
  # lm_rsquared <- PurpleAirSoH_dailyMetFit(pat)
  # Notice that cor_stats and lm_rsquared$pm25_A_temperature_rsquared are the same.
  
  
  # Begin calculations:
  tbl <-
    pat$data  
  
  # Put it on a local time axis and trim
  # tbl$datetime <- lubridate::with_tz(tbl$datetime, tzone = timezone)
  # tbl <- dplyr::filter(tbl, .data$datetime >= start & .data$datetime <= endtime)
  
  tbl <-
    tbl %>%
    # Group by day so that the r-squared can be applied to a local 24 hour day
    dplyr::mutate(localDaystamp = strftime(.data$datetime, "%Y%m%d", 
                                           tz = timezone))
  
  # Preallocate a list
  rsquared_list <- list()
  
  # Pat try block
  result <- try({
    # Loop through each unique day in the dataset
    for ( day in unique(tbl$localDaystamp) ) {
      
      # pull out the data associated with one day at a time
      day_tbl <- 
        dplyr::filter(tbl, .data$localDaystamp == day)
      
      # Daily try block to try to test if fits can be calculated on a daily basis 
      result <- try({
        # calculate the r-squared between several variables
        pm25_A_humidity_model <- lm(day_tbl$pm25_A ~ day_tbl$humidity, subset = NULL, weights = NULL)
        pm25_A_humidity_model_summary <- summary(pm25_A_humidity_model)
        pm25_A_humidity_rsquared <- as.numeric(pm25_A_humidity_model_summary$r.squared)
        
        pm25_A_temperature_model <- lm(day_tbl$pm25_A ~ day_tbl$temperature, subset = NULL, weights = NULL)
        pm25_A_temperature_model_summary <- summary(pm25_A_temperature_model)
        pm25_A_temperature_rsquared <- as.numeric(pm25_A_temperature_model_summary$r.squared)
        
        pm25_B_humidity_model <- lm(day_tbl$pm25_B ~ day_tbl$humidity, subset = NULL, weights = NULL)
        pm25_B_humidity_model_summary <- summary(pm25_B_humidity_model)
        pm25_B_humidity_rsquared <- as.numeric(pm25_B_humidity_model_summary$r.squared)
        
        pm25_B_temperature_model <- lm(day_tbl$pm25_B ~ day_tbl$temperature, subset = NULL, weights = NULL)
        pm25_B_temperature_model_summary <- summary(pm25_B_temperature_model)
        pm25_B_temperature_rsquared <- as.numeric(pm25_B_temperature_model_summary$r.squared)
        
      }, silent = TRUE)
      
      # If the daily try block comes back error free, fill day with the fit values
      if ( ! "try-error" %in% class(result) ) {
        result <- try({
          # add the r-squared per day, per variable comparison to a list
          rsquared_list[[day]] <- list(
            pm25_A_humidity_rsquared = pm25_A_humidity_rsquared,
            pm25_A_temperature_rsquared = pm25_A_temperature_rsquared,
            pm25_B_humidity_rsquared = pm25_B_humidity_rsquared,
            pm25_B_temperature_rsquared = pm25_B_temperature_rsquared
          )
        }, silent = TRUE)
      }
      
      # If the daily try block comes back with error, fill day with NA
      if ( "try-error" %in% class(result) ) {
        rsquared_list[[day]] <- list(
          pm25_A_humidity_rsquared = as.numeric(NA),
          pm25_A_temperature_rsquared = as.numeric(NA),
          pm25_B_humidity_rsquared = as.numeric(NA),
          pm25_B_temperature_rsquared = as.numeric(NA)
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
      int_rsquared_tbl <- dplyr::as_tibble(rsquared_list)
      
      #  transpose and reformat as a matrix in order to have the desired outcome after transpose
      rsquared_matrix <- t(as.matrix(int_rsquared_tbl))
      
      # reformat as a data.frame in order to change datetime to POSIXCT and add the colnames
      rsquared_df <- data.frame(rsquared_matrix)
      
      # reformat to tibble to change the datetime from rownames to an actual column of data
      rsquared_df <- tibble::rownames_to_column(rsquared_df, var="datetime") 
      
      # change datetime into a POSIXCT 
      rsquared_df$datetime <- MazamaCoreUtils::parseDatetime(rsquared_df$datetime, 
                                                             timezone = timezone)
      
      # add column names
      colnames <- c( "datetime","pm25_A_humidity_rsquared", "pm25_A_temperature_rsquared",
                     "pm25_B_humidity_rsquared", "pm25_B_temperature_rsquared")
      
      colnames(rsquared_df) <-colnames
      
      # re-define each of the columns as numeric rather than lists for easier plotting in ggplot
      rsquared_df$pm25_A_temperature_rsquared <- as.numeric(rsquared_df$pm25_A_temperature_rsquared)
      rsquared_df$pm25_A_humidity_rsquared <- as.numeric(rsquared_df$pm25_A_humidity_rsquared)
      rsquared_df$pm25_B_temperature_rsquared <- as.numeric(rsquared_df$pm25_B_temperature_rsquared)
      rsquared_df$pm25_B_humidity_rsquared <- as.numeric(rsquared_df$pm25_B_humidity_rsquared)
      rsquared_df <- 
        rsquared_df %>%
        dplyr::mutate_if(is.numeric, ~replace(., is.nan(.), as.numeric(NA)))
      
      # join with empty daily column to flag missing days
      rsquared_df <- dplyr::left_join(days, rsquared_df, by = "datetime")
    
    }, silent = TRUE)
  }
  
  # If the pat try block comes back with errors, fill with NA's
  if ( "try-error" %in% class(result) ) {
    rsquared_df <- 
      days %>%
      dplyr::mutate(pm25_A_temperature_rsquared = as.numeric(NA)) %>%
      dplyr::mutate(pm25_A_humidity_rsquared = as.numeric(NA)) %>%
      dplyr::mutate(pm25_B_temperature_rsquared = as.numeric(NA)) %>%
      dplyr::mutate(pm25_B_humidity_rsquared = as.numeric(NA))
  }
  # ----- Return ---------------------------------------------------------------
  
  return(rsquared_df)
  
}






