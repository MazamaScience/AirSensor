#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains 
#' 
#' @title Daily correlation values
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param aggregationPeriod The period of time over which to aggregate the data. 
#' in order to create complete pairs between the A and B channels. If there is 
#' not a complete pair, the function will return an "NA" for that day. It is
#' recommended that this value is between "2 min" and "1 hour" The smaller the 
#' \emph{aggregation period}, the longer this function will take to run. The 
#' default is set to "10 min" for this reason.
#' 
#' @description This function calculates the daily correlation values between
#' the \code{pm25_A}, \code{pm25_B}, \code{humidity}, and \code{temperature} 
#' channels. One correlation value for each channel pair will 
#' be returned for each day.
#' 
#' 
#' @examples  
#' tbl <- 
#'   example_pat %>%
#'   SoH_dailyCorrelation(aggregationPeriod = "30 min") 
#'   
#' timeseriesTbl_multiplot(tbl, ylim = c(-1,1), style = "line")
#' 

SoH_dailyCorrelation <- function(
  pat = NULL,
  aggregationPeriod = "10 min"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  
  # ----- Convert period to seconds --------------------------------------------
  
  periodParts <- strsplit(aggregationPeriod, " ", fixed = TRUE)[[1]]
  
  if ( length(periodParts) == 1 ) {
    periodCount <- 1
    units <- periodParts[1]
  } else {
    periodCount <- as.numeric(periodParts[1])
    units <- periodParts[2]
  }
  
  if ( units == "sec"     ) unitSecs <- 1
  if ( units == "min"     ) unitSecs <- 60
  if ( units == "hour"    ) unitSecs <- 3600
  if ( units == "day"     ) unitSecs <- 3600 * 24
  if ( units == "week"    ) unitSecs <- 3600 * 24 * 7
  if ( units == "month"   ) unitSecs <- 3600 * 24 * 31
  if ( units == "quarter" ) unitSecs <- 3600 * 24 * 31 * 3
  if ( units == "year"    ) unitSecs <- 3600 * 8784 
  
  periodSeconds <- periodCount * unitSecs 
  
  hourFactor <- 3600 / periodSeconds
  
  # ----- Calculate daily correlation values -----------------------------------
  
  timezone <- pat$meta$timezone
  
  # Note: after initial completion of this function, decided to chop the passed 
  # in pat objects by full days. First convert the datetime column in the pat to
  # local time, then filter based on the first and last full day in the local
  # timezone
  
  pat$data$datetime <- lubridate::with_tz(pat$data$datetime, 
                                          tzone = timezone)
  # Parse the hours in datetime to find the first and last full days
  hour <- lubridate::hour(pat$data$datetime)
  start <- pat$data$datetime[ min(which(hour == 0)) ]
  end <- pat$data$datetime[ max(which(hour == 23)) ]
  
  # Add create hourly tibble based on daterange to join later and flag missing data
  days <- tibble(datetime = seq(start, end, by = "day")) 
  days$datetime <- lubridate::as_date(days$datetime)
  days$datetime <- MazamaCoreUtils::parseDatetime(days$datetime, timezone = timezone)
  
  # Filter the pat based on the times established above.
  pat <- pat_filterDate(pat, start, end, timezone = timezone) 
  
  # Begin aggregation and correlation calculations
  pat_tbl <-
    
    pat %>%
    # Aggregate to at least 2 min to avoid NA offset between channels A and B. 
    # Recommend aggregating to at least 10 min for speed.
    pat_aggregate(period = aggregationPeriod) %>%
    
    # Establish a local time channel
    mutate(localTime = lubridate::with_tz(.data$datetime, 
                                          tzone = timezone)) %>%
    
    # Group by day so that the correlations can be applied to a local 24 hour day
    dplyr::mutate(localDaystamp = strftime(.data$localTime, "%Y%m%d", 
                                           tz = timezone)) %>%
    dplyr::group_by(.data$localDaystamp)
  
  # Preallocate a list
  correlation_list <- list()
  
  # Loop through each unique day in the dataset
  for ( day in unique(pat_tbl$localDaystamp) ) {
    
    # pull out the data associated with one day at a time
    day_tbl <- 
      dplyr::filter(pat_tbl, .data$localDaystamp == day)
    
    
    correlation_list[[day]] <- day_tbl
    
    
    # calculate the correlation between several variables
    pm25_A_pm25_B_cor <- stats::cor(day_tbl$pm25_A_mean, day_tbl$pm25_B_mean, 
                                    use = "pairwise.complete.obs")
    pm25_A_humidity_cor <- stats::cor(day_tbl$pm25_A_mean, day_tbl$humidity_mean, 
                                      use = "pairwise.complete.obs")
    pm25_A_temperature_cor <- stats::cor(day_tbl$pm25_A_mean, day_tbl$temperature_mean, 
                                         use = "pairwise.complete.obs")
    pm25_B_humidity_cor <- stats::cor(day_tbl$pm25_B_mean, day_tbl$humidity_mean, 
                                      use = "pairwise.complete.obs")
    pm25_B_temperature_cor <- stats::cor(day_tbl$pm25_B_mean, day_tbl$temperature_mean, 
                                         use = "pairwise.complete.obs")
    temperature_humidity_cor <- stats::cor(day_tbl$temperature_mean, day_tbl$humidity_mean, 
                                           use = "pairwise.complete.obs")
    
    # calculate the ab slope
    model <- lm(day_tbl$pm25_A_mean ~ day_tbl$pm25_B_mean, subset = NULL, weights = NULL)
    pm25_A_pm25_B_slope <- as.numeric(model$coefficients[2])      # as.numeric() to remove name
    pm25_A_pm25_B_intercept <- as.numeric(model$coefficients[1])
    pm25_A_pm25_B_r_squared <- summary(model)$r.squared
    
    
    # add the correlation per day, per variable comparison to a list
    correlation_list[[day]] <- list(
      pm25_A_pm25_B_cor = pm25_A_pm25_B_cor,
      pm25_A_humidity_cor = pm25_A_humidity_cor,
      pm25_A_temperature_cor = pm25_A_temperature_cor,
      pm25_B_humidity_cor = pm25_B_humidity_cor,
      pm25_B_temperature_cor = pm25_B_temperature_cor,
      temperature_humidity_cor = temperature_humidity_cor,
      pm25_A_pm25_B_slope = pm25_A_pm25_B_slope,
      pm25_A_pm25_B_intercept = pm25_A_pm25_B_intercept
    )
    
  }
  # TODO: the following is messy and not the most efficient way to convert 
  # from a list to a dataframe and certainly could be improved.
  
  # reformat the list as a tibble
  int_correlation_tbl <- dplyr::as_tibble(correlation_list)
  
  #  transpose and reformat as a matrix in order to have the desired outcome after transpose
  correlation_matrix <- t(as.matrix(int_correlation_tbl))
  
  # reformat as a data.frame in order to change datetime to POSIXCT and add the colnames
  correlation_df <- data.frame(correlation_matrix)
  
  # reformat to tibble to change the datetime from rownames to an actual column of data
  correlation_df <- tibble::rownames_to_column(correlation_df, var="datetime") 
  
  # change datetime into a POSIXCT 
  correlation_df$datetime <- MazamaCoreUtils::parseDatetime(correlation_df$datetime, 
                                                            timezone = timezone)
  
  # add column names
  colnames <- c( "datetime","pm25_A_pm25_B_cor", "pm25_A_humidity_cor", "pm25_A_temperature_cor",
                 "pm25_B_humidity_cor", "pm25_B_temperature_cor",
                 "temperature_humidity_cor", "pm25_A_pm25_B_slope", "pm25_A_pm25_B_intercept")
  colnames(correlation_df) <-colnames
  
  # re-define each of the columns as numeric rather than lists for easier plotting in ggplot
  correlation_df$pm25_A_pm25_B_cor <- as.numeric(correlation_df$pm25_A_pm25_B_cor)
  correlation_df$pm25_A_temperature_cor <- as.numeric(correlation_df$pm25_A_temperature_cor)
  correlation_df$pm25_A_humidity_cor <- as.numeric(correlation_df$pm25_A_humidity_cor)
  correlation_df$pm25_B_temperature_cor <- as.numeric(correlation_df$pm25_B_temperature_cor)
  correlation_df$pm25_B_humidity_cor <- as.numeric(correlation_df$pm25_B_humidity_cor)
  correlation_df$temperature_humidity_cor <- as.numeric(correlation_df$temperature_humidity_cor)
  correlation_df$pm25_A_pm25_B_slope <- as.numeric(correlation_df$pm25_A_pm25_B_slope)
  correlation_df$pm25_A_pm25_B_intercept <- as.numeric(correlation_df$pm25_A_pm25_B_intercept)

  
  # join with empty daily column to flag missing days
  correlation_df <- dplyr::left_join(days, correlation_df, by = "datetime")
  

  
  # ----- Return ---------------------------------------------------------------
  
  return(correlation_df)
  
}






