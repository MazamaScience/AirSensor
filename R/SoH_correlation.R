#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains
#' 
#' @title Daily correlation values
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param aggregationPeriod The period of time over which to aggregate the data. 
#' Recommended that this value is at least greater than 2 min in order to have 
#' complete pairs between the A and B channels. If there is not a complete pair, 
#' the function will return an for that day. The smaller the \emph{aggregation
#' period}, the longer this function will take to run. The default is set to "10
#' min" for this reason.
#' 
#' @description This function calculates the daily correlation values between
#'  the \code{pm25_A}, \code{pm25_B}, \code{humidity}, and 
#' \code{temperature} channels. One correlation value for each channel pair will 
#' be returned for each day.
#' 
#' 
#' @examples  
#' tbl <- 
#'   example_pat %>%
#'   SoH_correlation(testPeriod = "30 min") 
#'   
#' timeseriesTbl_multiplot(tbl, ylim = c(-1,1), style = "line)

SoH_correlation <- function(
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
  
  # ----- Calculate pct_DC -----------------------------------------------------
  
  timezone <- pat$meta$timezone
  
  pat_tbl <-
    pat %>%
    # Aggregate to at least 2 min to avoid NA offset between channels A and B. 
    # Recommend aggregating to at least 10 min for speed.
    pat_aggregate(period = aggregationPeriod) %>%
    # Establish a local time channel
    mutate(localTime = lubridate::with_tz(datetime, tzone=timezone)) %>%
    # Group by day so that the correlations can be applied to a local 24 hour day
    dplyr::mutate(localDaystamp = strftime(.data$localTime, "%Y%m%d", tz = timezone)) %>%
    dplyr::group_by(.data$localDaystamp)
  
  # Preallocate a list
  correlation_list <- list()
  
  # Loop through each unique day in the dataset
  for ( day in unique(pat_tbl$localDaystamp) ) {
    
    # pull out the data associated with one day at a time
    day_tbl <- 
      dplyr::filter(pat_tbl, localDaystamp == day)
    
    
    correlation_list[[day]] <- day_tbl
    
    
    # calculate the correlation between several variables
    pm25_A_pm25_B_cor <- cor(day_tbl$pm25_A_mean, day_tbl$pm25_B_mean, use = "pairwise.complete.obs")
    pm25_A_humidity_cor <- cor(day_tbl$pm25_A_mean, day_tbl$humidity_mean, use = "pairwise.complete.obs")
    pm25_A_temperature_cor <- cor(day_tbl$pm25_A_mean, day_tbl$temperature_mean, use = "pairwise.complete.obs")
    pm25_B_humidity_cor <- cor(day_tbl$pm25_B_mean, day_tbl$humidity_mean, use = "pairwise.complete.obs")
    pm25_B_temperature_cor <- cor(day_tbl$pm25_B_mean, day_tbl$temperature_mean, use = "pairwise.complete.obs")
    temperature_humidity_cor <- cor(day_tbl$temperature_mean, day_tbl$humidity_mean, use = "pairwise.complete.obs")
    
    
    # add the correlation per day, per variable comparison to a list
    correlation_list[[day]] <- list(
      pm25_A_pm25_B_cor = pm25_A_pm25_B_cor,
      pm25_A_humidity_cor = pm25_A_humidity_cor,
      pm25_A_temperature_cor = pm25_A_temperature_cor,
      pm25_B_humidity_cor = pm25_B_humidity_cor,
      pm25_B_temperature_cor = pm25_B_temperature_cor,
      temperature_humidity_cor = temperature_humidity_cor
    )
    
  }
  # reformat the list as a tibble
  int_correlation_tbl <- dplyr::as_tibble(correlation_list)
  
  # reformat as a matrix in order to have the desired outcome after transpose
  correlation_matrix<- t(as.matrix(int_correlation_tbl))
  
  # reformat as a data.frame in order to change datetime to POSIXCT and add the colnames
  correlation_df <- data.frame(correlation_matrix)
  
  # reformat to tibble to change the datetime from rownames to an actual column of data
  correlation_df <- tibble::rownames_to_column(correlation_df, var="datetime") 
  
  # change datetime into a POSIXCT 
  correlation_df$datetime <- MazamaCoreUtils::parseDatetime(correlation_df$datetime, timezone = timezone)
  
  # add column names
  colnames <- c( "datetime","pm25_A_pm25_B_cor", "pm25_A_humidity_cor", "pm25_A_temperature_cor",
                 "pm25_B_humidity_cor", "pm25_B_temperature_cor",
                 "temperature_humidity_cor")
  colnames(correlation_df) <-colnames
  
  # re-define each of the columns as numeric rather than lists for easier plotting in ggplot
  correlation_df$pm25_A_pm25_B_cor <- as.numeric(correlation_df$pm25_A_pm25_B_cor)
  correlation_df$pm25_A_temperature_cor <- as.numeric(correlation_df$pm25_A_temperature_cor)
  correlation_df$pm25_A_humidity_cor <- as.numeric(correlation_df$pm25_A_humidity_cor)
  correlation_df$pm25_B_temperature_cor <- as.numeric(correlation_df$pm25_B_temperature_cor)
  correlation_df$pm25_B_humidity_cor <- as.numeric(correlation_df$pm25_B_humidity_cor)
  correlation_df$temperature_humidity_cor <- as.numeric(correlation_df$temperature_humidity_cor)
  
  

  
  # ----- Return ---------------------------------------------------------------
  
  return(correlation_df)
  
}






