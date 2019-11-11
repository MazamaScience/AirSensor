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
#' channels. One correlation value for each channel pair except \code{pm25_A}, 
#' \code{pm25_B}, and \code{humidity}, \code{temperature} will be returned for 
#' each day. All returned values are expected to hover near 0 for a properly
#' functioning sensor.
#' 
#' 
#' @examples  
#' tbl <- 
#'   example_pat_failure_A %>%
#'   PurpleAirSoH_dailyCorrelation_OLD(aggregationPeriod = "30 min") 
#'   
#' timeseriesTbl_multiplot(
#'   tbl, 
#'   parameters = c("pm25_B_temperature_cor", "pm25_A_temperature_cor"),
#'   ylim = c(-1,1), 
#'   style = "line"
#' )
#' 
#' timeseriesTbl_multiplot(
#'   tbl, 
#'   autoRange = TRUE, 
#'   style = "line"
#' )
#' 

PurpleAirSoH_dailyCorrelation_OLD <- function(
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
  
  # ----- Create daily tbl -----------------------------------------------------
  
  # Get full days in the local timezone
  timezone <- pat$meta$timezone
  localTime <- lubridate::with_tz(pat$dat$datetime, tzone = timezone)
  hour <- lubridate::hour(localTime)
  start <- lubridate::floor_date(localTime[ min(which(hour == 0)) ], unit = "hour")
  end <- lubridate::floor_date(localTime[ max(which(hour == 23)) ], unit = "hour")
  
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
  
  # Create daily tibble based on daterange to join with the valid_tbl and 
  # flag missing data
  days <- tibble(datetime = datetime) 
  
  # ----- Calculate dailyCorrelation -------------------------------------------
  
  # Begin percent DC calculations:
  pct_tbl <-
    pat %>%
    pat_aggregate(period = aggregationPeriod)
  
  # Put it on a local time axis and trim
  pct_tbl$datetime <- lubridate::with_tz(pct_tbl$datetime, tzone = timezone)
  pct_tbl <- dplyr::filter(pct_tbl, .data$datetime >= start & .data$datetime <= end)
  
  pct_tbl <-
    pct_tbl %>%
    # Group by day so that the correlations can be applied to a local 24 hour day
    dplyr::mutate(localDaystamp = strftime(.data$datetime, "%Y%m%d", 
                                           tz = timezone)) %>%
    dplyr::group_by(.data$localDaystamp)
  
  # Preallocate a list
  correlation_list <- list()
  
  # Loop through each unique day in the dataset
  for ( day in unique(pct_tbl$localDaystamp) ) {
    
    # pull out the data associated with one day at a time
    day_tbl <- 
      dplyr::filter(pct_tbl, .data$localDaystamp == day)
    
    
    correlation_list[[day]] <- day_tbl
    
    
    # calculate the correlation between several variables
    pm25_A_humidity_cor <- stats::cor(day_tbl$pm25_A_mean, day_tbl$humidity_mean, 
                                      use = "pairwise.complete.obs")
    pm25_A_temperature_cor <- stats::cor(day_tbl$pm25_A_mean, day_tbl$temperature_mean, 
                                         use = "pairwise.complete.obs")
    pm25_B_humidity_cor <- stats::cor(day_tbl$pm25_B_mean, day_tbl$humidity_mean, 
                                      use = "pairwise.complete.obs")
    pm25_B_temperature_cor <- stats::cor(day_tbl$pm25_B_mean, day_tbl$temperature_mean, 
                                         use = "pairwise.complete.obs")

    
    # add the correlation per day, per variable comparison to a list
    correlation_list[[day]] <- list(
      pm25_A_humidity_cor = pm25_A_humidity_cor,
      pm25_A_temperature_cor = pm25_A_temperature_cor,
      pm25_B_humidity_cor = pm25_B_humidity_cor,
      pm25_B_temperature_cor = pm25_B_temperature_cor
    )
    
  }
  
  # TODO:  The following is messy and not the most efficient way to convert 
  # TODO:  from a list to a dataframe and certainly could be improved.
  
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
  colnames <- c( "datetime","pm25_A_humidity_cor", "pm25_A_temperature_cor",
                 "pm25_B_humidity_cor", "pm25_B_temperature_cor")
  colnames(correlation_df) <-colnames
  
  # re-define each of the columns as numeric rather than lists for easier plotting in ggplot
  correlation_df$pm25_A_temperature_cor <- as.numeric(correlation_df$pm25_A_temperature_cor)
  correlation_df$pm25_A_humidity_cor <- as.numeric(correlation_df$pm25_A_humidity_cor)
  correlation_df$pm25_B_temperature_cor <- as.numeric(correlation_df$pm25_B_temperature_cor)
  correlation_df$pm25_B_humidity_cor <- as.numeric(correlation_df$pm25_B_humidity_cor)

  
  # join with empty daily column to flag missing days
  correlation_df <- dplyr::left_join(days, correlation_df, by = "datetime")
  
  # ----- Return ---------------------------------------------------------------
  
  return(correlation_df)
  
}






