#' @export
#' @importFrom rlang .data
#' 
#' @title Apply hourly aggregation QC using "AB_OO" algorithm
#' 
#' @param pat A PurpleAir timeseries object.
#' @param min_count Aggregation bins with fewer than \code{min_count} measurements
#' will be marked as \code{NA}.
#' @param returnAllColumns Logical specifying whether to return all columns
#' of statistical data generated for QC algorithm or just the final \code{pm25}
#' result.
#'  
#' @description Creates a \code{pm25} timeseries by averaging aggregated data
#' from the A and B channels and applying the following QC logic:
#' 
#' \enumerate{
#' \item{Create pm25 by averaging the A and B channel aggregation means}
#' \item{Invalidate data where:  (min_count < 20)}
#' \item{No further QC}
#' }
#'
#' @note Purple Air II sensors reporting after the June, 2019 firmware
#' upgrade report data every 2 minutes or 30 measurements per hour. The default
#' setting of \code{min_count = 20} is equivalent to a required data recovery
#' rate of 67%.
#' 
#' @return Data frame with columns \code{datetime} and \code{pm25}.
#' 
#' @examples 
#' \dontrun{
#' library(AirSensor)
#' 
#' df_00 <- 
#'   example_pat %>%
#'   pat_qc() %>%
#'   PurpleAirQC_hourly_AB_00()
#'   
#' names(df)
#'   
#' plot(df_00, pch = 16, cex = 0.8, col = "red")
#' }

PurpleAirQC_hourly_AB_00 <- function(
  pat = NULL,
  min_count = 20,
  returnAllColumns = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  MazamaCoreUtils::stopIfNull(min_count)
  MazamaCoreUtils::stopIfNull(returnAllColumns)
  
  # ----- Prepare aggregated data ----------------------------------------------
  
  # Hourly counts
  countData <- 
    pat %>% 
    pat_aggregate(function(x) { base::length(na.omit(x)) }) %>% 
    pat_extractData()
  
  # Hourly means
  meanData <- 
    pat %>%
    pat_aggregate(function(x) { base::mean(x, na.rm = TRUE) }) %>% 
    pat_extractData()
  
  # ----- Create hourly dataframe ----------------------------------------------
  
  # NOTE:  Include variables used in QC so that they can be used to create a
  # NOTE:  plot visualizing the how the QC algorithm rejects values.
  
  hourlyData <- 
    dplyr::tibble(datetime = meanData$datetime) %>%
    
    # Create pm25 by averaging the A and B channel aggregation means
    dplyr::mutate(pm25 = (meanData$pm25_A + meanData$pm25_B) / 2) %>%
    
    # Calculate min_count and mean_diff for use in QC
    dplyr::mutate(min_count = pmin(countData$pm25_A, countData$pm25_B, na.rm = TRUE)) %>%
    dplyr::mutate(mean_diff = abs(meanData$pm25_A - meanData$pm25_B)) %>%
    
    # -----hourly_AB_00 QC algorithm -----
    
    # When only a fraction of the data are reporting, something is wrong.
    # Invalidate data where:  (min_count < SOME_THRESHOLD)
    dplyr::mutate(pm25 = replace(
      .data$pm25, 
      which(.data$min_count < min_count), 
      NA) 
    )
  
  # ----- Return ---------------------------------------------------------------
  
  if ( !returnAllColumns ) {
    hourlyData <- 
      hourlyData %>%
      dplyr::select(.data$datetime, .data$pm25)
  }
  
  return(hourlyData)
  
}

