#' @export
#' @importFrom rlang .data
#' 
#' @title Apply QC to Aggregation Statistics
#' 
#' @param aggregationStats Dataframe of statistics as returned by
#' \code{pat_aggregate()}.
#' @param min_count Aggregation bins with fewer than `min_count` measurements
#' will be marked as `NA`.
#' @param returnAllColumns Logical specifying whether to return all columns
#' of statistical data generated for QC algorithm or just the final `pm25` 
#' result.
#'  
#' @description Creates a \code{pm25} timeseries by averaging aggregated data
#' from the A and B channels and applying the following QC logic:
#' 
#' \enumerate{
#' \item{Create pm25 by averaging the A and B channel aggregation means}
#' \item{Invalidate data where:  (min_count < 20)}
#' \item{Invalidate data where:  (p-value < 1e-4) & (mean_diff > 10)}
#' \item{Invalidate data where:  (pm25 < 100) & (mean_diff > 20)}
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
#' df <- 
#'   example_pat %>%
#'   pat_qc() %>%
#'   pat_aggregate() %>%
#'   PurpleAirQC_hourly_AB_01()
#'   
#' plot(df)
#' }

PurpleAirQC_hourly_AB_01 <- function(
  aggregationStats = NULL,
  min_count = 20,
  returnAllColumns = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(aggregationStats)
  
  # ----- hourly_AB_01 ---------------------------------------------------------
  
  hourlyData <-
    aggregationStats %>%
    # Create pm25 by averaging the A and B channel aggregation means
    dplyr::mutate(pm25 = (.data$pm25_A_mean + .data$pm25_B_mean) / 2) %>%
    # Calculate min_count and mean_diff for use in QC
    dplyr::mutate(min_count = pmin(.data$pm25_A_count, .data$pm25_B_count, na.rm = TRUE)) %>%
    dplyr::mutate(mean_diff = abs(.data$pm25_A_mean - .data$pm25_B_mean)) %>%
    # hourly_AB_01 follows
    # When only a fraction of the data are reporting, something is wrong.
    # Invalidate data where:  (min_count < SOME_THRESHOLD)
    dplyr::mutate(pm25 = replace(
      .data$pm25, 
      which(.data$min_count < min_count), 
      NA) 
    ) %>%
    # When the means are significantly different AND 'large', something is wrong.
    # Invalidate data where:  (p-value < 1e-4) & (mean_diff > 10)
    dplyr::mutate(pm25 = replace(
      .data$pm25,
      which( (.data$pm25_p < 1e-4) & (.data$mean_diff > 10) ),
      NA)
    ) %>% 
    # A difference of 20 ug/m3 should only be seen at very high levels.
    # Invalidate data where:  (mean < 100) & (mean_diff > 20)
    dplyr::mutate(pm25 = replace(
      .data$pm25,
      which( (.data$pm25 < 100) & (.data$mean_diff > 20) ),
      NA)
    )
  
  if ( !returnAllColumns ) {
    hourlyData <- 
      hourlyData %>%
      dplyr::select(.data$datetime, .data$pm25)
  }
  
  return(hourlyData)
  
}

