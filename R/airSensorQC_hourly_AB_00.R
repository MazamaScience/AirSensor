#' @export
#' @importFrom rlang .data
#' 
#' @title Apply QC to Aggregation Statistics
#' 
#' @param aggregationStats Dataframe of statistics as returned by
#' \code{pat_aggregate()}.
#' @param min_count Aggregation bins with fewer than `min_count` measurements
#' will be marked as `NA`.
#'  
#' @description Creates a \code{pm25} timeseries by averaging aggregated data
#' from the A and B channels and applying the following QC logic:
#' 
#' \enumerate{
#' \item{Create pm25 by averaging the A and B channel aggregation means}
#' \item{Invalidate data where:  (min_count < 10)}
#' \item{No further QC}
#' }
#'
#' @return Data frame with columns \code{datetime} and \code{pm25}.
#' 
#' @examples 
#' \dontrun{
#' df <- 
#'   example_pat %>%
#'   pat_qc() %>%
#'   pat_aggregate() %>%
#'   airSensorQC_hourly_AB_00()
#'   
#' plot(df)
#' }

airSensorQC_hourly_AB_00 <- function(
  aggregationStats,
  min_count = 10
) {
  
  # ----- hourly_AB_01 ---------------------------------------------------------
  
  hourlyData <-
    aggregationStats %>%
    # Create pm25 by averaging the A and B channel aggregation means
    dplyr::mutate(pm25 = (.data$pm25_A_mean + .data$pm25_B_mean) / 2) %>%
    # Calculate min_count and mean_diff for use in QC
    dplyr::mutate(min_count = pmin(.data$pm25_A_count, .data$pm25_B_count, na.rm = TRUE)) %>%
    dplyr::mutate(mean_diff = abs(.data$pm25_A_mean - .data$pm25_B_mean)) %>%
    # hourly_AB_00 follows
    # When only a fraction of the data are reporting, something is wrong.
    # Invalidate data where:  (min_count < SOME_THRESHOLD)
    dplyr::mutate(pm25 = replace(
      .data$pm25, 
      which(.data$min_count < min_count), 
      NA) 
    ) %>%
    dplyr::select(.data$datetime, .data$pm25)
  
  return(hourlyData)
  
}

