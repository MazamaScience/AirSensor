#' @export
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_point ylim ggtitle 
#' @importFrom ggplot2 scale_y_continuous scale_y_log10 scale_color_manual theme
#' 
#' @title Validation plot for QC algorithms
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param period Time period to average to. Can be "sec", "min", "hour", 
#' "day", "DSTday", "week", "month", "quarter" or "year". A number can also
#'  precede these options followed by a space (i.e. "2 day" or "37 min").
#' @param qc_algorithm Named QC algorithm to apply to hourly aggregation stats.
#' @param min_count Aggregation bins with fewer than `min_count` measurements
#' will be marked as `NA`.
#'  
#' @description A diagnostic plot is generated showing time series for the
#' aggregation statistics used in the QC algorithm. Reviewing these time series
#' for different "pat" objects can provide insight into what kinds of instrument
#' problems a QC algorithm will catch or miss.
#'
#' Current QC algorithms include:
#' \itemize{
#' \item{\code{hourly_AB_00}}
#' \item{\code{hourly_AB_01}}
#' }
#'
#' @return A dataframe of aggregation statistics.
#' 
#' @seealso PurpleAirQC_hourly_AB_00
#' @seealso PurpleAirQC_hourly_AB_01
#' 
#' @examples 
#' \dontrun{
#' scsg_15 <- pat_load("SCSG_15", "2019-06-13", "2019-06-20")
#' PurpleAirQC_validationPlot(scsg_15)
#' 
#' scap_14 <- pat_load("SCAP_14", "2019-06-13", "2019-06-20")
#' PurpleAirQC_validationPlot(scap_14)
#' 
#' scem_05 <- pat_load("SCEM_05", "2019-06-13", "2019-06-20")
#' PurpleAirQC_validationPlot(scem_05)
#' }

PurpleAirQC_validationPlot <- function(
  pat = NULL,
  period = "1 hour",
  qc_algorithm = "hourly_AB_01",
  min_count = 20
) {
  
  # ===== DEBUG ================================================================
  
  if ( FALSE ) {
    
    pat <- example_pat
    period <- "1 hour"
    qc_algorithm <- "hourly_AB_01"
    min_count <- 20
    
  }  
  
  # ----- Validate Parameters --------------------------------------------------
  
  period <- tolower(period)

  if ( !pat_isPat(pat) )
    stop("Required parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Required parameter 'pat' has no data.") 
  
  if ( !qc_algorithm %in% c("hourly_AB_00", "hourly_AB_01") ) {
    stop("Required parameter 'qc_algorithm' must be one of 'hourly_AB_00' or 'hourly_AB_01'")
  }
  
  # ----- Prepare data ---------------------------------------------------------
  
  hourlyData <-
    pat %>%
    pat_qc() %>%
    pat_aggregate(period = "1 hour")
  
  FUN <- get(paste0("PurpleAirQC_", qc_algorithm))
  
  hourlyData <-
    hourlyData %>%
    FUN(returnAllColumns = TRUE)

  # ----- Create plots ---------------------------------------------------------
  
  # Set up default colors
  colors <- c(rgb(0.9, 0.25, 0.2), rgb(0.2, 0.25, 0.9))
  
  # min count plot
  gg_min_count <- 
    ggplot(hourlyData, aes(.data$datetime, .data$min_count)) +
    geom_point(shape = 15) +
    ylim(0, max(hourlyData$min_count)) +
    ggtitle("A/B minimum count")
  
  # mean difference plot
  gg_mean_diff <- 
    ggplot(hourlyData, aes(.data$datetime, .data$mean_diff)) +
    geom_point(shape = 15) +
    scale_y_continuous(limits=c(1, max(20, hourlyData$mean_diff, na.rm = TRUE))) + 
    ggtitle("A/B difference")
  
  # p-value plot
  gg_pm25_p <- 
    ggplot(hourlyData, aes(.data$datetime, .data$pm25_p)) +
    geom_point(shape = 15) +
    scale_y_log10() + 
    ggtitle("t-test p-value")
  
  # Jons_qc_1 plot
  gg_pm25_qc <- 
    ggplot(hourlyData, aes(.data$datetime, .data$pm25)) +
    geom_point(shape = 15) +
    scale_y_continuous() + 
    ggtitle(qc_algorithm)
  
  # A B means
  gg_AB_means <- 
    hourlyData %>%
    dplyr::select(.data$datetime, .data$pm25_A_mean, .data$pm25_B_mean) %>%
    tidyr::gather("channel", "value", -.data$datetime) %>%
    
    ggplot(aes(.data$datetime, .data$value, color = .data$channel)) +
    geom_point(shape = 15) +
    scale_color_manual(values=colors) +
    scale_y_continuous() + 
    theme(legend.position = "none") +
    ggtitle("A/B separate")
  
  # ----- Assemmble full plot --------------------------------------------------
  
  multi_ggplot(
    gg_AB_means,
    gg_mean_diff,
    gg_pm25_p,
    gg_min_count,
    gg_pm25_qc
  )
  
}
