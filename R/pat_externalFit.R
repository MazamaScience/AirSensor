#' @export
#' @importFrom rlang .data
#' @importFrom stats lm
#' @import dplyr
#' @import graphics
#' 
#' @title Linear model fitting of PurpleAir and federal time series data
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param replaceOutliers Logical specifying whether or not to replace outliers.
#' @param showPlot Logical specifying whether to generate a model fit plot.
#' @param size Size of points.
#' @param shape Symbol to use for points.
#' @param color Color of points.
#' @param alpha Opacity of points.
#' @param xylim Vector of (lo,hi) limits used as limits on the correlation plot 
#' axes -- useful for zooming in.
#' @param period Time period to average to. Can be "sec", "min", "hour", 
#' "day", "DSTday", "week", "month", "quarter" or "year". A number can also
#' precede these options followed by a space (i.e. "2 day" or "37 min").
#' @param channel Data channel to use for PM2.5 -- one of "a", "b or "ab".
#' @param qc_algorithm Named QC algorithm to apply to hourly aggregation stats.
#' @param min_count Aggregation bins with fewer than `min_count` measurements
#' will be marked as `NA`.
#' 
#' @description Uses a linear model to fit data from PurpleAir to data from 
#' a federal monitor.
#' 
#' A diagnostic plot is produced if `showPlot = TRUE`.
#' 
#' @return A linear model, fitting the `pat` pm25 readings to federal monitor 
#' readings.
#' 
#' @examples
#' \dontrun{
#' pat <- pat_load("SCAH_07")
#' pat_externalFit(pat)
#' }

pat_externalFit <- function(
  pat = NULL,
  replaceOutliers = TRUE,
  showPlot = TRUE,
  size = 1,
  shape = 15,
  color = "purple",
  alpha = 0.6,
  xylim = NULL,
  period = "1 hour",
  channel = "ab",
  qc_algorithm = "hourly_AB_01",
  min_count = 10
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  # For easier access
  meta <- pat$meta
  data <- pat$data
  
  if ( is.null(xylim) ) {
    dataMin <- min(c(0, data$pm25_A, data$pm25_B), na.rm = TRUE)
    dataMax <- max(c(data$pm25_A, data$pm25_B), na.rm = TRUE)
    xylim <- c(dataMin, dataMax)
  }
  
  # ----- Assemble data ---------------------------------------------
  
  if ( replaceOutliers )
    pat <- pat_outliers(pat, showPlot = FALSE, replace = TRUE)
  
  # Get the hourly aggregated data
  paHourly_data <-
    pat %>% 
    pat_createAirSensor(period = "1 hour") %>%
    PWFSLSmoke::monitor_extractData()
  names(paHourly_data) <- c("datetime", "pa_pm25")
  
  tlim <- range(paHourly_data$datetime)
  
  # Get the PWFSL monitor data
  monitorID = pat$meta$pwfsl_closestMonitorID
  pwfsl_data <-
    PWFSLSmoke::monitor_load(tlim[1], tlim[2], monitorIDs = monitorID) %>%
    PWFSLSmoke::monitor_subset(tlim = tlim) %>%
    PWFSLSmoke::monitor_extractData()
  names(pwfsl_data) <- c("datetime", "pwfsl_pm25")
  
  # Create a tidy dataframe appropriate for ggplot
  tidy_data <-
    dplyr::full_join(paHourly_data, pwfsl_data, by = "datetime")
  
  # ----- Linear model ---------------------------------------------------------
  
  # Model A as a function of B (data should lie on a line)
  model <- lm(tidy_data$pwfsl_pm25 ~ tidy_data$pa_pm25, subset = NULL, 
              weights = NULL)
  
  slope <- as.numeric(model$coefficients[2])      # as.numeric() to remove name
  intercept <- as.numeric(model$coefficients[1])
  r_squared <- summary(model)$r.squared
  
  # Label for linear fit
  equationLabel <- 
    ggplot2::annotate(
      geom = "text", 
      x = 0.75 * xylim[2],
      y = c(0.4, 0.3, 0.2) * xylim[2], 
      label = c(paste0("Slope = ", round(slope, digits = 2)),
                paste0("Intercept = ", round(intercept, digits = 1)),
                paste0("R\U00B2 = ", round(r_squared, digits = 3))) )
  
  # ----- Plot -----------------------------------------------------------------
  
  if ( showPlot ) { 
    
    pm25_plot <- 
      tidy_data %>% 
      ggplot2::ggplot() +
      ggplot2::geom_point(ggplot2::aes(x = .data$datetime, y = .data$pwfsl_pm25),
                          size = size,
                          shape = shape,
                          color = rgb(0.9, 0.25, 0.2), # pat_multiplot default
                          alpha = alpha) +
      ggplot2::geom_point(ggplot2::aes(x = .data$datetime, y = .data$pa_pm25),
                          size = size,
                          shape = shape,
                          color = rgb(0.2, 0.25, 0.9), # pat_multiplot default
                          alpha = alpha) +
      ggplot2::ylim(xylim) +
      ggplot2::ggtitle(expression("Channel A/B PM"[2.5])) #+ 
      #ggplot2::xlab(year) + ggplot2::ylab("\u03bcg / m\u00b3") 

    print(pm25_plot)
    
  }
  
  return(model)
  
}










