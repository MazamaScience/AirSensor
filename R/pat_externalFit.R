#' @export
#' @importFrom rlang .data
#' @importFrom stats lm
#' @import dplyr
#' @import graphics
#' 
#' @title Linear model fitting of PurpleAir and federal PWFSL time series data
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param replaceOutliers Logical specifying whether or not to replace outliers.
#' @param showPlot Logical specifying whether to generate a model fit plot.
#' @param size Size of points.
#' @param lm_shape Symbol to use for linear model points.
#' @param ts_shape Symbol to use for time series plot points.
#' @param color Color of points.
#' @param alpha Opacity of points.
#' @param xylim Vector of (lo,hi) limits used as limits on the correlation plot 
#' axes -- useful for zooming in.
#' @param channel Data channel to use for PM2.5 -- one of "a", "b or "ab".
#' @param qc_algorithm Named QC algorithm to apply to hourly aggregation stats.
#' @param min_count Aggregation bins with fewer than `min_count` measurements
#' will be marked as `NA`.
#' @param pa_color Color of hourly points
#' @param pwfsl_color Color of hourly points
#' 
#' @description Produces a linear model between data from PurpleAir and data 
#' from the closest PWFSL monitor.
#' 
#' A diagnostic plot is produced if `showPlot = TRUE`.
#' 
#' @return A linear model, fitting the `pat` PurpleAir readings to the closest
#' PWFSL monitor readings.
#' 
#' @examples
#' \dontrun{
#' pat <- pat_load("SCAH_22")
#' pat_externalFit(pat)
#' }

pat_externalFit <- function(
  pat = NULL,
  replaceOutliers = TRUE,
  showPlot = TRUE,
  size = 1,
  lm_shape = 15,
  ts_shape = 1,
  color = "purple",
  alpha = 0.5,
  xylim = NULL,
  channel = "ab",
  qc_algorithm = "hourly_AB_01",
  min_count = 10,
  pa_color = "purple",
  pwfsl_color = "black"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  # For easier access
  meta <- pat$meta
  data <- pat$data
  
  # ----- Assemble data ---------------------------------------------
  
  if ( replaceOutliers )
    pat <- pat_outliers(pat, showPlot = FALSE, replace = TRUE)
  
  # Get the hourly aggregated PurpleAir data
  paHourly_data <-
    pat %>% 
    pat_createAirSensor(period = "1 hour", 
                        channel = channel,
                        qc_algorithm = qc_algorithm,
                        min_count = min_count) %>%
    PWFSLSmoke::monitor_extractData()
  
  names(paHourly_data) <- c("datetime", "PA")
  
  tlim <- range(paHourly_data$datetime)
  
  # Get the PWFSL monitor data
  monitorID = pat$meta$pwfsl_closestMonitorID
  pwfsl_data <-
    PWFSLSmoke::monitor_load(tlim[1], tlim[2], monitorIDs = monitorID) %>%
    PWFSLSmoke::monitor_subset(tlim = tlim) %>%
    PWFSLSmoke::monitor_extractData()
  
  names(pwfsl_data) <- c("datetime", "PWFSL")
  
  # Combine data from both monitors into one dataframe
  both_data <- dplyr::full_join(paHourly_data, pwfsl_data, by = "datetime")
  
  # Create a tidy dataframe appropriate for ggplot
  tidy_data <-
    both_data %>%
    tidyr::gather("source", "pm25", -.data$datetime)
  
  # Define square xy limit now that we have the data for both monitors
  if ( is.null(xylim) ) {
    dataMin <- min(c(0, both_data$PA, both_data$PWFSL), na.rm = TRUE)
    dataMax <- max(c(both_data$PA, both_data$PWFSL), na.rm = TRUE)
    xylim <- c(dataMin, dataMax)
  }
  
  # ----- Linear model ---------------------------------------------------------
  
  # Model PWSFL as a function of PurpleAir (data should lie on a line)
  model <- lm(both_data$PWFSL ~ both_data$PA, subset = NULL, 
              weights = NULL)
  
  slope <- as.numeric(model$coefficients[2])      # as.numeric() to remove name
  intercept <- as.numeric(model$coefficients[1])
  r_squared <- summary(model)$r.squared
  
  # Label for linear fit
  equationLabel <- 
    ggplot2::annotate(
      geom = "text", 
      x = 0.75 * xylim[2],
      y = c(0.25, 0.15, 0.05) * xylim[2], 
      label = c(paste0("Slope = ", round(slope, digits = 2)),
                paste0("Intercept = ", round(intercept, digits = 1)),
                paste0("R\U00B2 = ", round(r_squared, digits = 3))) )
  
  # ----- Construct Plot -------------------------------------------------------
  
  if ( showPlot ) { 
    
    # LH Linear regression plot
    lm_plot <- 
      both_data %>% 
      ggplot2::ggplot(ggplot2::aes(x = .data$PA, y = .data$PWFSL)) + 
      ggplot2::geom_point(size = size, 
                          shape = lm_shape,
                          color = color,
                          alpha = alpha) + 
      ggplot2::stat_smooth(geom = "line", color = "gray70", alpha = 0.9, 
                           method = "lm") + 
      ggplot2::labs(title = "Correlation", 
                    x = paste0("PurpleAir: \"", pat$meta$label, "\""),
                    y = paste0("PWFSL: ", monitorID)) + 
      ggplot2::theme_bw() + 
      ggplot2::xlim(xylim) +
      ggplot2::ylim(xylim) +
      ggplot2::coord_fixed() +    # square aspect ratio
      equationLabel
    
    # Labels
    timezone <- pat$meta$timezone[1]
    year <- strftime(pat$data$datetime[1], "%Y", tz=timezone)
    
    ts_plot <-
      tidy_data %>%
      ggplot2::ggplot() +
      ggplot2::geom_point(ggplot2::aes(x = .data$datetime,
                                       y = .data$pm25,
                                       color = source),
                          size = size,
                          shape = ts_shape,
                          alpha = alpha) +
      ggplot2::scale_color_manual(values = c(pa_color, pwfsl_color)) +
      ggplot2::ylim(xylim) +
      ggplot2::ggtitle(expression("PM"[2.5])) + 
      ggplot2::xlab(year) + ggplot2::ylab("\u03bcg / m\u00b3") 
              
    bannerText <- paste0("Sensor / Monitor Comparision -- Distance: ",
                         round((pat$meta$pwfsl_closestDistance/1000), 1),
                         "km")
    bannerGrob <- grid::textGrob(bannerText,
                             just = "left",
                             x = 0.025,
                             gp = grid::gpar(fontsize = 20, col="grey60"))
    
    plot <- gridExtra::grid.arrange(bannerGrob, lm_plot, ts_plot, 
                                    ncol = 1, heights = c(1, 6, 3))
    print(plot)
  }
  
  return(invisible(model))
}










