#' @export
#' @importFrom rlang .data
#' @importFrom stats lm
#' @import dplyr
#' @import graphics
#' 
#' @title Linear model fitting of PurpleAir and federal PWFSL time series data
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param showPlot Logical specifying whether to generate a model fit plot.
#' @param size Size of points.
#' @param pa_color Color of hourly points.
#' @param pwfsl_color Color of hourly points.
#' @param alpha Opacity of points.
#' @param lr_shape Symbol to use for linear model points.
#' @param lr_color Color of linear model plot points.
#' @param lr_lwd Width of linear regression line.
#' @param lr_lcolor Color of linear regression line.
#' @param lr_lalpha Opacity of linear regression line.
#' @param ts_shape Symbol to use for time series points.
#' @param xylim Vector of (lo,hi) limits used as limits on the correlation plot 
#' axes -- useful for zooming in.
#' @param channel Data channel to use for PM2.5 -- one of "a", "b or "ab".
#' @param replaceOutliers Logical specifying whether or not to replace outliers.
#' @param qc_algorithm Named QC algorithm to apply to hourly aggregation stats.
#' @param min_count Aggregation bins with fewer than `min_count` measurements
#' will be marked as `NA`.
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
#' 
#' \donttest{
#' library(AirSensor)
#' 
#' pat <- example_pat
#' pat_externalFit(pat)
#' }

pat_externalFit <- function(
  pat = NULL,
  showPlot = TRUE,
  size = 1,
  pa_color = "purple",
  pwfsl_color = "black",
  alpha = 0.5,
  lr_shape = 15,
  lr_color = "black",
  lr_lwd = 1.5,
  lr_lcolor = "tomato",
  lr_lalpha = 0.45,
  ts_shape = 1,
  xylim = NULL,
  channel = "ab",
  replaceOutliers = TRUE,
  qc_algorithm = "hourly_AB_01",
  # TODO:  allow FUN as an argument
  min_count = 20
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  # For easier access
  meta <- pat$meta
  data <- pat$data
  
  # ----- Assemble data --------------------------------------------------------
  
  if ( replaceOutliers )
    pat <- pat_outliers(pat, showPlot = FALSE, replace = TRUE)
  
  # Get the hourly aggregated PurpleAir data
  paHourly_data <-
    pat %>% 
    pat_createAirSensor( 
      parameter = 'pm25',
      FUN = PurpleAirQC_hourly_AB_02,
      min_count = min_count
    ) %>% 
    PWFSLSmoke::monitor_extractData()
  names(paHourly_data) <- c("datetime", "pa_pm25")
  
  # Get the PWFSL monitor data
  monitorID <- pat$meta$pwfsl_closestMonitorID
  tlim <- range(paHourly_data$datetime)
  pwfsl_monitor <-
    PWFSLSmoke::monitor_load(tlim[1], tlim[2], monitorIDs = monitorID) %>%
    PWFSLSmoke::monitor_subset(tlim = tlim)
  pwfsl_data <-
    pwfsl_monitor %>%
    PWFSLSmoke::monitor_extractData()
  names(pwfsl_data) <- c("datetime", "pwfsl_pm25")
  
  # Get monitor names for labeling
  pwfsl_siteName <- pwfsl_monitor$meta$siteName
  pwfsl_agencyName <- pwfsl_monitor$meta$agencyName
  
  # Combine data from both monitors into one dataframe
  both_data <- dplyr::full_join(paHourly_data, pwfsl_data, by = "datetime")
  
  # Create a tidy dataframe appropriate for ggplot
  tidy_data <-
    both_data %>%
    tidyr::gather("source", "pm25", -.data$datetime)
  
  # Define square xy limit now that we have the data for both monitors
  if ( is.null(xylim) ) {
    dataMin <- min(c(0, both_data$pa_pm25, both_data$pwfsl_pm25), na.rm = TRUE)
    dataMax <- max(c(both_data$pa_pm25, both_data$pwfsl_pm25), na.rm = TRUE)
    xylim <- c(dataMin, dataMax)
  }
  
  # ----- Linear model ---------------------------------------------------------
  
  # Model PWSFL as a function of PurpleAir (data should lie on a line)
  model <- lm(both_data$pwfsl_pm25 ~ both_data$pa_pm25, subset = NULL, 
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
    
    timezone <- pat$meta$timezone[1]
    year <- strftime(pat$data$datetime[1], "%Y", tz=timezone)
    
    # LH Linear regression plot
    lr_plot <- 
      both_data %>% 
      ggplot2::ggplot(ggplot2::aes(x = .data$pa_pm25, y = .data$pwfsl_pm25)) + 
      ggplot2::geom_point(size = size, 
                          shape = lr_shape,
                          color = lr_color,
                          alpha = alpha) + 
      ggplot2::geom_smooth(method = "lm", size = 0, alpha = 0.45) +
      ggplot2::stat_smooth(geom = "line", color = lr_lcolor, alpha = lr_lalpha, 
                           method = "lm", size = lr_lwd) + 
      ggplot2::labs(title = "Correlation", 
                    x = paste0("PurpleAir: \"", pat$meta$label, "\""),
                    y = paste0("PWFSL: \"", pwfsl_siteName, "\"")) + 
      ggplot2::theme_bw() + 
      ggplot2::xlim(xylim) +
      ggplot2::ylim(xylim) +
      ggplot2::coord_fixed() +    # square aspect ratio
      equationLabel
    
    # Set time axis to sensor local time
    tidy_data$datetime <- lubridate::with_tz(tidy_data$datetime, 
                                             tzone = timezone)
    
    # Time series PM 2.5 plot
    ts_plot <-
      tidy_data %>%
      ggplot2::ggplot() +
      ggplot2::geom_point(ggplot2::aes(x = .data$datetime,
                                       y = .data$pm25,
                                       color = source),
                          size = size,
                          shape = ts_shape,
                          alpha = alpha) +
      ggplot2::scale_color_manual(values = c(pa_color, pwfsl_color),
                                  name = "Source",
                                  labels = c("PurpleAir", "PWFSL")) +
      ggplot2::ylim(xylim) +
      ggplot2::ggtitle(expression("PM"[2.5])) + 
      ggplot2::xlab(year) + ggplot2::ylab("\u03bcg / m\u00b3")
    
    # Gather and arrange the linear regression and time series plots with a banner title
    roundedDistance <- round((pat$meta$pwfsl_closestDistance / 1000), 1)
    bannerText <- paste0("Sensor / Monitor Comparison -- Distance: ",
                         roundedDistance, "km")
    bannerGrob <- grid::textGrob(bannerText,
                             just = "left",
                             x = 0.025,
                             gp = grid::gpar(fontsize = 20, col="grey50"))
    
    plot <- gridExtra::grid.arrange(bannerGrob, lr_plot, ts_plot, 
                                    ncol = 1, heights = c(1, 6, 3))
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(invisible(model))
  
}










