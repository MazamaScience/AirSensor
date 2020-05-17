#' @export
#' @importFrom rlang .data
#' @importFrom stats lm
#' @import dplyr
#' @import graphics
#' 
#' @title Linear model fitting of channel A and B time series data
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param showPlot Logical specifying whether to generate a model fit plot.
#' @param size Size of points.
#' @param a_color Color of time series channel A points.
#' @param b_color Color of time series channel B points.
#' @param alpha Opacity of points.
#' @param lr_shape Symbol to use for linear regression points.
#' @param lr_color Color of linear regression points.
#' @param lr_lwd Width of linear regression line.
#' @param lr_lcolor Color of linear regression line.
#' @param lr_lalpha Opacity of linear regression line.
#' @param ts_shape Symbol to use for time series points.
#' @param xylim Vector of (lo,hi) limits used as limits on the correlation plot 
#' axes -- useful for zooming in.
#' 
#' @description Uses a linear model to fit data from channel B to data from 
#' channel A.
#' 
#' A diagnostic plot is produced if \code{showPlot = TRUE}.
#' 
#' @return A linear model, fitting the \code{pat} B channel readings to A channel 
#' readings.
#' 
#' @examples
#' library(AirSensor)
#' 
#' example_pat %>%
#'   pat_internalFit()

pat_internalFit <- function(
  pat = NULL,
  showPlot = TRUE,
  size = 1,
  a_color = "red",
  b_color = "blue",
  alpha = 0.25,
  lr_shape = 15,
  lr_color = "black",
  lr_lwd = 1.5,
  lr_lcolor = "tomato",
  lr_lalpha = 0.45,
  ts_shape = 1,
  xylim = NULL
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
  
  if ( is.null(xylim) ) {
    dataMin <- min(c(0, data$pm25_A, data$pm25_B), na.rm = TRUE)
    dataMax <- max(c(data$pm25_A, data$pm25_B), na.rm = TRUE)
    xylim <- c(dataMin, dataMax)
  }
  
  a_pm25 <- 
    data %>%
    dplyr::select(.data$datetime, .data$pm25_A)
  
  b_pm25 <- 
    data %>%
    dplyr::select(.data$datetime, .data$pm25_B)
  
  # Create a tidy dataframe appropriate for ggplot
  tidy_data <-
    dplyr::full_join(a_pm25, b_pm25, by = "datetime") %>%
    tidyr::gather("channel", "pm25", -.data$datetime)
  
  # ----- Linear model ---------------------------------------------------------
  
  # Model A as a function of B (data should lie on a line)
  model <- lm(data$pm25_A ~ data$pm25_B, subset = NULL, weights = NULL)
  
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
    
    timezone <- pat$meta$timezone[1]
    year <- strftime(pat$data$datetime[1], "%Y", tz=timezone)
    
    # LH Linear regression plot
    lr_plot <- 
      pat$data %>% 
      ggplot2::ggplot(ggplot2::aes(x = .data$pm25_B, y = .data$pm25_A)) + 
      ggplot2::geom_point(size = size, 
                         shape = lr_shape,
                       color = lr_color,
                       alpha = alpha) + 
      ggplot2::geom_smooth(formula = y ~ x, method = "lm", 
                           size = 0, alpha = 0.45) +
      ggplot2::stat_smooth(formula = y ~ x, method = "lm", geom = "line",
                           color = lr_lcolor, alpha = lr_lalpha, size = lr_lwd) + 
      ggplot2::labs(title = "Channel Linear Regression", 
                    x = "Channel B (\u03bcg / m\u00b3)", 
                    y = "Channel A (\u03bcg / m\u00b3)") + 
      ggplot2::theme_bw() + 
      ggplot2::xlim(xylim) +
      ggplot2::ylim(xylim) +
      ggplot2::coord_fixed() +    # square aspect ratio
      equationLabel
    
    # Set time axis to sensor local time
    tidy_data$datetime <- lubridate::with_tz(tidy_data$datetime, 
                                             tzone = timezone)
    
    ts_plot <-
      tidy_data %>%
      ggplot2::ggplot() +
      ggplot2::geom_point(ggplot2::aes(x = .data$datetime,
                                       y = .data$pm25,
                                       color = .data$channel),
                          size = size,
                          shape = ts_shape,
                          alpha = alpha) +
      ggplot2::scale_color_manual(values = c(a_color, b_color),
                                  name = "Channel",
                                  labels = c("A", "B")) +
      ggplot2::ylim(xylim) +
      ggplot2::ggtitle(expression("PM"[2.5])) + 
      ggplot2::xlab(year) + ggplot2::ylab("\u03bcg / m\u00b3")
    
    
    # Gather and arrange the linear regression and time series plots with a banner title
    bannerText <- paste0("A / B Channel Comparison -- ", pat$meta$label)
    bannerGrob <- grid::textGrob(bannerText,
                                 just = "left",
                                 x = 0.025,
                                 gp = grid::gpar(fontsize = 18, col="grey50"))
    
    plot <- gridExtra::grid.arrange(bannerGrob, lr_plot, ts_plot, 
                                    ncol = 1, heights = c(1, 6, 3))
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(invisible(model))
  
}

