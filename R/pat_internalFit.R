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
#' @param shape Symbol to use for points.
#' @param color Color of points.
#' @param alpha Opacity of points.
#' @param xylim Vector of (lo,hi) limits used as limits on the correlation plot 
#' axes -- useful for zooming in.
#' 
#' @description Uses a liner model to fit data from channel B to data from 
#' channel A.
#' 
#' A diagnostic plot is produced if `showPlot = TRUE`.
#' 
#' @return A linear model, fitting the `pat` B channel readings to A channel 
#' readings.
#' 
#' @examples
#' \dontrun{
#' pat_internalFit(pat = example_pat)
#' }

pat_internalFit <- function(
  pat = NULL,
  showPlot = TRUE,
  size = 1,
  shape = 15,
  color = "purple",
  alpha = 0.25,
  xylim = NULL
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
    
    # LH Linear regression plot
    lm_plot <- 
      pat$data %>% 
      ggplot2::ggplot(ggplot2::aes(x = .data$pm25_B, y = .data$pm25_A)) + 
      ggplot2::geom_point(size = size, 
                          shape = shape,
                          color = color,
                          alpha = alpha) + 
      ggplot2::geom_smooth(method = "lm", color = "gray80", alpha = 1.0) + 
      ggplot2::labs(title = "Channel A vs. Channel B", 
                    x = "Channel B PM 2.5 (\U00B5g/m3)", 
                    y = "Channel A PM 2.5 (\U00B5g/m3)") + 
      ggplot2::theme_bw() + 
      ggplot2::xlim(xylim) +
      ggplot2::ylim(xylim) +
      ggplot2::coord_fixed() +    # square aspect ratio
      equationLabel
    
    # # RH pm25_over plot
    # # TODO: Fix printing on ggmultiplot
    # pm25_plot <- invisible(pat_multiplot(pat = pat, 
    #                                      plottype = "pm25_over") )
    
    # Copied from pat_multiplot 
    
    # Labels
    timezone <- pat$meta$timezone[1]
    year <- strftime(pat$data$datetime[1], "%Y", tz=timezone)
    
    # TODO:  Do we need to do this to get local timezones?
    # # Create a tibble
    # tbl <- 
    #   dplyr::tibble(datetime = lubridate::with_tz(pat$data$datetime, timezone),
    #                 pm25_A = pat$data$pm25_A, 
    #                 pm25_B = pat$data$pm25_B, 
    #                 humidity = pat$data$humidity, 
    #                 temp = pat$data$temperature)
    
    pm25_plot <- 
      pat$data %>% 
      ggplot2::ggplot() +
      ggplot2::geom_point(ggplot2::aes(x = .data$datetime, y = .data$pm25_A),
                          size = size,
                          shape = shape,
                          color = rgb(0.9, 0.25, 0.2), # pat_multiplot default
                          alpha = alpha) +
      ggplot2::geom_point(ggplot2::aes(x = .data$datetime, y = .data$pm25_B),
                          size = size,
                          shape = shape,
                          color = rgb(0.2, 0.25, 0.9), # pat_multiplot default
                          alpha = alpha) +
      ggplot2::ylim(xylim) +
      ggplot2::ggtitle(expression("Channel A/B PM"[2.5])) + 
      ggplot2::xlab(year) + ggplot2::ylab("\u03bcg / m\u00b3") 
    
    
    plot <- multi_ggplot(lm_plot, pm25_plot, 
                         cols = 1, plotList = NULL)
    
    print(plot)
    
  }
  
  return(invisible(model))
  
}

