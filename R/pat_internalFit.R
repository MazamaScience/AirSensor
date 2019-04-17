#' @export
#' @importFrom rlang .data
#' @importFrom stats lm
#' @import dplyr
#' @import graphics
#' 
#' @title Return a linear model gitting channel A and B
#' 
#' @param pat Purple Air Timeseries "pat" object
#' @param subset (From `stats::lm`) An optional vector specifying a subset of
#'   observations to be used in the fitting process.
#' @param weights (From `stats::lm`) An optional vector of weights to be used in
#'   the fitting process. Should be NULL or a numeric vector. If non-NULL,
#'   weighted least squares is used with weights weights (that is, minimizing
#'   sum(w*e^2)); otherwise ordinary least squares is used.
#' @param showPlot logical specifying whether to generate a model fit plot
#' 
#' @description Uses a liner model to fit data from channel B to data from 
#' channel A.
#' 
#' A diagnostic plot is produced if `showPlot = TRUE`.
#' 
#' @return A linear model, fitting the `pat` B channel readings to A channel readings.
#' 

pat_internalFit <- function(
  pat,
  subset = NULL,
  weights = NULL,
  showPlot = TRUE
  ) {
  
  # Validate parameters -----------------------------------------------------
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  if ( !is.null(weights) && !is.numeric(weights) ) {
    stop("Parameter 'weights' must be either NULL or a numeric vector.")
  }
  
  # For easier access
  meta <- pat$meta
  data <- pat$data
  
  # Model A as a function of B (data should lie on a line)
  model <- lm(data$pm25_A ~ data$pm25_B, subset = subset, weights = weights)
  
  slope <- as.numeric(model$coefficients[2])      # as.numeric() to remove name
  intercept <- as.numeric(model$coefficients[1])
  r_squared <- summary(model)$r.squared
  
  # Label for linear fit
  equationLabel <- 
    ggplot2::annotate(
      geom = "text", 
      x = 150, 
      y = c(50, 40, 30), 
      label = c(paste0("Slope = ", round(slope, digits = 2)),
                paste0("Intercept = ", round(intercept, digits = 1)),
                paste0("R\U00B2 = ", round(r_squared, digits = 3))) )
  
  if ( showPlot ) { 
    
    plot <- 
      pat$data %>% 
      ggplot2::ggplot(ggplot2::aes(x = .data$pm25_A, y = .data$pm25_B)) + 
      ggplot2::geom_point(shape = 18, color = "purple", alpha = 1/2) + 
      ggplot2::geom_smooth(method = "lm", alpha = 1/2) + 
      ggplot2::labs(title = "Channel A vs. Channel B", 
                    x = "Channel B PM 2.5 (\U00B5g/m3)", 
                    y = "Channel A PM 2.5 (\U00B5g/m3)" ) + 
      ggplot2::theme_bw() + 
      equationLabel
    
    print(plot)
    
  }
  
  return(invisible(model))
  
}

