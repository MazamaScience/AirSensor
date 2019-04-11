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
  
  if ( !pat_isEmpty(pat) )
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
  
  if ( showPlot ) {
    
    # Draw correlation plot between the PurpleAir monitor's A and B channel 
    # PM 2.5 readings
    
    par(pty = "s") # to make it square
    
    point_color <- rgb(red = 0.7, green = 0.25, blue = 0.7, alpha = 0.5)
    
    pm25_max <- max(c(data$pm25_A, data$pm25_B), na.rm = TRUE)
    
    plot(data$pm25_A ~ data$pm25_B, las = 1,
         xlim = c(0, pm25_max), ylim = c(0, pm25_max),
         pch = 18, cex = 1.75, col = point_color,
         xlab = "Channel B PM 2.5 (\U00B5g/m3)", ylab = "Channel A PM 2.5 (\U00B5g/m3)")
    
    title("Channel A/B Data Consistency")
    mtext(pat$meta$label, 3, 0)
    
    grid(nx = NULL, ny = NULL, col = "gray")
    
    abline(model)
    
    legend(x = "topleft", cex = 0.8,
           legend = c(paste0("slope = ", round(slope, digits = 2)),
                      paste0("intercept = ", round(intercept, digits = 1)),
                      paste0("R\U00B2 = ", round(r_squared, digits = 3))))
    
    par(pty = "m")
    
  }
  
  return(invisible(model))
  
}

