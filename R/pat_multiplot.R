#' @export
#' @importFrom rlang .data
#' @importFrom grDevices rgb
#' 
#' @title Display multiple plots on one page
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param plottype Quick-reference plot types: "all", "aux", "pm25".
#' @param sampleSize Either an integer or fraction to determine sample size.
#' @param columns Number of columns in the plot layout. Use \code{NULL} for 
#' defaults.
#' @param ylim Vector of (lo,hi) y-axis limits. 
#' @param a_size Size of pm25_A points.
#' @param a_shape Symbol to use for pm25_A points.
#' @param a_color Color of pm25_A points.
#' @param b_size Size of pm25_B points.
#' @param b_shape Symbol to use for pm25_B points.
#' @param b_color Color of pm25_B points.
#' @param t_size Size of temperature points.
#' @param t_shape Symbol to use for temperature points.
#' @param t_color Color of temperature points.
#' @param h_size Size of humidity points.
#' @param h_shape Symbol to use for humidity points.
#' @param h_color Color of humidity points.
#' @param alpha Opacity of points.
#' 
#' @description A plotting function that uses ggplot2 to display multiple 
#' ggplot objects in a single pane. Can either be passed individual ggplot 
#' objects OR a pat object and a plot type. 
#' Typical usage would be to supply \code{pat} and use the \code{plottype} 
#' argument to quickly display preformatted plots. 
#' \itemize{
#' \item{"all": pm25_A, pm25_B, temperature, humidity}
#' \item{"pm25_a": PM2.5 from channel A only}
#' \item{"pm25_b": PM2.5 from channel B only}
#' \item{"pm25": PM2.5 from channels A and B in separate plots}
#' \item{"pm25_over": PM2.5 from channels A and B in the same plot}
#' \item{"aux": auxillary data (temperature, humidity)}
#' } 
#' 
#' @return A ggplot object.
#' 
#' @note Additional documentation of the multiplot algorithm is available at 
#' cookbook-r.com.
#' 
#' @examples
#' \dontrun{
#' pat_multiplot(pat = example_pat, plottype = "pm25", alpha = 0.5)
#' }

pat_multiplot <- function(
  pat = NULL, 
  plottype = "all", 
  sampleSize = 5000,
  columns = NULL,
  ylim = NULL,
  a_size = 1,
  a_shape = 15,
  a_color = rgb(0.9, 0.25, 0.2),
  b_size = 1,
  b_shape = 15,
  b_color = rgb(0.2, 0.25, 0.9),
  t_size = 1,
  t_shape = 15,
  t_color = "black",
  h_size = 1,
  h_shape = 15,
  h_color = "black",
  alpha = 0.5
) {
  
  options(warn = -1)
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !is.null(pat) ) {
    if ( !pat_isPat(pat) )
      stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
    
    if ( pat_isEmpty(pat) )
      stop("Parameter 'pat' has no data.")
  }
  
  # ----- Reduce large datasets by sampling ------------------------------------
  
  if ( !is.null(pat) && !is.null(sampleSize) ) { 
    
    if ( sampleSize > 1 ) {
      pat <- 
        pat %>% 
        pat_sample(sampleSize = sampleSize, keepOutliers = TRUE)
    } else {
      pat <- 
        pat %>% 
        pat_sample(sampleFraction = sampleSize, keepOutliers = TRUE)
    }
    
  }
  
  # ----- Create plots ---------------------------------------------------------
  
  
  # Labels
  timezone <- pat$meta$timezone[1]
  year <- strftime(pat$data$datetime[1], "%Y", tz=timezone)
  
  # Create a tibble
  tbl <- 
    dplyr::tibble(
      datetime = lubridate::with_tz(pat$data$datetime, timezone),
      pm25_A = pat$data$pm25_A, 
      pm25_B = pat$data$pm25_B, 
      humidity = pat$data$humidity, 
      temp = pat$data$temperature
    )
  
  # Default y limits
  if ( is.null(ylim) ) {
    if ( plottype == "pm25_a") {
      ylim <- range(tbl$pm25_A, na.rm = TRUE)
    } else if ( plottype == "pm25_b" ) {
      ylim <- range(tbl$pm25_B, na.rm = TRUE)
    } else {
      # Use the same y limits for both plots
      ylim <- range(c(tbl$pm25_A, tbl$pm25_B), na.rm = TRUE)
    }
  }
  
  channelA <- 
    tbl %>% 
    ggplot2::ggplot(ggplot2::aes(x = .data$datetime, y = .data$pm25_A)) + 
    ggplot2::geom_point(
      size = a_size, 
      shape = a_shape,
      color = a_color,
      alpha = alpha
    ) + 
    ggplot2::ylim(ylim) +
    ggplot2::labs(
      x = year, 
      y = "\u03bcg / m\u00b3", 
      title = expression("Channel A PM"[2.5]), 
      subtitle = pat$meta$label
    ) + 
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11),
      plot.subtitle = ggplot2::element_text(size = 8),
    )
  
  channelB <-   
    tbl %>% 
    ggplot2::ggplot(ggplot2::aes(x = .data$datetime, y = .data$pm25_B)) + 
    ggplot2::geom_point(
      size = b_size, 
      shape = b_shape,
      color = b_color,
      alpha = alpha
    ) + 
    ggplot2::ylim(ylim) +
    ggplot2::labs(
      x = year, 
      y = "\u03bcg / m\u00b3", 
      title = expression("Channel B PM"[2.5]), 
      subtitle = pat$meta$label
    ) + 
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11),
      plot.subtitle = ggplot2::element_text(size = 8),
    )
  
  channelAB <- 
    tbl %>% 
    ggplot2::ggplot() +
    ggplot2::geom_point(
      ggplot2::aes(x = .data$datetime, y = .data$pm25_A),
      size = a_size,
      shape = a_shape,
      color = a_color,
      alpha = alpha
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = .data$datetime, y = .data$pm25_B),
      size = b_size,
      shape = b_shape,
      color = b_color,
      alpha = alpha
    ) +
    ggplot2::ylim(ylim) +
    ggplot2::labs(
      x = year, 
      y = "\u03bcg / m\u00b3", 
      title = expression("Channel A/B PM"[2.5]), 
      subtitle = pat$meta$label
    ) + 
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11),
      plot.subtitle = ggplot2::element_text(size = 8),
    )
  
  temperature <-   
    tbl %>% 
    ggplot2::ggplot(ggplot2::aes(x = .data$datetime, y = .data$temp)) + 
    ggplot2::geom_point(
      size = t_size, 
      shape = t_shape,
      color = t_color,
      alpha = alpha
    ) + 
    ggplot2::labs(
      x = year, 
      y = "\u00b0F", 
      title = expression("Temperature"), 
      subtitle = pat$meta$label
    ) + 
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11),
      plot.subtitle = ggplot2::element_text(size = 8),
    )
  
  humidity <-   
    tbl %>% 
    ggplot2::ggplot(ggplot2::aes(x = .data$datetime, y = .data$humidity)) + 
    ggplot2::geom_point(
      size = h_size, 
      shape = h_shape,
      color = h_color,
      alpha = alpha
    ) + 
    ggplot2::labs(
      x = year, 
      y = "RH%", 
      title = expression("Humidity"), 
      subtitle = pat$meta$label
    ) + 
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12),
      plot.subtitle = ggplot2::element_text(size = 8)
    )
  
  # Assemble multi_ggplot
  if ( plottype == "pm25" ) {
    
    if ( is.null(columns) ) columns <- 1
    gg <- multi_ggplot(channelA, channelB, cols = columns)
    
  } else if ( plottype == "pm25_a" ) {
    
    columns <- 1
    gg <- multi_ggplot(channelA, cols = columns)
    
  } else if ( plottype == "pm25_b" ) {
    
    columns <- 1
    gg <- multi_ggplot(channelB, cols = columns)
    
  } else if ( plottype == "pm25_over" ) {
    
    columns <- 1
    gg <- multi_ggplot(channelAB, cols = columns)
    
  } else if ( plottype == "aux" ) {
    
    if ( is.null(columns) ) columns <- 1
    gg <- multi_ggplot(temperature, humidity, cols = columns)
    
  } else if ( plottype == "all") {
    
    if ( is.null(columns) ) columns <- 2
    
    # Get ordering right
    if ( columns == 1 ) {
      
      gg <- 
        multi_ggplot(
          channelA, 
          channelB, 
          humidity, 
          temperature, 
          cols = columns
        )
      
    } else if ( columns == 2 ) {
      
      gg <- 
        multi_ggplot(
          channelA, 
          humidity, 
          channelB,
          temperature, 
          cols = columns
        )
      
    } else {
      
      gg <- 
        multi_ggplot(
          channelA, 
          channelB, 
          humidity, 
          temperature, 
          cols = columns
        )
      
    }
    
  }
  
  options(warn=0)
  
  return(invisible(gg)) # TODO:  What should pat_multiplot() return?
  
}
