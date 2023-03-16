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
#' @param timezone Olson timezone used for the time axis. (Defaults to 
#' \code{pat} local time.)
#' 
#' @description A plotting function that uses ggplot2 to display multiple 
#' ggplot objects in a single pane. Can either be passed individual ggplot 
#' objects OR a pat object and a plot type. 
#' Typical usage would be to supply \code{pat} and use the \code{plottype} 
#' argument to quickly display preformatted plots. 
#' 
#' Available \code{plottype} options include:
#' 
#' \itemize{
#' \item{\code{"all"} -- pm25_A, pm25_B, temperature, humidity}
#' \item{\code{"pm25_a"} -- PM2.5 from channel A only}
#' \item{\code{"pm25_b"} -- PM2.5 from channel B only}
#' \item{\code{"pm25"} -- PM2.5 from channels A and B in separate plots}
#' \item{\code{"pm25_over"} -- PM2.5 from channels A and B in the same plot}
#' \item{\code{"aux"} -- auxiliary data (temperature, humidity)}
#' } 
#' 
#' @return A ggplot object.
#' 
#' @note Additional documentation of the multiplot algorithm is available at 
#' cookbook-r.com.
#' 
#' @examples
#' \donttest{
#' library(AirSensor)
#' 
#' example_pat %>%
#'   pat_multiPlot(plottype = "pm25", alpha = 0.5)
#' }

pat_multiPlot <- function(
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
  alpha = 0.5,
  timezone = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  MazamaCoreUtils::stopIfNull(plottype)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  # Use sensor timezone as default
  if ( is.null(timezone) )
    timezone <- pat$meta$timezone
  
  # Be tolerant of capitalization
  plottype <- tolower(plottype)
  
  # ----- Reduce large datasets by sampling ------------------------------------
  
  if ( !is.null(sampleSize) ) { 
    
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
  
  # NOTE:  Convert pat time axis to the selected timezone for proper formatting
  # NOTE:  by ggplot2.
  pat$data$datetime <- lubridate::with_tz(pat$data$datetime, tzone = timezone)
  
  # Default y limits
  if ( is.null(ylim) ) {
    if ( plottype == "pm25_a") {
      ylim <- range(pat$data$pm25_A, na.rm = TRUE)
    } else if ( plottype == "pm25_b" ) {
      ylim <- range(pat$data$pm25_B, na.rm = TRUE)
    } else {
      # Use the same y limits for both plots
      ylim <- range(c(pat$data$pm25_A, pat$data$pm25_B), na.rm = TRUE)
    }
  }
  
  # Labels
  yearLabel <- strftime(pat$data$datetime[1], "%Y (%Z)", tz = timezone)
  
  # * channelA -----------------------------------------------------------------
  
  channelA <- 
    ggplot2::ggplot(pat$data) + 
    ggplot2::geom_point(
      ggplot2::aes(.data$datetime, .data$pm25_A),
      size = a_size,
      shape = a_shape,
      color = a_color,
      alpha = alpha
    ) + 
    ggplot2::scale_x_datetime(breaks = '1 day', date_labels = '%b %d') + 
    ggplot2::ylim(ylim) +
    ggplot2::labs(
      x = yearLabel,
      y = "\u03bcg / m\u00b3",
      title = expression("Channel A PM"[2.5]),
      subtitle = pat$meta$label
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11),
      plot.subtitle = ggplot2::element_text(size = 8)
    )
  
  # * channelB -----------------------------------------------------------------
  
  channelB <- 
    ggplot2::ggplot(pat$data) + 
    ggplot2::geom_point(
      ggplot2::aes(.data$datetime, .data$pm25_B),
      size = b_size,
      shape = b_shape,
      color = b_color,
      alpha = alpha
    ) + 
    ggplot2::scale_x_datetime(breaks = '1 day', date_labels = '%b %d') + 
    ggplot2::ylim(ylim) +
    ggplot2::labs(
      x = yearLabel,
      y = "\u03bcg / m\u00b3",
      title = expression("Channel B PM"[2.5]),
      subtitle = pat$meta$label
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11),
      plot.subtitle = ggplot2::element_text(size = 8)
    )
  
  # * channelAB ----------------------------------------------------------------
  
  channelAB <- 
    ggplot2::ggplot(pat$data) +
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
    ggplot2::scale_x_datetime(breaks = '1 day', date_labels = '%b %d') + 
    ggplot2::ylim(ylim) +
    ggplot2::labs(
      x = yearLabel, 
      y = "\u03bcg / m\u00b3", 
      title = expression("Channel A/B PM"[2.5]), 
      subtitle = pat$meta$label
    ) + 
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11),
      plot.subtitle = ggplot2::element_text(size = 8)
    )
  
  # * temperature --------------------------------------------------------------
  
  temperature <-   
    ggplot2::ggplot(pat$data) + 
    ggplot2::geom_point(
      ggplot2::aes(x = .data$datetime, y = .data$temperature),
      size = t_size, 
      shape = t_shape,
      color = t_color,
      alpha = alpha
    ) + 
    ggplot2::scale_x_datetime(breaks = '1 day', date_labels = '%b %d') +
    ggplot2::labs(
      x = yearLabel, 
      y = "\u00b0F", 
      title = expression("Temperature"), 
      subtitle = pat$meta$label
    ) + 
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11),
      plot.subtitle = ggplot2::element_text(size = 8)
    )
  
  # * humidity -----------------------------------------------------------------
  
  humidity <-   
    ggplot2::ggplot(pat$data) + 
    ggplot2::geom_point(
      ggplot2::aes(x = .data$datetime, y = .data$humidity), 
      size = h_size, 
      shape = h_shape,
      color = h_color,
      alpha = alpha
    ) + 
    ggplot2::scale_x_datetime(breaks = '1 day', date_labels = '%b %d') +
    ggplot2::labs(
      x = yearLabel, 
      y = "RH%", 
      title = expression("Humidity"), 
      subtitle = pat$meta$label
    ) + 
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 12),
      plot.subtitle = ggplot2::element_text(size = 8)
    )
  
  # ----- Assemble multi_ggplot ------------------------------------------------
  
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
    
    gg <- multi_ggplot(channelA, channelB, humidity, temperature, cols = columns)
    
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(gg)
  
}

# ===== ALIASES ===============================================================

#' @rdname pat_multiPlot
#' @export
pat_multiplot <- pat_multiPlot

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  setArchiveBaseUrl("https://airsensor.aqmd.gov/PurpleAir/v1")
  
  pat <- 
    pat_load(
      label = "SCNP_05", 
      startdate = "2020-02-10",   # from beginning of start
      enddate = "2020-02-15",     # to *beginning* of end
      timezone = "America/Los_Angeles"
    )
  
  plottype <- "pm25_a"
  sampleSize <- 5000
  columns <- NULL
  ylim <- NULL
  a_size <- 1
  a_shape <- 15
  a_color <- rgb(0.9, 0.25, 0.2)
  b_size <- 1
  b_shape <- 15
  b_color <- rgb(0.2, 0.25, 0.9)
  t_size <- 1
  t_shape <- 15
  t_color <- "black"
  h_size <- 1
  h_shape <- 15
  h_color <- "black"
  alpha <- 0.5
  timezone <- "UTC"

  pat_multiPlot(
    pat = pat, 
    plottype = plottype, 
    sampleSize = sampleSize,
    columns = columns,
    ylim = ylim,
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
    alpha = 0.5,
    timezone = timezone
  )
    
}
