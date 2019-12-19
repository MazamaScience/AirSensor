#' @export
#' @importFrom rlang .data
#' 
#' @title Comparison of Purple Air and federal monitoring data
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param ylim Vector of (lo,hi) y-axis limits. 
#' @param replaceOutliers Logical specifying whether replace outliers in the
#'   \emph{pat} object.
#' @param a_size Size of pm25_A points.
#' @param a_shape Symbol to use for pm25_A points.
#' @param a_color Color of pm25_A points.
#' @param b_size Size of pm25_B points.
#' @param b_shape Symbol to use for pm25_B points.
#' @param b_color Color of pm25_B points.
#' @param ab_alpha Opacity of pm25_A, pm25_B points.
#' @param hourly_size Size of hourly points.
#' @param hourly_shape Symbol to use for hourly points
#' @param hourly_stroke Line width of hourly points
#' @param pa_color Color of hourly points
#' @param pwfsl_color Color of hourly points
#' 
#' @description Creates and regurns a ggplot object that plots raw \emph{pat}
#' data, hourly aggregated \emph{pat} data and hourly data from the nearest
#' federal monitor from the PWFSL database.
#' 
#' @return A ggplot object.
#' 
#' @examples
#' \donttest{
#' pat_monitorComparison(example_pat)
#' }

pat_monitorComparison <- function(
  pat = NULL,
  ylim = NULL,
  replaceOutliers = TRUE,
  a_size = 1,
  a_shape = 15,
  a_color = "gray80", # rgb(0.9, 0.25, 0.2),
  b_size = 1,
  b_shape = 15,
  b_color = "gray80", # rgb(0.2, 0.25, 0.9),
  ab_alpha = 0.5,
  hourly_size = 2,
  hourly_shape = 1,
  hourly_stroke = 0.6,
  pa_color = "purple",
  pwfsl_color = "black"
) {
  
  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")

  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")

  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  # ----- Assemble data --------------------------------------------------------
  
  if ( replaceOutliers )
    pat <- pat_outliers(pat, showPlot = FALSE, replace = TRUE)
  
  # Get the hourly aggregated data
  paHourly_data <-
    pat %>% 
    pat_createAirSensor(period = "1 hour") %>%
    PWFSLSmoke::monitor_extractData()
  
  names(paHourly_data) <- c("datetime", "PA hourly")

  tlim <- range(paHourly_data$datetime)
  
  # Get the PWFSL monitor data
  monitorID <- pat$meta$pwfsl_closestMonitorID
  pwfsl_monitor <-
    PWFSLSmoke::monitor_load(tlim[1], tlim[2], monitorIDs = monitorID) %>%
    PWFSLSmoke::monitor_subset(tlim = tlim)
  pwfsl_data <-
    pwfsl_monitor %>%
    PWFSLSmoke::monitor_extractData()

  names(pwfsl_data) <- c("datetime", "PWFSL")

  # Get monitor names for labeling
  pwfsl_siteName <- pwfsl_monitor$meta$siteName
  pwfsl_agencyName <- pwfsl_monitor$meta$agencyName
  
  # Create a tidy dataframe appropriate for ggplot
  tidy_data <-
    dplyr::full_join(paHourly_data, pwfsl_data, by = "datetime") %>%
    tidyr::gather("source", "pm25", -.data$datetime)
  
  # ----- Plot styling ---------------------------------------------------------

  if ( is.null(ylim) ) {
    dataMin <- min(c(0, pat$data$pm25_A, pat$data$pm25_B, tidy_data$pm25), 
                   na.rm = TRUE)
    dataMax <- max(c(pat$data$pm25_A, pat$data$pm25_B, tidy_data$pm25), 
                   na.rm = TRUE)
    ylim <- c(dataMin, dataMax)
  }
  
  # Labels
  timezone <- pat$meta$timezone[1]
  year <- strftime(pat$data$datetime[1], "%Y", tz=timezone)
  title <- paste0(
    "Sensor / Monitor comparison -- PurpleAir: \"",
    pat$meta$label,
    "\" is ",
    round((pat$meta$pwfsl_closestDistance/1000),1),
    " km from PWFSL: \"",
    pwfsl_siteName,
    "\""
  )
  
  # ----- Construct plot -------------------------------------------------------
  
  # Set time axis to sensor local time
  pat$data$datetime <- lubridate::with_tz(pat$data$datetime, tzone = timezone)
  tidy_data$datetime <- lubridate::with_tz(tidy_data$datetime, tzone = timezone)
  
  pm25_plot <-
    pat$data %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = .data$datetime, y = .data$pm25_A),
                        size = a_size,
                        shape = a_shape,
                        color = a_color,
                        alpha = ab_alpha) +
    ggplot2::geom_point(ggplot2::aes(x = .data$datetime, y = .data$pm25_B),
                        size = b_size,
                        shape = b_shape,
                        color = b_color,
                        alpha = ab_alpha) +
    ggplot2::geom_point(data = tidy_data,
                        ggplot2::aes(x = .data$datetime, y = .data$pm25, color = source),
                        size = hourly_size,
                        shape = hourly_shape,
                        stroke = hourly_stroke,
                        alpha = 1) +
    ggplot2::scale_color_manual(values=c(pa_color, pwfsl_color)) +
    
    ggplot2::ylim(ylim) +
    
    ggplot2::ggtitle(title) +
    ggplot2::xlab(year) + 
    ggplot2::ylab("\u03bcg / m\u00b3")
  
  # ----- Return ---------------------------------------------------------------
  
  return(pm25_plot)

}

# ===== DEBUGGING ============================================================

if ( FALSE ) {
  
  pas <- pas_load()
  pat <- pat_load(pas, label = "SCPR_19", startdate = 20190618, enddate = 20190629)
  ylim <- NULL
  replaceOutliers <- TRUE
  a_size <- 1
  a_shape <- 15
  a_color <- "gray80"
  b_size <- 1
  b_shape <- 15
  b_color <- "gray80"
  ab_alpha <- 0.5
  hourly_size <- 2
  hourly_shape <- 1
  hourly_stroke <- 0.6
  pa_color <- "purple"
  pwfsl_color <- "black"
  
}

