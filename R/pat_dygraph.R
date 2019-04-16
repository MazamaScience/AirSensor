#' @keywords pa_timeseries
#' @export
#' @title Create Interactive Time Series Plot
#' @param pat Purple Air Timeseries "pat" object from \code{createPATimeseriesObject()}
#' @param title title text
#' @param xlab optional title for the x axis
#' @param ylab optional title for the y axis
#' @param tlim optional vector with start and end times (integer or character
#'   representing YYYYMMDD[HH])
#' @param rollPeriod rolling mean to be applied to the data
#' @param showLegend logical to toggle display of the legend
#' @description This function creates interactive graphs that will be displayed
#'   in RStudio's 'Viewer' tab.
#' @return Initiates the interactive dygraph plot in RStudio's 'Viewer' tab.
#' @examples
#' \dontrun{
#' pas <- example_pas
#' nb <- pat_load(pas, "North Bend Weather", startdate = 20180801, enddate = 20180901)
#' subset_nb <- pat_sample(pat=nb, sampleSize = 1000, setSeed = 1)
#' pat_dygraph(pat = subset_nb, xlab = "2018", rollPeriod = 7)
#' }

pat_dygraph <- function(pat,
                        title = NULL,
                        xlab = NULL,
                        ylab = "PM2.5 Concentration",
                        tlim = NULL,
                        rollPeriod = 1,
                        showLegend = TRUE) {
  
  # Sanity check
  if ( is.null(pat) ) {
    stop("must provide pat object")
  }
  
  # Convert tlim to POSIXct
  if ( !is.null(tlim) ) {
    dateWindow <- parseDatetime(tlim)
  } else {
    dateWindow <- NULL
  }
  
  # Set timezone
  tzCount <- length(unique(pat$meta$timezone))
  if (tzCount > 1) {
    warning(paste0(tzCount, " timezones found. Using UTC time."))
    tzone <- "UTC"
  } else {
    tzone <- unique(pat$meta$timezone)
  }
  
  # Access time
  datetime <- pat$data$datetime
  
  # Create an xts from all data columns except the first which is 'datetime'
  timeseriesData <- xts::xts(pat$data$pm25_B, datetime, tzone = tzone)
  
  # Add siteNames
  # Sanity check for existence of siteName column
  
  if ( is.null(pat$meta$label) ) {
    pat$meta$label <- "N/A"
  }
  # Sanity check for existence of names
  siteNames <- ifelse(is.na(pat$meta$label),
                      names(pat$data)[-1], pat$meta$label)
  names(timeseriesData) <- siteNames
  
  show <- ifelse(showLegend, "always", "never")
  
  # TODO: Get multiple PurpleAir sensors to plot, possibly by passing a list of
  #       pat objects. Currently only one can be plotted at a time. 
  
  # Create dygraph
  dygraphs::dygraph(timeseriesData, main = title, xlab = xlab, ylab = ylab) %>%
    dygraphs::dyOptions(useDataTimezone = TRUE) %>% # Always show local time
    dygraphs::dyLegend(show = show, width = 250, labelsSeparateLines = TRUE) %>%
    dygraphs::dyRangeSelector(dateWindow = dateWindow) %>%
    dygraphs::dyRoller(rollPeriod = rollPeriod)
  
}