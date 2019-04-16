#' @keywords pa_timeseries
#' @export
#' @title Create Interactive Time Series Plot
#' @param pat Purple Air Timeseries "pat" object from \code{createPATimeseriesObject()}
#' @param plottype Quick-reference plot types: "pm25", "humidity", "temperature"
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
                        plottype = NULL,
                        title = NULL,
                        xlab = NULL,
                        ylab = NULL,
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
  
  
  if ( is.null(title) )( title <- pat$meta$label )
  
  show <- ifelse(showLegend, "always", "never")
  
  # Create dygraph
  # TODO: Use custom options provided by user and custom colors (R&B for pm25)
  
  makeGraph <- function(timeseriesMatrix) {
    
    dygraphs::dygraph(timeseriesMatrix, main = title, xlab = xlab, ylab = ylab) %>%
      dygraphs::dyOptions(useDataTimezone = TRUE) %>% # Always show local time
      dygraphs::dyLegend(show = show, width = 250, labelsSeparateLines = TRUE) %>%
      dygraphs::dyRangeSelector(dateWindow = dateWindow) %>%
      dygraphs::dyRoller(rollPeriod = rollPeriod)
    
  }
  
  
  # Create an xts from all data columns except the first which is 'datetime'
  if ( is.null(plottype) || plottype == "pm25" ) { 
    
    channelA <- xts::xts(x=pat$data$pm25_A, order.by=datetime, tzone = tzone)
    channelB <- xts::xts(x=pat$data$pm25_B, order.by=datetime, tzone = tzone)
    timeseriesMatrix <- cbind(channelA, channelB)
    names(timeseriesMatrix) <- c("Channel A", "Channel B")
    
    if ( is.null(ylab) )( ylab <- "\u03bcg / m\u00b3" )
    
    makeGraph(timeseriesMatrix)
    
  } else if ( plottype == "humidity" ) {
    
    humidity <- xts::xts(x=pat$data$humidity, order.by=datetime, tzone = tzone)
    timeseriesMatrix <- cbind(humidity)
    names(timeseriesMatrix) <- c(paste0(pat$meta$label, "-Humidity"))
    
    if ( is.null(ylab) )( ylab <- "RH%")
    
    makeGraph(timeseriesMatrix)
    
  } else if ( plottype == "temperature" || plottype == "temp" ) {
    
    temperature <- xts::xts(x=pat$data$temperature, order.by=datetime, tzone = tzone)
    timeseriesMatrix <- cbind(temperature)
    names(timeseriesMatrix) <- c(paste0(pat$meta$label, "-Temperature"))
    
    if ( is.null(ylab) )( ylab <- "\u00b0F")
    
    makeGraph(timeseriesMatrix)
    
  }

}