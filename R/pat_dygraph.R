#' @keywords pa_timeseries
#' @export
#' 
#' @title Interactive time series plot
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object from \code{pat_createNew()}
#' @param parameter Data to display: "pm25", "humidity", "temperature" or "pressure".
#' @param sampleSize Either an integer or fraction to determine sample size.
#' @param title title text
#' @param xlab optional title for the x axis
#' @param ylab optional title for the y axis
#' @param tlim optional vector with start and end times (integer or character
#'   representing YYYYMMDD[HH])
#' @param rollPeriod Width (hours) of rolling mean to be applied to the data.
#' @param showLegend Logical specifying whether to add a legend.
#' @param colors Vector of colors to be used for plotting.
#' @param timezone Olson timezone used to interpret \code{tlim}. (Defaults to 
#' \code{pat} local time.)
#' 
#' @description This function creates interactive graphs that will be displayed
#' in RStudio's 'Viewer' tab.
#' 
#' The list of available parameters include:
#' 
#' \itemize{
#' \item{\code{pm25} -- A and B channel PM2.5 (ug/m3)}
#' \item{\code{temperature} -- temperature (F)}
#' \item{\code{humidity} -- humidity (\%)}
#' \item{\code{pressure} -- pressure (hPa)}
#' }
#' 
#' @return Initiates the interactive dygraph plot in RStudio's 'Viewer' tab.
#' 

pat_dygraph <- function(
  pat = NULL,
  parameter = "pm25",
  sampleSize = 5000,
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  tlim = NULL,
  rollPeriod = 1,
  showLegend = TRUE,
  colors = NULL,
  timezone = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  # Use sensor timezone as default
  if ( is.null(timezone) )
    timezone <- pat$meta$timezone
  
  # ----- Reduce large datasets by sampling ------------------------------------
  
  if ( !is.null(sampleSize) ) { 
    
    if ( sampleSize > 1 ) {
      pat <- 
        pat %>% 
        pat_sample(sampleSize = sampleSize)
    } else {
      pat <- 
        pat %>% 
        pat_sample(sampleFraction = sampleSize)
    }
    
  }
  
  # ----- Prepare data ---------------------------------------------------------
  
  # Convert tlim to POSIXct
  if ( !is.null(tlim) ) {
    dateWindow <- MazamaCoreUtils::parseDatetime(tlim, timezone = timezone)
  } else {
    dateWindow <- NULL
  }
  
  # Set pat local timezone
  tzCount <- length(unique(pat$meta$timezone))
  if (tzCount > 1) {
    warning(paste0(tzCount, " timezones found. Using UTC time."))
    tzone <- "UTC"
  } else {
    tzone <- unique(pat$meta$timezone)
  }
  
  # Pull out variables
  datetime <- pat$data$datetime
  pm25_A <- pat$data$pm25_A
  pm25_B <- pat$data$pm25_B
  temperature <- pat$data$temperature
  humidity <- pat$data$humidity
  pressure <- pat$data$pressure
  label <- pat$meta$label
  
  # Create an xts from all data columns except the first which is 'datetime'
  
  if ( is.null(parameter) || tolower(parameter) == "pm25" ) { 
    
    channelA <- xts::xts(x = pm25_A, order.by = datetime, tzone = tzone)
    channelB <- xts::xts(x = pm25_B, order.by = datetime, tzone = tzone)
    timeseriesMatrix <- cbind(channelA, channelB)
    names(timeseriesMatrix) <- c("Channel A", "Channel B")
    
    if ( is.null(ylab) )( ylab <- "\u03bcg / m\u00b3" )
    if ( is.null(colors) )( colors <- c("red", "blue")  )
    
  } else if ( tolower(parameter) == "humidity" ) {
    
    humidityData <- xts::xts(x = humidity, order.by = datetime, tzone = tzone)
    timeseriesMatrix <- cbind(humidityData)
    names(timeseriesMatrix) <- c(paste0(label, "-Humidity"))
    
    if ( is.null(ylab) )( ylab <- "RH%")
    
  } else if ( tolower(parameter) == "temperature" || tolower(parameter) == "temp" ) {
    
    temperatureData <- xts::xts(x = temperature, order.by = datetime, tzone = tzone)
    timeseriesMatrix <- cbind(temperatureData)
    names(timeseriesMatrix) <- c(paste0(label, "-Temperature"))
    
    if ( is.null(ylab) )( ylab <- "\u00b0F" )
    
  } else if ( tolower(parameter) == "pressure" || tolower(parameter) == "hpa" ) {
    
    pressureData <- xts::xts(x = pressure, order.by = datetime, tzone = tzone)
    timeseriesMatrix <- cbind(pressureData)
    names(timeseriesMatrix) <- c(paste0(label, "-Pressure"))
    
    if ( is.null(ylab) )( ylab <- "hPa" )
    
  } else {
    
    stop("Required parameter 'parameter' is not recognized")  
    
  }
  
  # ----- Make graph -----------------------------------------------------------
  
  if ( is.null(title) ) title <- label
  
  show <- ifelse(showLegend, "always", "never")
  
  graph <- 
    dygraphs::dygraph(timeseriesMatrix, main = title, xlab = xlab, ylab = ylab) %>%
    dygraphs::dyOptions(useDataTimezone = TRUE) %>% # Always show local time
    dygraphs::dyLegend(show = show, width = 250, labelsSeparateLines = TRUE) %>%
    dygraphs::dyRangeSelector(dateWindow = dateWindow) %>%
    dygraphs::dyRoller(rollPeriod = rollPeriod) %>%
    dygraphs::dyOptions(colors = colors)
  
  # ----- Return ---------------------------------------------------------------
  
  return( graph )
  
}