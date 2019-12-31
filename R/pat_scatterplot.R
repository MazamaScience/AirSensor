#' @export
#' @import graphics
#' 
#' @title Compare all variables in a PurpleAir Timeseries object
#' 
#' @description Creates a multi-panel scatterplot comparing all variables in the
#' \emph{pat} object. If any variables have no valid data, they are omitted from
#' the plot.
#' 
#' The list of available parameters include:
#' 
#' \itemize{
#' \item{\code{datetime} -- measurement time}
#' \item{\code{pm25_A} -- A channel PM2.5 (ug/m3)}
#' \item{\code{pm25_A} -- A channel PM2.5 (ug/m3)}
#' \item{\code{temperature} -- temperature (F)}
#' \item{\code{humidity} -- humidity (\%)}
#' \item{\code{uptime} -- seconds since last reset}
#' \item{\code{adc0} -- analog input voltage}
#' \item{\code{rssi} -- wifi signal strength (dBm)}
#' }
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param parameters Vector of parameters to include.
#' @param sampleSize Integer to determine sample size.
#' @param sampleFraction Fractional sample size.
#' @param shape Symbol to use for points.
#' @param size Size of points.
#' @param color Color of points.
#' @param alpha Opacity of points.
#' 
#' @return Multi-panel ggplot comparing all parameters.
#' 
#' @examples
#' \donttest{
#' pat <- 
#'   example_pat %>% 
#'   pat_filterDate(20180811,20180818)
#' pat_scatterplot(pat, sampleSize = 1000)
#' }

pat_scatterplot <- function(
  pat = NULL,
  parameters = c('datetime', 'pm25_A', 'pm25_B', 'temperature', 'humidity'),
  sampleSize = 5000,
  sampleFraction = NULL,
  size = 0.5,
  shape = 15,
  color = "black",
  alpha = 0.25
) {
  
  # Suppress warnings until the end of this function
  options(warn = -1)
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  if ( !is.null(sampleFraction) )
    sampleSize <- NULL # Disable one 
  
  # For easier access
  meta <- pat$meta
  data <- pat$data
  
  data <- data[,parameters]
  
  # Test for presence of data
  columnIsEmpty <- unlist(lapply(data, function(x) { all(is.na(x)) }))
  goodColumns <- names(which(!columnIsEmpty))
  data <- data[,goodColumns]
  
  if ( sum(columnIsEmpty) > 0 ) {
    badColumns <- names(which(columnIsEmpty))
    badString <- paste(badColumns, collapse = ", ")
    message(paste("Columns: (", 
                  badString, 
                  ") have all missing data and are not shown."))
  }

  # ----- Create plot ----------------------------------------------------------
  
  gg <- 
    scatterplot(
      data = data, 
      size = size,
      sampleSize = sampleSize, 
      sampleFraction = sampleFraction,
      shape = shape, 
      color = color, 
      alpha = alpha
    )
  
  # ----- Return ---------------------------------------------------------------
  
  options(warn=0)

  return(gg)
  
}

