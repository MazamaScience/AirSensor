#' @export
#' @import graphics
#' 
#' @title Compare all variables in a Purple Air Timeseries object
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
#' @param pat Purple Air Timeseries \emph{pat} object
#' @param parameters vector of parameters to include
#' @param sampleSize Either an integer or fraction to determine sample size
#' @param shape symbol to use for points
#' @param size size of points
#' @param color color of points
#' @param alpha opacity of points
#' 
#' @return Tibble portion of the \emph{pat} object, subset.
#' 

pat_scatterplot <- function(
  pat = NULL,
  parameters = c('datetime', 'pm25_A', 'pm25_B', 'temperature', 'humidity'),
  sampleSize = 5000,
  size = 0.5,
  shape = 15,
  color = "black",
  alpha = 0.25
) {
  
  # Validate parameters -----------------------------------------------------
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  # ----- Reduce large datasets by sampling ------------------------------------
  
  if ( !is.null(sampleSize) ) { 
    
    if ( sampleSize > 1 ) {
      pat <- 
        pat %>% 
        pat_sample(sampleSize = sampleSize, forGraphics = TRUE)
    } else {
      pat <- 
        pat %>% 
        pat_sample(sampleFraction = sampleSize, forGraphics = TRUE)
    }
    
  }
  
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
  
  scatterPlot <- 
    scatterPlot(
      data = data, 
      size = size, 
      shape = shape, color = 
        color, 
      alpha = alpha
    ) 
  
  return(scatterPlot)
  
}

