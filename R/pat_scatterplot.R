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
#' \item{\code{acd0} -- analog input voltage}
#' \item{\code{rssi} -- wifi signal strength (dBm)}
#' }
#' 
#' @param pat Purple Air Timeseries \emph{pat} object
#' @param parameters vector of parameters to include
#' 
#' @return Tibble portion of the \emph{pat} object, subset.
#' 

pat_scatterplot <- function(
  pat,
  parameters = c('datetime', 'pm25_A', 'pm25_B', 'temperature', 'humidity')
) {
  
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
    badString <- paste(badColumns, collapse=", ")
    message(paste("Columns: (", 
                  badString, 
                  ") have all missing data and are not shown."))
  }
  
  scatterPlot <- 
    GGally::ggpairs( data,
                     mapping = ggplot2::aes(alpha=0.15),
                     lower = list(
                       continuous = GGally::wrap(
                         "points", alpha = 0.3, size=0.5, shape=15)),
                     diag = list(
                       continuous = GGally::wrap(
                         "densityDiag", alpha = 0.5, size=0.5)), 
                     upper = list(continuous = "cor") ) + 
    ggplot2::theme_bw()
  
  return(scatterPlot)
  
}

