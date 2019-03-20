#' @export
#' @import graphics
#' @title Return a Linear Model Fitting Channel A and BCompare All Variables in a Purple Air Timeseries Object
#' @param pat Purple Air Timeseries "pat" object from \code{createPATimeseriesObject()}
#' @param use_adc0 logical specifying whether include the \code{adc0} parameter
#' @param use_rssi logical specifying whether include the \code{rssi} parameter
#' @param showPlot logical specifying whether to generate internal data plots
#' @return Tibble portion of the \code{pat} object, subset
#' @description Creates a multi-panel comparing all variables in the \code{pat} object.
#' If any variables have no valid data, they are omitted from the plot.
#' 
#' A tibble is returned containing only the requested variables and omitting any
#' that have no valid data.

pat_internalData <- function(pat,
                             use_adc0 = FALSE,
                             use_rssi = TRUE,
                             showPlot = TRUE) {
  
  # For easier access
  meta <- pat$meta
  data <- pat$data
  
  # Determine which columns to show
  includedColumns <- c('datetime','pm25_A','pm25_B','temperature','humidity','uptime')
  if ( use_rssi ) includedColumns <- c(includedColumns,'rssi')
  if ( use_adc0 ) includedColumns <- c(includedColumns,'adc0')
  
  data <- data[,includedColumns]
  
  # Test for presence of data
  columnIsEmpty <- unlist(lapply(data, function(x) { all(is.na(x)) }))
  goodColumns <- names(which(!columnIsEmpty))
  data <- data[,goodColumns]
  
  if ( sum(columnIsEmpty) > 0 ) {
    badColumns <- names(which(columnIsEmpty))
    badString <- paste(badColumns, collapse=", ")
    message(paste("Columns: (", badString, ") have all missing data and are not shown."))
  }
  
  if ( showPlot ) {
    # Use pch='.' to speed up plotting
    plot(data, pch='.')
  }
  
  return(invisible(data))
}

