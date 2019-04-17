#' @export
#' @importFrom rlang .data
#' @import graphics
#' 
#' @title Sample PurpleAir time series data
#' 
#' @param pat Purple Air Timeseries "pat" object
#' @param sampleSize a non-negative integer giving the number of rows to choose.
#' @param sampleFraction the fraction of rows to choose.
#' @param weights a vector of probability weights for obtaining the elements of the
#' vector being sampled.
#' @param setSeed an integer that sets random numbver generation. Can be used to 
#' reproduce sampling.

#' @return \code{pat} timeseries object - subset.
#' 
#' @description A sampling function that accepts PurpleAir timeseries dataframes
#' and reduces them by randomly selecting distinct rows of the users choosen 
#' size. 
#' 
#' @examples 
#' \dontrun{
#' pat <- pat_load()
#' subset <- pat_sample(pat, sampleSize=1000, setSeed=1234)
#' }
#' 

pat_sample <- function(
  pat, 
  sampleSize=NULL, 
  sampleFraction=NULL, 
  weight=NULL, 
  setSeed=NULL
  ) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( is.null(pat) ) 
    stop("'pat' must be defined.")
  
  # ----- Detect Outliers ------------------------------------------------------
  
  outlierIndex_A <-
    pat$data$pm25_A %>%
    seismicRoll::findOutliers(n = 11, thresholdMin = 4)
  
  outlierIndex_B <- 
    pat$data$pm25_B %>% 
    seismicRoll::findOutliers(n = 11, thresholdMin = 4)
  
  outlierIndex_AB <- 
    c(outlierIndex_A, outlierIndex_B)
  
  outlierData <- pat$data[outlierIndex_AB,]
  
  if ( !is.null(setSeed) )( set.seed(setSeed) )
  
  # ----- Remove outlier data -> Sample data -> Reinsert outlier data -> Sort
  
  if ( !is.null(sampleSize) && is.null(sampleFraction) ) {
    
    pat$data <- 
      pat$data[-outlierIndex_AB,] %>% 
      dplyr::sample_n(size=sampleSize, replace=FALSE, weight=weight) %>% 
      dplyr::bind_rows(outlierData) %>% 
      dplyr::arrange(.data$datetime)
      
  } else if ( is.null(sampleSize) && !is.null(sampleFraction) ) {
    
    pat$data <- 
      pat$data[-outlierIndex_AB,] %>% 
      dplyr::sample_frac(size=sampleFraction, replace=FALSE, weight=weight) %>% 
      dplyr::bind_rows(outlierData) %>% 
      dplyr::arrange(.data$datetime)
  
  } else {
    
    stop("Cannot use both fixed number & fractional sampling")
  
  }
  
  return (pat)
  
}
