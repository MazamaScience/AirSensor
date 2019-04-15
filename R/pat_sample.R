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
# TODO: Finish implementing weighted values. PM2.5 values should be uniformly 
#       weighted, and outliers should have a greater weight. 
# TODO: Remove duplicates! Even if the probability is slim

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
  
  outliers <- 
    pat %>% 
    pat_outliers(n = 11, thresholdMin = 4, showPlot = FALSE)
  
  if ( !is.null(setSeed) )( set.seed(setSeed) )
  
  if ( !is.null(sampleSize) && is.null(sampleFraction) ) {
    
    sampled_outliers <- 
      outliers$data %>%
      dplyr::sample_n(size=sampleSize, replace=FALSE, weight=weight)
    
    pat$data <- 
      dplyr::bind_rows(sampled_outliers, pat$data) %>% 
      arrange(.data$datetime) %>%
      dplyr::sample_n(size=sampleSize, replace=FALSE, weight=weight)
      
  } else if ( is.null(sampleSize) && !is.null(sampleFraction) ) {
    
    sampled_outliers <- 
      outliers$data %>%
      dplyr::sample_frac(size=sampleFraction, replace=FALSE, weight=weight)
    
    pat$data <- 
      dplyr::bind_rows(sampled_outliers, pat$data) %>% 
      arrange(.data$datetime) %>%
      dplyr::sample_frac(size=sampleFraction, replace=FALSE, weight=weight)
  
  } else {
    
    stop("Cannot use both fixed number & fractional sampling")
  
  }
  
  return (pat)
  
}
