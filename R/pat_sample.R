#' @export
#' @importFrom rlang .data
#' @import graphics
#' 
#' @title Sample PurpleAir time series data
#' 
#' @param pat Purple Air Timeseries "pat" object
#' @param sampleSize a non-negative integer giving the number of rows to choose.
#' @param sampleFraction the fraction of rows to choose.
#' @param weight a vector of probability weights for obtaining the elements of the
#' vector being sampled.
#' @param setSeed an integer that sets random numbver generation. Can be used to 
#' reproduce sampling.
#' @param forGraphics logical specifying a graphics focused sampling algorithm
#' (see Details)
#''
#' @return \code{pat} timeseries object - subset.
#' 
#' @description A sampling function that accepts PurpleAir timeseries dataframes
#' and reduces them by randomly selecting distinct rows of the users choosen 
#' size. 
#' 
#' If both `sampleSize` and `sampleFraction` are unspecified,
#'  `sampleSize = 5000` will be used.
#' 
#' @details When `forGraphics = FALSE`, random sampling is used to provide a
#' statistically relevant subsample of the data.
#' 
#' When `forGraphics = TRUE`, a customized sampling algorithm is used that
#' attempts to create subsets for use in plotting that create plots that are
#' visually identical to plots using all data. This is accomplished by
#' preserving outliers and only sampling data in regions where overplotting
#' is expected.
#' 
#' The process is as follows:
#' \enumerate{
#' \item{find outliers using `seismicRoll::findOutliers()`}
#' \item{create a subset consisting of only outliers}
#' \item{sample the remaining data}
#' \item{merge the outliers and sampled data}
#' }
#' 
#' @examples 
#' \dontrun{
#' pat <- pat_load()
#' subset <- pat_sample(pat, sampleSize=1000, setSeed=1234)
#' }
#' 

pat_sample <- function(
  pat = NULL,
  sampleSize = NULL, 
  sampleFraction = NULL, 
  weight = NULL, 
  setSeed = NULL,
  forGraphics = FALSE
) {
  
  # Validate parameters --------------------------------------------------------
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  if ( is.null(sampleSize) && is.null(sampleFraction) )
    sampleSize <- 5000
  
  if ( sampleSize > nrow(pat$data) ) 
    return(pat)
  
  # ----- Detect Outliers ------------------------------------------------------

  if ( forGraphics == TRUE ) {
    
    outlierIndex_A <-
      pat$data$pm25_A %>%
      filter(!is.na(.data)) %>% 
      seismicRoll::findOutliers(n = 11, thresholdMin = 4)
    
    outlierIndex_B <- 
      pat$data$pm25_B %>% 
      filter(!is.na(.data)) %>% 
      seismicRoll::findOutliers(n = 11, thresholdMin = 4)
    
    outlierIndex_AB <- 
      c(outlierIndex_A, outlierIndex_B)
    
  } else { # forGraphics == FALSE
    
    # Cheap hack to avoid rewriting too much code
    outlierIndex_AB <- 1
    
  }
  
  outlierData <- pat$data[outlierIndex_AB,]
  
  if ( !is.null(setSeed) ) set.seed(setSeed)
  
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
