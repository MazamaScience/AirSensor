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
    
    A_data <- 
      filter(pat$data, !is.na(.data$pm25_A)) %>% 
      select( -.data$pm25_B, -.data$datetime_B)
    
    B_data <- 
      filter(pat$data, !is.na(.data$pm25_B)) %>% 
      select(.data$datetime, .data$pm25_B, .data$datetime_B)
    
    # Find outliers 
    outlierIndex_A <- 
      seismicRoll::findOutliers(
        A_data$pm25_A, 
        n = 23, 
        thresholdMin = 8
      )
    
    outlierIndex_B <- 
      seismicRoll::findOutliers(
        B_data$pm25_B, 
        n = 23, 
        thresholdMin = 8
      )
    
  } else { # forGraphics == FALSE
    
    # Cheap hack to avoid rewriting too much code
    outlierIndex_A <- 1
    outlierIndex_B <- 1
    
  }
  
  A_outlierData <- A_data[outlierIndex_A,]
  B_outlierData <- B_data[outlierIndex_B,]
  
  if ( !is.null(setSeed) )( set.seed(setSeed) )
  
  # ----- Remove outlier data -> Sample data -> Reinsert outlier data ->
  
  if ( !is.null(sampleSize) && is.null(sampleFraction) ) {
    
    A_data <- 
      A_data[-outlierIndex_A,] %>% 
      dplyr::sample_n(
        size = (sampleSize - length(outlierIndex_A)+ length(outlierIndex_B)) /2, 
        replace = FALSE, 
        weight = weight
      ) %>% 
      dplyr::bind_rows(A_outlierData) 
    
    B_data <- 
      B_data[-outlierIndex_B,] %>% 
      dplyr::sample_n(
        size = (sampleSize - length(outlierIndex_B)+ length(outlierIndex_A)) /2, 
        replace = FALSE,
        weight = weight
      ) %>% 
      dplyr::bind_rows(B_outlierData)
    
  } else if ( is.null(sampleSize) && !is.null(sampleFraction) ) {
    
    A_data <- 
      A_data[-outlierIndex_A,] %>% 
      dplyr::sample_frac(
        size = sampleFraction/2, 
        replace = FALSE, 
        weight = weight 
      ) %>% 
      dplyr::bind_rows(A_outlierData)
    
    B_data <- 
      B_data[-outlierIndex_B,] %>% 
      dplyr::sample_frac(
        size = sampleFraction/2, 
        replace = FALSE, 
        weight = weight) %>% 
      dplyr::bind_rows(B_outlierData) 
    
  } else {
    
    stop("Cannot use both fixed number & fractional sampling")
    
  }
  
  data <- 
    dplyr::full_join(
      A_data, 
      B_data, 
      by = 'datetime'
    ) %>%
    dplyr::select(
      .data$datetime, 
      .data$pm25_A, 
      .data$pm25_B, 
      .data$temperature, 
      .data$humidity, 
      .data$uptime, 
      .data$adc0, 
      .data$rssi, 
      .data$datetime_A, 
      .data$datetime_B
    ) %>%
    dplyr::distinct() %>% 
    dplyr::arrange(.data$datetime)
  
  # ----- Create the Purple Air Timeseries (pat) object ------------------------
  
  # Combine meta and data dataframes into a list
  pat <- list(meta = pat$meta, data = data)
  class(pat) <- c("pa_timeseries", class(pat))
  
  return(invisible(pat))
}
