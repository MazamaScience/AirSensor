#' @export
#' @importFrom rlang .data
#' @import graphics
#' 
#' @title Sample PurpleAir time series data
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param sampleSize Non-negative integer giving the number of rows to choose.
#' @param sampleFraction Fraction of rows to choose.
#' @param setSeed Integer that sets random number generation. Can be used to 
#'   reproduce sampling.
#' @param keepOutliers logical specifying a graphics focused sampling algorithm
#'   (see Details).
#'
#' @return A subset of the given \emph{pat} object.
#' 
#' @description A sampling function that accepts PurpleAir timeseries dataframes
#' and reduces them by randomly selecting distinct rows of the users choosen 
#' size. 
#' 
#' If both `sampleSize` and `sampleFraction` are unspecified,
#'  `sampleSize = 5000` will be used.
#' 
#' @details When `keepOutliers = FALSE`, random sampling is used to provide a
#' statistically relevant subsample of the data.
#' 
#' When `keepOutliers = TRUE`, a customized sampling algorithm is used that
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
#' pat <- pat_load("SCNP_20", 20190411, 20190521)
#' subset <- pat_sample(pat, sampleSize=1000, setSeed=1)
#' }
#' 

pat_sample <- function(
  pat = NULL,
  sampleSize = NULL, 
  sampleFraction = NULL, 
  setSeed = NULL,
  keepOutliers = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  if ( is.null(sampleSize) && is.null(sampleFraction) )
    sampleSize <- 5000
  
  if ( sampleSize > nrow(pat$data) ) 
    return(pat)
  
  # ----- Detect Outliers ------------------------------------------------------
  
  A_data <- 
    dplyr::filter(pat$data, !is.na(.data$pm25_A)) %>% 
    dplyr::select( -.data$pm25_B, -.data$datetime_B)
  
  B_data <- 
    dplyr::filter(pat$data, !is.na(.data$pm25_B)) %>% 
    dplyr::select(.data$datetime, .data$pm25_B, .data$datetime_B)
  
  
  if ( keepOutliers == TRUE ) {
    
    # Find outliers 
    outlierIndex_A <- 
      which(
        .flagOutliers(
          df = A_data, 
          parameter = "pm25_A",
          windowSize = 23,
          thresholdMin = 8
        )[,ncol(A_data) + 1]
      )
    
    outlierIndex_B <- 
      which(
        .flagOutliers(
          df = B_data, 
          parameter = "pm25_B",
          windowSize = 23,
          thresholdMin = 8
        )[,ncol(B_data) + 1]
      )
    
    # Can't have an index of zero
    if ( length(outlierIndex_A) == 0 ) outlierIndex_A <- c(1)
    if ( length(outlierIndex_B) == 0 ) outlierIndex_B <- c(1)
    
  } else { # keepOutliers == FALSE
    
    # Cheap hack to avoid rewriting too much code
    outlierIndex_A <- c(1)
    outlierIndex_B <- c(1)
    
  }
  
  A_outlierData <- A_data[outlierIndex_A,]
  B_outlierData <- B_data[outlierIndex_B,]
  
  if ( !is.null(setSeed) ) {
    set.seed(setSeed) 
  }
  
  # ----- Remove outlier data -> Sample data -> Reinsert outlier data ->
  
  if ( !is.null(sampleSize) && is.null(sampleFraction) ) {
    
    A_data <- 
      A_data[-outlierIndex_A,] %>% 
      .sample(
        sampleSize = (sampleSize - length(outlierIndex_A) + 
                        length(outlierIndex_B)) / 2
      ) %>% 
      dplyr::bind_rows(A_outlierData) 
    
    B_data <- 
      B_data[-outlierIndex_B,] %>% 
      .sample(
        sampleSize = (sampleSize - length(outlierIndex_B) + 
                        length(outlierIndex_A)) / 2
      ) %>% 
      dplyr::bind_rows(B_outlierData)
    
  } else if ( is.null(sampleSize) && !is.null(sampleFraction) ) {
    
    A_data <- 
      A_data[-outlierIndex_A,] %>% 
      .sample(
        sampleFraction = sampleFraction / 2
      ) %>% 
      dplyr::bind_rows(A_outlierData)
    
    B_data <- 
      B_data[-outlierIndex_B,] %>% 
      .sample(
        sampleFraction = sampleFraction / 2
        ) %>% 
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
  
  # ----- Return ---------------------------------------------------------------
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  return(invisible(pat))
  
}
