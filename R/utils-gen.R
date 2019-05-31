#' @export
#' 
#' @title General table row sampling
#' 
#' @description This is a wrapper around sample() to make it easy to select 
#' random rows from a table. Supports both integer (sampleSize) and fractional 
#' (sampleFraction) N row sampling. For reproducible debugging, set a hash seed.
#' 
#' @param data
#' @param sampleSize
#' @param sampleFraction
#' @param weight
#' @param setSeed 
#' 
#' @return A data.frame
#' 

sample <- 
  function(
    data,
    sampleSize = NULL,
    sampleFraction = NULL, 
    weight = NULL,
    setSeed = NULL
  ) { 
    
    if (!is.null(setSeed) )( set.seed(setSeed) ) 
    
    if ( !is.null(sampleSize) && 
         sampleSize <= nrow(data) 
    ) { 
      
      sz <- sampleSize
      
    } 
    
    if ( !is.null(sampleFraction) &&
         sampleFraction <= 1 && 
         sampleFraction > 0 
    ) { 
      
      sz <- nrow(data) * sampleFraction
      
    }
    
    # ----- Fastest possible way to sample -------------------------------------
    subset <- 
      data[
        .Internal(
          sample(
            x = nrow(data), 
            size = sz, 
            replace = FALSE, 
            prob = weight
          )
        ),]
    
    return(subset)
    
  }

#' @export
#' 
#' @title Flag outliers in vectorized data
#' 
#' @description Outlier detection using Hampel identification.  For each sample 
#'  of a vector, the function computes the median of a center-aligned window composed of the sample 
#'  and its \code{windowSize} surrounding samples. It also estimates the 
#'  standard deviation of each sample about its window median using the median 
#'  absolute deviation. If a sample differs from the median by more than three 
#'  standard deviations, it is replaced with the median. If x is a matrix, then 
#'  hampel treats each column of x as an independent channel.
#'  
#'  The \code{thresholdMin} level is similar to a sigma value for normally 
#'  distributed data. 
#' 
#' @param df A data frame.
#' @param parameter The data frame parameter to check and replace outliers
#' @param windowSize The size of the rolling window
#' @param thresholdMin The minimum threshold value to detect outliers
#' 
#' @return \code{data.frame} A data.frame with an additional flag vector
#' 

flagOutliers <- 
  function(
    df, 
    parameter = NULL,
    windowSize = 23,
    thresholdMin = 8
  ) {
    
    if ( is.null(parameter) ) 
      stop("Missing parameter")
    
    data <- df[[parameter]]
    flag <- paste0("flag_", parameter)
    
    # Index outliers
    outlierInd <- 
      seismicRoll::findOutliers(
        x = data, 
        n = windowSize,
        thresholdMin = thresholdMin
      )
    
    # Make a new logical column with name: parameter_flag
    
    df[[flag]] <- FALSE 
    
    df[[flag]][outlierInd] <- TRUE
    
    return(df)
    
  }

#' @export
#' 
#' @title Replace outliers with rolling median
#' 
#' @description Perform smoothing by replacing outliers with the window median 
#' value. Using this technique, you can create an highly smoothed, artificial 
#' dataset by setting \code{thresholdMin = 1} or lower (but always above zero).
#' 
#' @param df A data frame.
#' @param parameter The data frame parameter to check and replace outliers
#' @param medWin The size of the rolling median window
#' @param ... Parameters to extend \code{flagOutliers}
#' 
#' @return a \code{data.frame} with replaced outliers
#' 

replaceOutliers <- 
  function(
    df,
    parameter = NULL, 
    medWin = 7,
    ...
  ) { 
    
    df <- flagOutliers(
      df = df, 
      parameter = parameter,
      ...
    )
    
    if ( is.null(parameter) ) 
      stop("Missing parameter")
    
    flag <- paste0("flag_", parameter)
    
    roll_med <- function(x, n) seismicRoll::roll_median(x, n)
    
    flagged <- which(df[[flag]])
    
    df[[flag]] = NULL
    
    df[[parameter]][flagged] <- 
      roll_med(df[[parameter]], medWin)[flagged]
    
    return(df)
    
  }
