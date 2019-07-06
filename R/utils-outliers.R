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

.flagOutliers <- 
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

.replaceOutliers <- 
  function(
    df,
    parameter = NULL, 
    medWin = 7,
    ...
  ) { 
    
    df <- .flagOutliers(
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


#' @title Plot flagged outliers
#' 
#' @description an internal capability to quickly plot outlier data. 
#'
#' @param df data frame that contains datetime, PM2.5, and outlier boolean flags
#' @param parameter the parameter with the associated outliers flag
#' @param ylim y limits for consistency 
#' @param xlab x axis label
#' @param ylab y axis label
#' @param title plot title
#' @param subtitle plot substitle
#' @param data_shape shape of points 
#' @param data_size size of points
#' @param data_color color of points
#' @param data_alpha alpha of points
#' @param outlier_shape outlier point shape
#' @param outlier_size outlier point size
#' @param outlier_color outlier point color
#' @param outlier_alpha outlier point alpha
#'
#' @return gg object
#' 
.plotOutliers <- 
  function(
    df, 
    parameter = "pm25",
    ylim = NULL,
    xlab = "Date", 
    ylab = "\u03bcg / m\u00b3", 
    title = "default", 
    subtitle = NULL,
    data_shape = 18, 
    data_size = 1, 
    data_color = "black",
    data_alpha = 0.5,
    outlier_shape = 8, 
    outlier_size = 1, 
    outlier_color = "red",
    outlier_alpha = 1.0
  ) {
    
    col_flag <- names(df)[which(stringr::str_detect(names(df), "flag_"))]
    col_param <- names(df)[which(stringr::str_detect(names(df), parameter))][1]
    
    df_ind <- which(df[[col_flag]])
        
  
  df_outliers <-   
    ggplot2::geom_point(
      data =  df[df_ind,],
      mapping = ggplot2::aes(x = .data$datetime, y = .data[[col_param]]), 
      shape = outlier_shape, 
      size = outlier_size, 
      color = outlier_color,
      alpha = outlier_alpha
    )
  
  df_plot <- 
    ggplot2::ggplot(
      data = df,
      mapping = ggplot2::aes(x = .data$datetime, y = .data[[col_param]])) + 
    ggplot2::geom_point(
      shape = data_shape,
      size = data_size,
      color = data_color,
      alpha = data_alpha
    ) + 
    ggplot2::ylim(ylim) +
    ggplot2::labs(
      x = xlab, 
      y = ylab, 
      title = 
        ifelse(
          title == "default", 
          paste0(
            "Channel ",
            ifelse(stringr::str_detect(col_param, "A") ,"A ", "B "),
            "PM2.5"
          ), 
          title
        ), 
      subtitle = subtitle
    ) + 
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11),
      plot.subtitle = ggplot2::element_text(size = 8),
    ) + 
    df_outliers
  
  return(df_plot) # No sampling will occur
  
}
