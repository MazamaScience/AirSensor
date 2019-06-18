#' @export
#' @importFrom rlang .data
#' @import graphics
#' 
#' @title Detect and replace timeseries outliers
#' 
#' @param pat Purple Air Timeseries "pat" object
#' @param windowSize integer window size for outlier detection
#' @param thresholdMin threshold value for outlier detection
#' @param replace logical specifying whether replace outliers with the window 
#' median value
#' @param showPlot logical specifying whether to generate outlier detection 
#' plots
#' @param data_shape symbol to use for data points
#' @param data_size size of data points
#' @param data_color color of data points
#' @param data_alpha opacity of data points
#' @param outlier_shape symbol to use for outlier points
#' @param outlier_size size of outlier points
#' @param outlier_color color of outlier points
#' @param outlier_alpha opacity of outlier points
#' 
#' @return \code{pat} timeseries object with outliers replaced by median values.
#' 
#' @description Outlier detection using a Median Average Deviation "Hampel" 
#' filter. This function applies a rolling Hampel filter to find those points
#' that are very far out in the tails of the distribution of values within the
#' window.
#' 
#' The \code{thresholdMin} level is similar to a sigma value for normally 
#' distributed data. The default threshold setting \code{thresholdMin = 8} 
#' identifies points that are extremely unlikely to be part of a normal 
#' distribution and therefore very likely to be an outlier. By choosing a 
#' relatively large value for `thresholdMin`` we make it less likely that we 
#' will generate false positives.
#' 
#' The default setting of the window size \code{windowSize = 23} means that 23 samples
#' from a single channel are used to determine the distribution of values for
#' which a median is calculated. Each Purple Air channel makes a measurement
#' approximately every 80 seconds so the temporal window is 23 * 80 sec or
#' approximately 30 minutes. This seems like a reasonable period of time over
#' which to evaluate PM2.5 measurements.
#' 
#' Specifying \code{replace = TRUE} allows you to perform smoothing by 
#' replacing outliers with the window median value. Using this technique, you 
#' can create an highly smoothed, artificial dataset by setting 
#' \code{thresholdMin = 1} or lower (but always above zero).
#' 
#' @note Additional documentation on the algorithm is available in 
#' \code{seismicRoll::findOutliers()}.

pat_outliers <- function(
  pat = NULL,
  windowSize = 23,
  thresholdMin = 8,
  replace = FALSE,
  showPlot = TRUE,
  data_shape = 18, 
  data_size = 1, 
  data_color = "black",
  data_alpha = 0.5,
  outlier_shape = 8, 
  outlier_size = 1, 
  outlier_color = "red",
  outlier_alpha = 1.0
) {
  
  # ===== DEBUGGING ============================================================
  
  if ( FALSE ) {
    
    windowSize = 23
    thresholdMin = 8
    replace = FALSE
    showPlot = TRUE
    data_shape = 18 
    data_size = 1
    data_color = "black"
    data_alpha = 0.5
    outlier_shape = 8 
    outlier_size = 1
    outlier_color = "red"
    outlier_alpha = 1.0
    
  }
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  # ----- Prepare separate A/B subsets -----------------------------------------
  
  # NOTE:  Outlier detection doesn't work when there are lots of missing values.
  # NOTE:  The 'pat' object combines data from both channels which are on separate
  # NOTE:  time axes. The result is a dataframe that has lots of missing values.
  # NOTE:  We filter here to separate the A data from the B data and avoid this problem.
  # NOTE:  But we retain the omitted records for merging back later.
  # NOTE:
  # NOTE:  We keep most columns in A_data and only pm25_B and datetime_B in B_data
  # NOTE:  so that we can dplyr::left_join() them together at the end.
  
  A_data <- 
    dplyr::filter(pat$data, !is.na(.data$pm25_A)) %>% 
    dplyr::select( -.data$pm25_B, -.data$datetime_B)
  B_data <- 
    dplyr::filter(pat$data, !is.na(.data$pm25_B)) %>% 
    dplyr::select(.data$datetime, .data$pm25_B, .data$datetime_B)
  
  A_missing <- 
    dplyr::filter(pat$data, is.na(.data$pm25_A)) %>% 
    dplyr::select( -.data$pm25_B, -.data$datetime_B)
  B_missing <- 
    dplyr::filter(pat$data, is.na(.data$pm25_B)) %>% 
    dplyr::select(.data$datetime, .data$pm25_B, .data$datetime_B)
  
  # Flag outliers 
  A_flagged <- 
    flagOutliers(
      A_data,
      parameter = "pm25_A", 
      windowSize = windowSize, 
      thresholdMin = thresholdMin
    )
  
  B_flagged <- 
    flagOutliers(
      B_data,
      parameter = "pm25_B", 
      windowSize = windowSize, 
      thresholdMin = thresholdMin
    )
  
  A_outlierIndices <- 
    which(A_flagged[,ncol(A_flagged)])
  
  B_outlierIndices <- 
    which(B_flagged[,ncol(B_flagged)])
  
  # Create median-fixed replacement values
  if ( replace ) {
    
    A_fixed <- 
      replaceOutliers(
        A_data, 
        parameter = "pm25_A"
      )[["pm25_A"]]
    
    B_fixed <- 
      replaceOutliers(
        B_data, 
        parameter = "pm25_B"
      )[["pm25_B"]]
    
  } else { 
    
    A_fixed <- A_data$pm25_A
    B_fixed <- B_data$pm25_B
    
    A_fixed[A_outlierIndices] <- NA
    B_fixed[B_outlierIndices] <- NA
    
  }
  
  # ----- Plot the data --------------------------------------------------------
  
  if ( showPlot ) {
    
    # Use the same y limits for both plots
    ylim <- range(c(A_data$pm25_A, B_data$pm25_B), na.rm = TRUE)
    ylim[1] <- min(0, ylim[1]) # always zero unless ylim[1] is neg (possible???)
    
    A_outliers <- A_data[A_outlierIndices,] %>%       
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = .data$datetime, y = .data$pm25_A), 
        shape = outlier_shape, 
        size = outlier_size, 
        color = outlier_color,
        alpha = outlier_alpha
      )
    
    B_outliers <- B_data[B_outlierIndices,] %>% 
      ggplot2::geom_point(
        mapping = ggplot2::aes(x = .data$datetime, 
                               y = .data$pm25_B), 
        shape = outlier_shape, 
        size = outlier_size, 
        color = outlier_color,
        alpha = outlier_alpha
      )
    
    channelA <- 
      A_data %>%
      dplyr::tibble(datetime = A_data$datetime, pm25_A = A_data$pm25_A) %>% 
      ggplot2::ggplot(ggplot2::aes(x = .data$datetime, y = .data$pm25_A)) + 
      ggplot2::geom_point(
        shape = data_shape,
        size = data_size,
        color = data_color,
        alpha = data_alpha
      ) + 
      ggplot2::ylim(ylim) +
      ggplot2::labs(
        x = "Date", 
        y = "\u03bcg / m\u00b3", 
        title = expression("Channel A PM"[2.5]), 
        subtitle = pat$meta$label
      ) + 
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 11),
        plot.subtitle = ggplot2::element_text(size = 8),
      ) + 
      A_outliers
    
    channelB <- 
      B_data %>%
      dplyr::tibble(datetime = B_data$datetime, pm25_B = B_data$pm25_B) %>% 
      ggplot2::ggplot(ggplot2::aes(x = .data$datetime, y = .data$pm25_B)) + 
      ggplot2::geom_point(
        shape = data_shape,
        size = data_size,
        color = data_color,
        alpha = data_alpha
      ) + 
      ggplot2::ylim(ylim) +
      ggplot2::labs(
        x = "Date", 
        y = "\u03bcg / m\u00b3", 
        title = expression("Channel B PM"[2.5]), 
        subtitle = pat$meta$label
      ) + 
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 11),
        plot.subtitle = ggplot2::element_text(size = 8),
      ) + 
      B_outliers
    
    multi_ggplot(plotList = list(channelA, channelB)) # No sampling will occur
    
  }
  
  # ----- Create a fixed 'data' dataframe --------------------------------------
  
  A_data$pm25_A <- A_fixed
  B_data$pm25_B <- B_fixed
  
  # Add back records with missing values.
  # Save time -- don't arrange by datetime yet.
  A_full <- dplyr::bind_rows(A_data, A_missing)
  B_full <- dplyr::bind_rows(B_data, B_missing)
  
  # Combine dataframes 
  data <- 
    dplyr::full_join(A_full, B_full, by = 'datetime') %>% 
    dplyr::arrange(.data$datetime)
  
  data <- data[,c("datetime",    "pm25_A",      "pm25_B",     
                  "temperature", "humidity",    "uptime",     
                  "adc0",        "rssi",        "datetime_A", 
                  "datetime_B")]
  
    

  # ----- Create the Purple Air Timeseries (pat) object ------------------------
  
  # Combine meta and data dataframes into a list
  pat <- list(meta = pat$meta, data = data)
  class(pat) <- c("pa_timeseries", class(pat))
  
  return(invisible(pat))
  
}
