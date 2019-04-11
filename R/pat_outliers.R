#' @export
#' @importFrom rlang .data
#' @import graphics
#' 
#' @title Detect and replace timeseries outliers
#' 
#' @param pat Purple Air Timeseries "pat" object
#' @param n integer window size
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
#' The default setting of the window size \code{n = 23} means that 23 samples
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
  pat,
  n = 23,
  thresholdMin = 8,
  replace = FALSE,
  showPlot = TRUE,
  data_shape = 18, 
  data_size = 1, 
  data_color = "black",
  data_alpha = 0.1,
  outlier_shape = 8, 
  outlier_size = 1, 
  outlier_color = "red",
  outlier_alpha = 1.0
) {
  
  # ----- Prepare separate A/B subsets -----------------------------------------
  
  # NOTE:  Outlier detection doesn't work when there are lots of missing values.
  # NOTE:  The 'pat' object combines data from both channels which are on separate
  # NOTE:  time axes. The result is a dataframe that has lots of missing values.
  # NOTE:  We filter here to separate the A data from the B data and avoid this problem.
  # NOTE:
  # NOTE:  We keep most columns in A_data and only pm25_B and datetime_B in B_data
  # NOTE:  so that we can dplyr::left_join() them together at the end.
  
  A_data <- 
    filter(pat$data, !is.na(.data$pm25_A)) %>% 
    select( -.data$pm25_B, -.data$datetime_B)
  B_data <- 
    filter(pat$data, !is.na(.data$pm25_B)) %>% 
    select(.data$datetime, .data$pm25_B, .data$datetime_B)
  
  # Find outliers 
  A_outlierIndices <- 
    seismicRoll::findOutliers(A_data$pm25_A, 
                              n = n, 
                              thresholdMin = thresholdMin)
  B_outlierIndices <- 
    seismicRoll::findOutliers(B_data$pm25_B, 
                              n = n, 
                              thresholdMin = thresholdMin)
  
  # Create median-fixed replacement values
  A_fixed <- A_data$pm25_A
  if ( replace ) {
    A_fixed[A_outlierIndices] <- 
      seismicRoll::roll_median(A_data$pm25_A, n)[A_outlierIndices]
  } else {
    A_fixed[A_outlierIndices] <- NA
  }
  
  B_fixed <- B_data$pm25_B
  if ( replace ) {
    B_fixed[B_outlierIndices] <- 
      seismicRoll::roll_median(B_data$pm25_B, n)[B_outlierIndices]
  } else {
    B_fixed[B_outlierIndices] <- NA
  }
  
  # ----- Plot the data --------------------------------------------------------
  
  if ( showPlot ) {
    
    # Use the same y limits for both plots
    ylim <- range(c(A_data$pm25_A, B_data$pm25_B), na.rm = TRUE)
      
    A_outliers <- A_data[A_outlierIndices,] %>%       
      ggplot2::geom_point(mapping = ggplot2::aes(x = .data$datetime, 
                                                 y = .data$pm25_A), 
                          shape = outlier_shape, 
                          size = outlier_size, 
                          color = outlier_color,
                          alpha = outlier_alpha)
    
    B_outliers <- B_data[B_outlierIndices,] %>% 
      ggplot2::geom_point(mapping = ggplot2::aes(x = .data$datetime, 
                                                 y = .data$pm25_B), 
                          shape = outlier_shape, 
                          size = outlier_size, 
                          color = outlier_color,
                          alpha = outlier_alpha)
    
    channelA <- 
      A_data %>%
      tibble(datetime = A_data$datetime, pm25_A = A_data$pm25_A) %>% 
      ggplot2::ggplot(ggplot2::aes(x = .data$datetime, y = .data$pm25_A)) + 
      ggplot2::geom_point(shape = data_shape,
                          size = data_size,
                          color = data_color,
                          alpha = data_alpha) + 
      ggplot2::ylim(ylim) +
      ggplot2::ggtitle(expression("Channel A PM"[2.5])) + 
      ggplot2::xlab("Date") + ggplot2::ylab("\u03bcg / m\u00b3") + 
      A_outliers
    
    channelB <- 
      B_data %>%
      tibble(datetime = B_data$datetime, pm25_B = B_data$pm25_B) %>% 
      ggplot2::ggplot(ggplot2::aes(x = .data$datetime, y = .data$pm25_B)) + 
      ggplot2::geom_point(shape = data_shape,
                          size = data_size,
                          color = data_color,
                          alpha = data_alpha) + 
      ggplot2::ylim(ylim) +
      ggplot2::ggtitle(expression("Channel B PM"[2.5])) + 
      ggplot2::xlab("Date") + ggplot2::ylab("\u03bcg / m\u00b3") + 
      B_outliers
    
    pat_multiplot(plotlist=list(channelA, channelB))
    
  }
  
  # ----- Create a fixed 'pat' object ------------------------------------------
  
  A_data$pm25_A <- A_fixed
  B_data$pm25_B <- B_fixed
  
  # Combine dataframes 
  data <- dplyr::full_join(A_data, B_data, by = 'datetime') %>%
    dplyr::select(.data$datetime, .data$pm25_A, .data$pm25_B, .data$temperature, 
                  .data$humidity, .data$uptime, .data$adc0, .data$rssi, 
                  .data$datetime_A, .data$datetime_B) %>%
    dplyr::arrange(.data$datetime)
  
  # ----- Create the Purple Air Timeseries (pat) object ------------------------
  
  # Combine meta and data dataframes into a list
  pat <- list(meta = pat$meta, data = data)
  class(pat) <- c("pa_timeseries", class(pat))
  
  return(invisible(pat))
  
}
