#' @export
#' @import graphics
#' @title Detect and Fix Timeseries Outliers
#' @param pat Purple Air Timeseries "pat" object from \code{createPATimeseriesObject()}
#' @param n integer window size
#' @param thresholdMin threshold value for outlier detection
#' @param replaceOutliers logical specifying whether replace outlier with the window median value
#' @param showPlot logical specifying whether to generate outlier detection plots
#' @return \code{pat} timeseries object with outliers replaced by median values.
#' @description Outlier detection using a Median Average Deviation "Hampel" filter.
#' 
#' The \code{thresholdMin} level is similar to a sigma value for normally distributed data.
#' Hampel filter values above 6 indicate a data value that is extremely unlikely to
#' be part of a normal distribution (~ 1/500 million) and therefore very likely to
#' be an outlier. By choosing a relatively large value for thresholdMin we make it
#' less likely that we will generate false positives.
#' 
#' Specifying \code{replaceOutliers = TRUE} allows you to perform smoothing by 
#' replacing outliers with the window median value. Using this technique, you can
#' create an highly smoothed, artificial dataset by setting \code{thresholdMin = 1}
#' or lower (but always above zero).
#' 
#' @note Additional documentation on the algorithm is available in 
#' \code{seismicRoll::findOutliers()}.

pat_findOutliers <- function(pat,
                             n = 11,
                             thresholdMin = 6,
                             replaceOutliers = FALSE,
                             showPlot = TRUE) {
  
  # ----- Prepare separate A/B subsets ----------------------------------------
  
  # NOTE:  Outlier detection doesn't work when there are lots of missing values.
  # NOTE:  The 'pat' object combines data from both channels which are on separate
  # NOTE:  time axes. The result is a dataframe that has lots of missing values.
  # NOTE:  We filter here to separate the A data from the B data and avoid this problem.
  # NOTE:
  # NOTE:  We keep most columns in A_data and only pm25_B and datetime_B in B_data
  # NOTE:  so that we can dplyr::left_join() them together at the end.
  
  A_data <- 
    filter(pat$data, !is.na(pm25_A)) %>% 
    select( -pm25_B, -datetime_B)
  B_data <- 
    filter(pat$data, !is.na(pm25_B)) %>% 
    select(datetime, pm25_B, datetime_B)
  
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
  if ( replaceOutliers ) {
    A_fixed[A_outlierIndices] <- 
      seismicRoll::roll_median(A_data$pm25_A, n)[A_outlierIndices]
  } else {
    A_fixed[A_outlierIndices] <- NA
  }
  
  B_fixed <- B_data$pm25_B
  if ( replaceOutliers ) {
    B_fixed[B_outlierIndices] <- 
      seismicRoll::roll_median(B_data$pm25_B, n)[B_outlierIndices]
  } else {
    B_fixed[B_outlierIndices] <- NA
  }
  
  # ----- Plot the data -------------------------------------------------------
  
  if ( showPlot ) {
    
    # Plot the data in two separate plots
    layout(matrix(seq(2)))
    
    plot(A_data$datetime, A_data$pm25_A, col='transparent',
         xlab = 'Date', ylab = "PM 2.5 (\U00B5g/m3)",
         las=1)
    
    title('channel A outliers')
    mtext(pat$meta$label, 3, 0)
    
    points(A_data$datetime[A_outlierIndices], A_data$pm25_A[A_outlierIndices],
           pch=16, cex=1.0, col='gray70') 
    
    points(A_data$datetime[A_outlierIndices], A_data$pm25_A[A_outlierIndices],
           pch=1, cex=1.0, col='red')
    
    points(A_data$datetime, A_fixed,
           pch=16, cex=0.5, col='black')
    
    # ---------------------------------
    
    plot(B_data$datetime, B_data$pm25_B, col='transparent',
         xlab = 'Date', ylab = "PM 2.5 (\U00B5g/m3)",
         las=1)
    
    title('channel B outliers')
    mtext(pat$meta$label, 3, 0)
    
    points(B_data$datetime[B_outlierIndices], B_data$pm25_B[B_outlierIndices],
           pch=16, cex=1.0, col='gray70') 
    
    points(B_data$datetime[B_outlierIndices], B_data$pm25_B[B_outlierIndices],
           pch=1, cex=1.0, col='red')
    
    points(B_data$datetime, B_fixed,
           pch=16, cex=0.5, col='black')
    
    layout(1)
    
  }
  
  # ----- Create a fixed 'pat' object -----------------------------------------
  
  A_data$pm25_A <- A_fixed
  B_data$pm25_B <- B_fixed
  
  # Combine dataframes 
  data <- dplyr::full_join(A_data, B_data, by = 'datetime') %>%
    dplyr::select(datetime, pm25_A, pm25_B, temperature, 
                  humidity, uptime, adc0, rssi, 
                  datetime_A, datetime_B) %>%
    dplyr::arrange(datetime)
  
  # ----- Create the Purple Air Timeseries (pat) object -----------------------
  
  # Combine meta and data dataframes into a list
  pat <- list(meta = pat$meta, data = data)
  class(pat) <- c("pa_timeseries", class(pat))
  
  return(invisible(pat))
  
}
