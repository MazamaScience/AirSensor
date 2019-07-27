#' @export
#' @importFrom rlang .data
#' @import graphics
#' 
#' @title Detect and replace time series outliers
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param windowSize Integer window size for outlier detection.
#' @param thresholdMin Threshold value for outlier detection.
#' @param replace Logical specifying whether replace outliers with the window
#'   median value.
#' @param showPlot Logical specifying whether to generate outlier detection 
#'   plots.
#' @param data_shape Symbol to use for data points.
#' @param data_size Size of data points.
#' @param data_color Color of data points.
#' @param data_alpha Opacity of data points.
#' @param outlier_shape Symbol to use for outlier points.
#' @param outlier_size Size of outlier points.
#' @param outlier_color Color of outlier points.
#' @param outlier_alpha Opacity of outlier points.
#' 
#' @return A \emph{pat} object with outliers replaced by median values.
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
#' which a median is calculated. Each PurpleAir channel makes a measurement
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
#' 
#' @examples
#' \donttest{
#' example_pat %>%
#'   pat_filterDate(20180801, 20180815) %>%
#'   pat_outliers(replace = TRUE, showPlot = TRUE)
#' }

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
    
    windowSize <- 23
    thresholdMin <- 8
    replace <- FALSE
    showPlot <- TRUE
    # data_shape <- 18 
    # data_size <- 1
    # data_color <- "black"
    # data_alpha <- 0.5
    # outlier_shape <- 8 
    # outlier_size <- 1
    # outlier_color <- "red"
    # outlier_alpha <- 1.0
     
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
  # NOTE:  We filter here to separate the A data from the B data and avoid this 
  # NOTE:  problem. But we retain the omitted records for merging back later.
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
    .flagOutliers(
      A_data,
      parameter = "pm25_A", 
      windowSize = windowSize, 
      thresholdMin = thresholdMin
    )
  
  B_flagged <- 
    .flagOutliers(
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
      .replaceOutliers(
        A_data, 
        parameter = "pm25_A"
      )[["pm25_A"]]
    
    B_fixed <- 
      .replaceOutliers(
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
   
    chA <- .plotOutliers(
      df = A_flagged, 
      ylim = ylim, 
      subtitle = pat$meta$label,
      data_shape = data_shape, 
      data_size = data_size, 
      data_color = data_color,
      data_alpha = data_alpha,
      outlier_shape = outlier_shape, 
      outlier_size = outlier_size, 
      outlier_color = outlier_color,
      outlier_alpha = outlier_alpha
      )
    chB <- .plotOutliers(
      df = B_flagged, 
      ylim = ylim,
      subtitle = pat$meta$label,
      data_shape = data_shape, 
      data_size = data_size, 
      data_color = data_color,
      data_alpha = data_alpha,
      outlier_shape = outlier_shape, 
      outlier_size = outlier_size, 
      outlier_color = outlier_color,
      outlier_alpha = outlier_alpha
      )
    
    multi_ggplot(plotList = list(chA, chB))
     
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
