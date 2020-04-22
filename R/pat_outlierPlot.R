#TODO: Roxygenize.

pat_outlierPlot <- function(
  pat = NULL,
  windowSize = 23,
  thresholdMin = 8,
  data_shape = 18, 
  data_size = 1, 
  data_color = "black",
  data_alpha = 0.5,
  outlier_shape = 8, 
  outlier_size = 1, 
  outlier_color = "red",
  outlier_alpha = 1.0,
  replacement_color = "green",
  replacement_alpha = 1.0
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
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
    
  
  # ----- Plot the data --------------------------------------------------------
  

  # Binding imputed values back to dfs. 
  A_flagged$imputed <-A_fixed
  B_flagged$imputed <- B_fixed
  
  # Set time axis to sensor local time
  timezone <- pat$meta$timezone
  A_flagged$datetime <- lubridate::with_tz(A_flagged$datetime, tzone = timezone)
  B_flagged$datetime <- lubridate::with_tz(B_flagged$datetime, tzone = timezone)
  
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
  
  chA <- 
    chA +
    geom_point(data = filter(A_flagged, flag_outliers_pm25_A),
               aes(y = imputed), color = replacement_color, alpha = replacement_alpha)
  chB <- 
    chB +
    geom_point(data = filter(B_flagged, flag_outliers_pm25_B), 
               aes(y = imputed), color = replacement_color, alpha = replacement_alpha)
  
  multi_ggplot(plotList = list(chA, chB))
}