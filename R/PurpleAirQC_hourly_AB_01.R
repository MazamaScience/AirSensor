#' @export
#' @importFrom rlang .data
#' 
#' @title Apply hourly aggregation QC using "AB_O1" algorithm
#' 
#' @param pat A PurpleAir timeseries object.
#' @param min_count Aggregation bins with fewer than \code{min_count} measurements
#' will be marked as \code{NA}.
#' @param returnAllColumns Logical specifying whether to return all columns
#' of statistical data generated for QC algorithm or just the final \code{pm25}
#' result.
#'  
#' @description Creates a \code{pm25} timeseries by averaging aggregated data
#' from the A and B channels and applying the following QC logic:
#' 
#' \enumerate{
#' \item{Create pm25 by averaging the A and B channel aggregation means}
#' \item{Invalidate data where:  (min_count < 20)}
#' \item{Invalidate data where:  (p-value < 1e-4) & (mean_diff > 10)}
#' \item{Invalidate data where:  (pm25 < 100) & (mean_diff > 20)}
#' }
#'
#' @note Purple Air II sensors reporting after the June, 2019 firmware
#' upgrade report data every 2 minutes or 30 measurements per hour. The default
#' setting of \code{min_count = 20} is equivalent to a required data recovery
#' rate of 67%.
#' 
#' @return Data frame with columns \code{datetime} and \code{pm25}.
#' 
#' @examples 
#' \donttest{
#' library(AirSensor)
#' 
#' df_00 <- 
#'   example_pat %>%
#'   pat_qc() %>%
#'   PurpleAirQC_hourly_AB_00()
#'   
#' df_01 <- 
#'   example_pat %>%
#'   pat_qc() %>%
#'   PurpleAirQC_hourly_AB_01()
#'   
#' df_02 <- 
#'   example_pat %>%
#'   pat_qc() %>%
#'   PurpleAirQC_hourly_AB_02()
#' 
#' layout(matrix(seq(2)))
#' 
#' plot(df_00, pch = 16, cex = 0.8, col = "red")
#' points(df_01, pch = 16, cex = 0.8, col = "black")
#' title("example_pat_failure_A -- PurpleAirQC_hourly_AB_01")
#' 
#' plot(df_00, pch = 16, cex = 0.8, col = "red")
#' points(df_02, pch = 16, cex = 0.8, col = "black")
#' title("example_pat_failure_A -- PurpleAirQC_hourly_AB_02")
#' 
#' layout(1)
#' }

PurpleAirQC_hourly_AB_01 <- function(
  pat = NULL,
  min_count = 20,
  returnAllColumns = FALSE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  MazamaCoreUtils::stopIfNull(min_count)
  MazamaCoreUtils::stopIfNull(returnAllColumns)
  
  # ----- Prepare aggregated data ----------------------------------------------
  
  # Hourly counts
  countData <- 
    pat %>%
    pat_aggregate( function(x) { base::length(na.omit(x)) } ) %>%
    pat_extractData()

  # Hourly means
  meanData <-
    pat %>%
    pat_aggregate( function(x) { base::mean(x, na.rm = TRUE) } ) %>%
    pat_extractData()

  # Hourly ttest
  # NOTE:  this uses the patData_aggregate function which uses a dataframe
  FUN <- function(x) {
    result <- try({
      hourly_ttest <- stats::t.test(x$pm25_A, x$pm25_B, paired = FALSE)
      tbl <- dplyr::tibble(
        t_score = as.numeric(hourly_ttest$statistic),
        p_value = as.numeric(hourly_ttest$p.value),
        df_value = as.numeric(hourly_ttest$parameter)
      )
    }, silent = TRUE)
    if ( "try-error" %in% class(result) ) {
      tbl <- dplyr::tibble(
        t_score = as.numeric(NA),
        p_value = as.numeric(NA),
        df_value = as.numeric(NA)
      )
    }
    return(tbl)
  }
  
  # TODO:  In R 3.5.3, the stats::t.test() function generates warnings inside of
  # TODO:  pat_aggregate(). Suppress these here.
  
  suppressWarnings({
    ttestData <- 
      pat %>%
      pat_extractData() %>%
      patData_aggregate(FUN)
  })
  
  # ----- Create hourly dataframe ----------------------------------------------
  
  # NOTE:  Include variables used in QC so that they can be used to create a
  # NOTE:  plot visualizing the how the QC algorithm rejects values.
  
  hourlyData <-
    dplyr::tibble(datetime = meanData$datetime) %>% 
    
    # Create pm25 by averaging the A and B channel aggregation means
    dplyr::mutate(pm25 = (meanData$pm25_A + meanData$pm25_B) / 2) %>%
    
    # Calculate min_count and mean_diff for use in QC
    dplyr::mutate(min_count = pmin(countData$pm25_A, countData$pm25_B, na.rm = TRUE)) %>%
    dplyr::mutate(mean_diff = abs(meanData$pm25_A - meanData$pm25_B)) %>%
    
    # -----hourly_AB_01 QC algorithm -----
  
    # When only a fraction of the data are reporting, something is wrong.
    # Invalidate data where:  (min_count < SOME_THRESHOLD)
    dplyr::mutate(pm25 = replace(
      .data$pm25, 
      which(.data$min_count < min_count), 
      NA) 
    ) %>%
    
    # When the means are significantly different AND 'large', something is wrong.
    # Invalidate data where:  (p-value < 1e-4) & (mean_diff > 10)
    dplyr::mutate(pm25 = replace(
      .data$pm25,
      which( (ttestData$p_value < 1e-4) & (.data$mean_diff > 10) ),
      NA)
    ) %>% 
    
    # A difference of 20 ug/m3 should only be seen at very high levels.
    # Invalidate data where:  (mean < 100) & (mean_diff > 20)
    dplyr::mutate(pm25 = replace(
      .data$pm25,
      which( (.data$pm25 < 100) & (.data$mean_diff > 20) ),
      NA)
    )
  
  # ----- Return ---------------------------------------------------------------
  
  if ( returnAllColumns ) {
    
    # Add other columns of data used in this QC
    hourlyData <-
      hourlyData %>%
      dplyr::mutate(
        pm25_A_count = countData$pm25_A,
        pm25_B_count = countData$pm25_B,
        pm25_A_mean = meanData$pm25_A,
        pm25_B_mean = meanData$pm25_B,
        p_value = ttestData$p_value
      )
    
  } else {
    
    hourlyData <- 
      hourlyData %>%
      dplyr::select(.data$datetime, .data$pm25)
    
  }
  
  return(hourlyData)
  
}

