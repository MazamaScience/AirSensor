#' @export
#' @importFrom rlang .data
#' 
#' @title Apply hourly aggregation QC using "AB_O2" algorithm
#' 
#' @param pat A PurpleAir timeseries object.
#' @param min_count Aggregation bins with fewer than \code{min_count} measurements
#' will be marked as NA.
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
#' \item{Invalidate data where:  (A/B hourly MAD > 3)}
#' \item{Invalidate data where:  (A/B hourly pct_diff > 0.5)}
#' }
#' 
#' MAD = "Median Absolute Deviation"
#'
#' @note Purple Air II sensors reporting after the June, 2019 firmware
#' upgrade report data every 2 minutes or 30 measurements per hour. The default
#' setting of \code{min_count = 20} is equivalent to a required data recovery
#' rate of 67\%.
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

PurpleAirQC_hourly_AB_02 <- function(
   pat = NULL, 
   min_count = 20, 
   returnAllColumns = FALSE
) { 
   
   # ----- Validate parameters -------------------------------------------------
   
   MazamaCoreUtils::stopIfNull(pat)
   MazamaCoreUtils::stopIfNull(min_count)
   MazamaCoreUtils::stopIfNull(returnAllColumns)
   
   # ----- Prepare aggregated data ---------------------------------------------
   
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
   
   # Hourly MAD
   madData <-
      pat %>% 
      pat_aggregate( function(x) { stats::mad(x, na.rm = TRUE) } ) %>%
      pat_extractData()
   
   # Hourly pctDiff
   A <- meanData$pm25_A
   B <- meanData$pm25_B
   pctDiff <- abs(A - B) / ((A + B + 0.01)/2) # add 0.01 to avoid division by zero
   
   # ----- Create masks --------------------------------------------------------
   
   # When only a fraction of the data are reporting, something is wrong.
   # Invalidate data where:  (min_count < SOME_THRESHOLD)
   minCountMask <- pmin(countData$pm25_A, countData$pm25_B, na.rm = TRUE) < min_count
   
   # When the A/B channels differ by a lot relative to their absolute value,
   # something is rong.
   # Invalidate data where (pctDiff > 0.5)
   pctDiffMask <- pctDiff > 0.5
   
   # When the median absolute deviation within an hour is high for either 
   # channel, something is probably wrong.
   # Invalidate data whre (MAD > 3) on either channel
   madMask <- (madData$pm25_A > 3) | (madData$pm25_B > 3)
   
   # ----- Create hourly dataframe ---------------------------------------------
   
   # NOTE:  Include variables used in QC so that they can be used to create a
   # NOTE:  plot visualizing the how the QC algorithm rejects values.
   
   hourlyData <-
      dplyr::tibble(datetime = meanData$datetime) %>% 
      
      # Create pm25 by averaging the A and B channel aggregation means
      dplyr::mutate(pm25 = (meanData$pm25_A + meanData$pm25_B) / 2) %>%
      
      # Create min_count, pctDiff
      dplyr::mutate(
         min_count = pmin(countData$pm25_A, countData$pm25_B, na.rm = TRUE),
         pct_diff = pctDiff,
         mad_A = madData$pm25_A,
         mad_B = madData$pm25_B
      ) %>%
      
      # -----hourly_AB_02 QC algorithm -----
   
      dplyr::mutate(pm25 = replace(.data$pm25, minCountMask, NA)) %>%
      dplyr::mutate(pm25 = replace(.data$pm25, pctDiffMask, NA)) %>%
      dplyr::mutate(pm25 = replace(.data$pm25, madMask, NA))
   
   # ----- Return ---------------------------------------------------------------
   
   if ( returnAllColumns ) {
      
      # Add other columns of data used in this QC
      hourlyData <-
         hourlyData %>%
         dplyr::mutate(
            pm25_A_count = countData$pm25_A,
            pm25_B_count = countData$pm25_B,
            pm25_A_mean = meanData$pm25_A,
            pm25_B_mean = meanData$pm25_B
         )
      
   } else {
      
      hourlyData <- 
         hourlyData %>%
         dplyr::select(.data$datetime, .data$pm25)
      
   }
   
   return(hourlyData)
   
}