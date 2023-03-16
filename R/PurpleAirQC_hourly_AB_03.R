#' @export
#' @importFrom rlang .data
#' 
#' @title Apply hourly aggregation QC using "AB_O4" algorithm
#' 
#' @param pat A PurpleAir timeseries object.
#' @param min_count Aggregation bins with fewer than \code{min_count} measurements
#' will be marked as NA.
#' @param returnAllColumns Logical specifying whether to return all columns
#' of statistical data generated for QC algorithm or just the final \code{pm25}
#' result.
#'  
#' @description Creates a \emph{pm25} timeseries by averaging aggregated data
#' from the A and B channels and applying the following QC logic:
#' 
#' \enumerate{
#' \item{Create pm25 by averaging the A and B channel aggregation means}
#' \item{Invalidate data where:  (min_count < 20)}
#' \item{Invalidate data where:  (A/B hourly difference > 5 AND A/B hourly percent difference > 70\%)}
#' \item{Invalidate data where:  (A/B hourly data recovery < 90\%)}
#' }
#'
#' @note Purple Air II sensors reporting after the June, 2019 firmware
#' upgrade report data every 2 minutes or 30 measurements per hour. The default
#' setting of \code{min_count = 20} is equivalent to a required data recovery
#' rate of 67\%. 
#' 
#' @return Data frame with columns \code{datetime} and \code{pm25}.
#' 

PurpleAirQC_hourly_AB_03 <- function(
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
   
   # Get percent recovered
   recoveredData <- 
      pat %>% 
      pat_extractData() %>%
      patData_aggregate( 
         function(df) { 
            A_recovered <- base::length(na.omit(df$pm25_A))/dim(df)[1] 
            B_recovered <- base::length(na.omit(df$pm25_B))/dim(df)[1]
            data.frame(A_recovered, B_recovered)
         }
      )
   
   # Hourly means
   meanData <-
      pat %>%
      pat_aggregate( function(x) { base::mean(x, na.rm = TRUE) } ) %>%
      pat_extractData()
   
   # Hourly pctDiff
   pctDiff <- abs(meanData$pm25_A - meanData$pm25_B) / 
      ((meanData$pm25_A + meanData$pm25_B + 0.01)/2) # add 0.01 to avoid division by zero
   
   minCount <- pmin(countData$pm25_A, countData$pm25_B, na.rm = TRUE)
   
   meanDiff <- abs(meanData$pm25_A - meanData$pm25_B)
   
   # ----- Create masks --------------------------------------------------------
   
   # When only a fraction of the data are reporting, something is wrong.
   # Invalidate data where:  (min_count < SOME_THRESHOLD)
   minCountMask <- pmin(countData$pm25_A, countData$pm25_B, na.rm = TRUE) < min_count
   
   # When the A/B channels differ by a lot relative to their absolute value,
   # something is wrong.
   # Invalidate data where (pctDiff > 0.5)
   pctDiffMask <- pctDiff > 0.7
   
   # Channel difference Mask
   # Invalidate when difference > 5
   diffMask <- meanDiff > 5
   
   # NOTE: Typical data recovery per 60-minute bin is 45/60 = 0.75.
   # NOTE: Flag anything below 0.75*0.9 = 0.675 
   recoveredMask <- recoveredData$A_recovered < (0.75*0.9) | recoveredData$B_recovered < (0.75*0.9)
   
   # ----- Create hourly dataframe ---------------------------------------------
   
   # NOTE:  Include variables used in QC so that they can be used to create a
   # NOTE:  plot visualizing how the QC algorithm rejects values.
   
   hourlyData <-
      # Fill dataframe
      dplyr::tibble(datetime = meanData$datetime) %>%
      dplyr::mutate(pm25 = (meanData$pm25_A + meanData$pm25_B) / 2) %>%
      dplyr::mutate(mean_diff = meanDiff) %>% 
      dplyr::mutate(pct_diff = pctDiff) %>% 
      # Replace pm25 with NA if count < minCount
      dplyr::mutate(
         pm25 = replace(
            .data$pm25, 
            minCountMask,
            NA
         )
      ) %>% 
      # Replace pm25 with NA if percent difference > 70% AND difference > 5
      dplyr::mutate(
         pm25 = replace(
            .data$pm25, 
            pctDiffMask & diffMask,
            NA
         )
      ) %>% 
      # Replace pm25 with NA if recovered data is < 90% 
      dplyr::mutate(
         pm25 = replace(
            .data$pm25, 
            recoveredMask,
            NA
         )
      )
   
   # ----- Return ---------------------------------------------------------------
   
   if ( returnAllColumns ) {
      
      # Add other columns of data used in this QC
      hourlyData <-
         hourlyData %>%
         dplyr::mutate(
            min_count = pmin(countData$pm25_A, countData$pm25_B, na.rm = TRUE),
            pm25_A_count = countData$pm25_A,
            pm25_B_count = countData$pm25_B,
            pm25_A_mean = meanData$pm25_A,
            pm25_B_mean = meanData$pm25_B,
            pm25_A_recovered = recoveredData$A_recovered,
            pm25_B_recovered = recoveredData$B_recovered,
         )
      
   } else {
      
      hourlyData <- 
         hourlyData %>%
         dplyr::select(.data$datetime, .data$pm25)
      
   }
   
   return(hourlyData)
   
}