#' @export
#' @importFrom rlang .data
#' 
#' @title Apply QC to Aggregation Statistics
#' 
#' @param pat A PurpleAir timeseries object.
#' @param min_count Aggregation bins with fewer than \code{min_count} measurements
#' will be marked as NA.
#' @param tolerance A maximum median absolute deviation threshold.  
#' @param max_diff A maximum percentage difference between pat channels (50\% = 0.5). 
#'  
#' @description Creates a \code{pm25} timeseries by averaging aggregated data
#' from the A and B channels, calculates MAD within the threshold, and values 
#' within a maxium percentage difference. 
#'
#' @note Purple Air II sensors reporting after the June, 2019 firmware
#' upgrade report data every 2 minutes or 30 measurements per hour. The default
#' setting of \code{min_count = 20} is equivalent to a required data recovery
#' rate of 67\%.
#' 
#' @return Data frame with columns \code{datetime} and \code{pm25}.
#' 
#' @examples 
#' QCed <- PurpleAirQC_hourly_AB_02(example_pat)
#' 
PurpleAirQC_hourly_AB_02 <- function(
  pat = NULL, 
  min_count = 20, 
  tolerance = 3,
  max_diff = 0.5
) { 
  
   # Create function to calculate the percent difference between chA and chB
   # NOTE:  Add 0.01 to avoid division by zero
   percent_diff <- function(x,y) { abs(x-y)/((x+y+0.01)/2) }
   
   # Aggregate hourly pat mean
   patMean <- pat_aggregate(pat, FUN = function(x) mean(x, na.rm = TRUE))
   
   # Logical - Flag exceeded channel differences  
   chDiffMask <- percent_diff(patMean$data$pm25_A, patMean$data$pm25_B) > max_diff
   
   # Calculate the mean of chA and chB
   chMean <- rowMeans(cbind(patMean$data$pm25_A, patMean$data$pm25_B), na.rm = TRUE, dims = 1L)
   
   # Aggregate hourly pat to NA omitted vector length
   patCount <- pat_aggregate(pat, FUN = function(x) length(na.omit(x)))$data
   
   # Logical - Flag under minimum counts
   minCountMask <- pmin(patCount$pm25_A, patCount$pm25_B, na.rm = TRUE) < min_count
   
   # Aggregate hourly pat median absolute deviation
   patMAD <- pat_aggregate(pat, FUN = function(x) stats::mad(x, na.rm = TRUE))$data
   
   # Logical - Flag out-of-tolerance MAD
   chA_MADMask <- patMAD$pm25_A > tolerance
   chB_MADMask <- patMAD$pm25_B > tolerance
   
   # Replace any masked values with NA, create new column `pm25` of channel mean
   patMean$data <- patMean$data %>% 
     dplyr::mutate(pm25 = chMean) %>% 
     dplyr::mutate(pm25 = replace(.data$pm25, minCountMask, NA)) %>% 
     dplyr::mutate(pm25 = replace(.data$pm25, chDiffMask, NA)) %>% 
     dplyr::mutate(pm25 = replace(.data$pm25, chA_MADMask | chB_MADMask, NA))
   
   return(patMean)
   
}