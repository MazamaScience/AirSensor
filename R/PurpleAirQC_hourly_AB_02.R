#' @export
#' @importFrom rlang .data
#' @importFrom stats mad
#' 
#' @title Apply QC to Aggregation Statistics
#' 
#' @param pat A PurpleAir timeseries object.
#' @param min_count Aggregation bins with fewer than `min_count` measurements
#' will be marked as `NA`.
#' @param threshold A maximum median absolute deviation threshold.  
#' @param max_diff A maximum percentage difference between pat channels (50% = 0.5). 
#'  
#' @description Creates a \code{pm25} timeseries by averaging aggregated data
#' from the A and B channels and applying the following QC logic:
#'
#' @note Purple Air II sensors reporting after the June, 2019 firmware
#' upgrade report data every 2 minutes or 30 measurements per hour. The default
#' setting of \code{min_count = 20} is equivalent to a required data recovery
#' rate of 67%.
#' 
#' @return Data frame with columns \code{datetime} and \code{pm25}.
#' 
#' @examples 
#' \dontrun{
#' QCed <- PurpleAirQC_hourly_AB_02(example_pat)
#' }

PurpleAirQC_hourly_AB_02 <- function(
  pat = NULL, 
  min_count = 20, 
  threshold = 3,
  max_diff = 0.5
) { 
  
   percent_diff <- function(x,y) { abs(x-y)/((x+y)/2) }
   
   hour_mean <- pat_aggregate(pat, FUN = function(x) mean(x, na.rm = TRUE))
   ch_mean <- rowMeans(cbind(hour_mean$data$pm25_A, hour_mean$data$pm25_B), na.rm = TRUE, dims = 1L)
   bad_ch_diff <- percent_diff(hour_mean$data$pm25_A, hour_mean$data$pm25_B) > max_diff
   
   hour_count <- pat_aggregate(pat, FUN = function(x) length(na.omit(x)))$data
   bad_min_count <- pmin(hour_count$pm25_A, hour_count$pm25_B, na.rm = TRUE) < min_count
   
   # mad 
   hour_mad <- pat_aggregate(pat, FUN = function(x) mad(x, na.rm = TRUE))$data
   bad_mad_diff <- percent_diff(hour_mad$pm25_A, hour_mad$pm25_B) > max_diff 
   
   bad_mad_A <- bad_mad_diff & hour_mad$pm25_A > threshold
   bad_mad_B <- bad_mad_diff & hour_mad$pm25_B > threshold
   # 
   
   hour_mean$data <- hour_mean$data %>% 
     dplyr::mutate(pm25 = ch_mean) %>% 
     dplyr::mutate(pm25 = replace(.data$pm25, bad_min_count, NA)) %>% 
     dplyr::mutate(pm25 = replace(.data$pm25, bad_ch_diff, NA)) %>% 
     dplyr::mutate(pm25 = replace(.data$pm25, bad_mad_A | bad_mad_B, NA))
   
   return(hour_mean)
   
}