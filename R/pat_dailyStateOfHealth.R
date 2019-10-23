#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains
#' 
#' @title Daily state of health
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' 
#' @description The number of valid (\emph{i.e.}, not NA or out-of-spec) sensor 
#' measurements are summed over the course of a calendar day (24 
#' hours unless a partial day is included in the data), then divided by the 
#' total number of measurements the sensor actually recorded in during that day 
#' (including NA and out-of-spec values) to return a percentage of the total
#' recorded measurements that are considered plausible.
#' 
#' @examples  
#' tbl <- 
#'   example_pat_failure_B %>%
#'   SoH_dailyPctValid() 
#' 
#' timeseriesTbl_multiplot(tbl, ylim = c(0,100))

pat_dailyStateOfHealth <- function(
  pat = NULL,
  FUNs = c(SoH_dailyPctDC, SoH_dailyPctReporting, SoH_dailyPctValid, SoH_dailyCorrelation)
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  
  
  # ----- pat_dailyStateOfHealth() ---------------------------------------------------
  
pctDC <- SoH_dailyPctDC(pat)
pctReport <- SoH_dailyPctDC(pat)
pctValid <- SoH_dailyPctValid(pat)
dailyCor <- SoH_dailyCorrelation()
  
}












