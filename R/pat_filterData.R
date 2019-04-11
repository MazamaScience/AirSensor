#' @export
#' @importFrom rlang .data
#' 
#' @title General purpose PurpleAir time series filter
#' 
#' @param pat a pa_timeseries dataframe
#' @param ... Logical predicates defined in terms of the variables in the 
#' `pat$data`. Multiple conditions are combined with & or seperated by a comma. 
#' Only rows where the condition evaluates to TRUE are kept.
#' 
#' @description A generalized data filter for pa_timeseries (`pat`) objects to 
#' choose rows/cases where conditions are true. Rows where the condition 
#' evaluates to NA are dropped.
#' 
#' @return a pa_timeseries object
#' @seealso \link{pat_filterDate}
#' @examples
#' \dontrun{
#' unhealthy <- pat_filterData(pat, pm25_A > 55.5, pm25_B > 55.5) 
#' }

pat_filterData <- function(pat, ...) {
  
  pat$data <- 
    dplyr::filter(pat$data,...)
  
  return(pat)
  
}
