#' @export
#' @importFrom rlang .data
#' 
#' @title General purpose data filtering for PurpleAir Timeseries objects
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param ... Logical predicates defined in terms of the variables in the 
#' \code{pat$data}.
#' 
#' @description A generalized data filter for \emph{pat} objects to 
#' choose rows/cases where conditions are true.  Multiple conditions are 
#' combined with \code{&} or seperated by a comma. Only rows where the condition 
#' evaluates to TRUE are kept.Rows where the condition evaluates to \code{NA}
#' are dropped.
#' 
#' @return A subset of the incoming \code{pat}.
#' 
#' @seealso \link{pat_filterDate}
#' @examples
#' \dontrun{
#' library(AirSensor)
#' unhealthy <- pat_filter(example_pat, pm25_A > 55.5, pm25_B > 55.5)
#' head(unhealthy$data)
#' }

pat_filter <- function(
  pat, 
  ...
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  # A little involved to catch the case where the user forgets to pass in 'pat'
  
  result <- try({
    if ( !pat_isPat(pat) )
      stop("First argument is not of class 'pat'.")
  }, silent = TRUE)
  
  if ( class(result) %in% "try-error" ) {
    err_msg <- geterrmessage()
    if ( stringr::str_detect(err_msg, "object .* not found") ) {
      stop(paste0(err_msg, "\n(Did you forget to pass in the 'pat' object?)"))
    }
  }
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  # ----- Filter data ----------------------------------------------------------
  
  pat$data <- 
    dplyr::filter(pat$data,...)
  
  # ----- Return ---------------------------------------------------------------
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  return(pat)
  
}
