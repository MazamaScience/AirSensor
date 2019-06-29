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
#' aredropped.
#' 
#' @return A subset of the incoming \code{pat}.
#' 
#' @seealso \link{pat_filterDate}
#' @examples
#' \dontrun{
#' unhealthy <- pat_filter(example_pat, pm25_A > 55.5, pm25_B > 55.5)
#' head(unhealthy)
#' }

pat_filter <- function(
  pat = NULL, 
  ...
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  # ----- Filter data ----------------------------------------------------------
  
  pat$data <- 
    dplyr::filter(pat$data,...)
  
  return(pat)
  
}
