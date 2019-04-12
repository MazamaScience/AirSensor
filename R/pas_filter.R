#' @export
#' @importFrom rlang .data
#' 
#' @title General purpose PurpleAir synoptic filter
#' 
#' @param pas a pa_synoptic dataframe
#' @param ... Logical predicates defined in terms of the variables in the 
#' \code{pas}. Multiple conditions are combined with & or seperated by a comma. 
#' Only rows where the condition evaluates to TRUE are kept.
#' 
#' @description A generalized data filter for pa_synoptic \emph{pas} objects to 
#' choose rows/cases where conditions are true. Rows where the condition 
#' evaluates to NA are dropped.
#' 
#' @return a pa_synoptic object
#' @seealso \link{pas_load}
#' @examples
#' \dontrun{
#' californiaSensors <- pas_filter(pas, stateCode == "CA") 
#' }

pas_filter <- function(pas, ...) { 
  
  pas <- 
    pas %>% 
    dplyr::filter(...)
  
  return(pas)
  
}
