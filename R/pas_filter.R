#' @export
#' @importFrom rlang .data
#' 
#' @title General purpose \emph{pa_synoptic} object filter
#' 
#' @param pas A \emph{pa_synoptic} object.
#' @param ... Logical predicates defined in terms of the variables in the 
#'   \code{pas}. Multiple conditions are combined with & or seperated by a comma. 
#'   Only rows where the condition evaluates to TRUE are kept.
#' 
#' @description A generalized data filter for \emph{pa_synoptic} objects to 
#'   choose rows/cases where conditions are true. Rows where the condition 
#'   evaluates to NA are dropped.
#' 
#' @return A subset of the given \emph{pa_synoptic} object.
#' 
#' @seealso \link{pas_filterArea}, \link{pas_within}
#' 
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
