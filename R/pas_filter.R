#' @export
#' @importFrom rlang .data
#' 
#' @title General purpose filtering for PurpleAir Synoptic objects
#' 
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param ... Logical predicates defined in terms of the variables in the 
#'   \code{pas}. Multiple conditions are combined with & or seperated by a comma. 
#'   Only rows where the condition evaluates to TRUE are kept.
#' 
#' @description A generalized data filter for \emph{pas} objects to 
#'   choose rows/cases where conditions are true. Rows where the condition 
#'   evaluates to NA are dropped.
#' 
#' @return A subset of the given \emph{pas} object.
#' 
#' @seealso \link{pas_filterArea}
#' @seealso \link{pas_within}
#' 
#' @examples
#' nrow(example_pas)
#' ca <- pas_filter(example_pas, stateCode == "CA")
#' nrow(ca)

pas_filter <- function(pas, ...) { 
  
  pas <- 
    pas %>% 
    dplyr::filter(...)
  
  return(pas)
  
}
