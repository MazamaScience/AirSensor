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
#' @seealso \link{pas_filterArea}, \link{pas_filterNear}
#' 
#' @examples
#' library(AirSensor)
#' 
#' nrow(example_pas)
#' 
#' # California
#' ca <- pas_filter(example_pas, stateCode == "CA")
#' nrow(ca)
#' 
#' # Seal Beach
#' scsb <- 
#'   ca %>%
#'   pas_filter(stringr::str_detect(label, "^SCSB_"))
#' nrow(scsb)
#' 
#'  \dontrun{
#' pas_leaflet(ca)
#' 
#' pas_leaflet(scsb, maptype = "satellite")
#' }

pas_filter <- function(
  pas, 
  ...
) { 

  # ----- Validate parameters --------------------------------------------------
  
  # A little involved to catch the case where the user forgets to pass in 'pas'
  
  result <- try({
    if ( !pas_isPas(pas) )
      stop("First argument is not of class 'pas'.")
  }, silent = TRUE)
  
  if ( class(result) %in% "try-error" ) {
    err_msg <- geterrmessage()
    if ( stringr::str_detect(err_msg, "object .* not found") ) {
      stop(paste0(err_msg, "\n(Did you forget to pass in the 'pas' object?)"))
    }
  }
  
  # ----- Apply filter ---------------------------------------------------------
  
  pas <- 
    pas %>% 
    dplyr::filter(...)
  
  # ----- Return ---------------------------------------------------------------
  
  return(pas)
  
}
