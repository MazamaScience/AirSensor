#' @export
#' @importFrom rlang .data
#' 
#' @title Return station labels from filtered PurpleAir Synoptic objects
#' 
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param states Vector of recognized  ISO state codes 
#' @param pattern Text pattern used to filter station labels
#' @param isOutside Logical, is the station located outside?
#' @param isParent Logigal, is the station a parent station?
#' 
#' @description A filter for \emph{pas} objects to return the labels of the 
#' stations of interest. Wrapper around \emph{pas_getColumn}.
#' 
#' @return A character vector of station labels.
#' 
#' 
#' 
pas_getLabels <- function(
  pas = NULL,
  states = NULL,
  pattern = ".*",
  isOutside = TRUE,
  isParent = TRUE
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
  
  # ----- pas_getLabels() ------------------------------------------------------
  

  labels <- pas_getColumn(pas, 
                          name = "label", 
                          states = states, 
                          pattern = pattern, 
                          isOutside = isOutside, 
                          isParent = isParent)
  
  # ---- Return ----------------------------------------------------------------
  
  return(labels)
}






