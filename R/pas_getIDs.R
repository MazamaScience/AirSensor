#' @export
#' @importFrom rlang .data
#' 
#' @title Return ID column of data from filtered PurpleAir Synoptic objects
#' 
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' 
#' @description A filter for \emph{pas} objects to return the ID column of the 
#' \emph{pas} object.
#' 
#' @return A column of data.
#' 
#' 
#' 
pas_getIDs <- function(
  pas = NULL
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
  
  # ----- pas_getIDs() ------------------------------------------------------
  

  # get the sensor ID
  IDs <- pas_getColumn(pas, "ID")
  

 

  
  # ---- Return ----------------------------------------------------------------
  
  return(IDs)
}






