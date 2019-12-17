#' @export
#' @importFrom rlang .data
#' 
#' @title Return column of data from filtered PurpleAir Synoptic objects
#' 
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param name Name of the column to return
#' 
#' @description A filter for \emph{pas} objects to return the column of the 
#' stations of interest
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
  

  # get the lat/lon
  longitude <- pas_getColumn(pas, "longitude")
  latitude <- pas_getColumn(pas, "latitude")
  
  # create location ID
  locationID <- MazamaLocationUtils::location_createID(longitude, latitude)
 

  
  # ---- Return ----------------------------------------------------------------
  
  return(locationID)
}






