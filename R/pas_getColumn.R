#' @export
#' @importFrom rlang .data
#' 
#' @title Return column of data from filtered PurpleAir Synoptic objects
#' 
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param name Name of the column to return
#' @param states Vector of recognized  ISO state codes 
#' @param pattern Text pattern used to filter station labels
#' @param isOutside Logical, is the station located outside?
#' @param isParent Logigal, is the station a parent station?
#' 
#' @description A filter for \emph{pas} objects to return the column of the 
#' stations of interest
#' 
#' @return A column of data.
#' 
#' 
#' 
pas_getColumn <- function(
  pas = NULL,
  name = NULL,
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
  
  # ----- pas_getColumn() ------------------------------------------------------
  
  # filter by Outside/Inside
  if ( !is.null(isOutside) ) {
    if ( isOutside ) {
      sub_pas <- pas %>% pas_filter(.data$DEVICE_LOCATIONTYPE == "outside")
    } else {
      sub_pas <- sub_pas %>% pas_filter(.data$DEVICE_LOCATIONTYPE == "inside")
    }
  }
  
  if ( is.null(isOutside) ) {
    sub_pas <- pas %>% pas_filter(.data$DEVICE_LOCATIONTYPE == "outside")
  }
  
  # filter by Parent
  if ( !is.null(isParent) ) {
    if ( isParent ) {
      sub_pas <- sub_pas %>% pas_filter(is.na(.data$parentID))
    } else {
      sub_pas <- sub_pas %>% pas_filter(!is.na(.data$parentID))
    }
  }
  
  if ( is.null(isParent) ) {
    sub_pas <- sub_pas %>% pas_filter(is.na(.data$parentID))
  }
  
  # filter by state code
  if ( !is.null(states) ) {
    sub_pas <- sub_pas %>% pas_filter(.data$stateCode %in% states)
  }
  
  if ( is.null(states) ) {
    sub_pas <- sub_pas %>% pas_filter(.data$stateCode %in% PWFSLSmoke::US_52)
  }
  
  
  # filter by label pattern
  if ( !is.null(pattern) ) {
    sub_pas <- sub_pas %>% pas_filter(stringr::str_detect(.data$label, pattern))
  }
  
  
  # make sure the name exists in a pas
  if(name %in% names(sub_pas)) {
    column <- dplyr::pull(sub_pas, name)
  }
  
  else {
    stop("'pas' does not contain requested column")
  }
  

  
  # ---- Return ----------------------------------------------------------------
  
  return(column)
}






