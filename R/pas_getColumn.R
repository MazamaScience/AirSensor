#' @export
#' @importFrom rlang .data
#' 
#' @title Return column of data from filtered PurpleAir Synoptic objects
#' 
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param name Name of the column to return.
#' @param pattern Text pattern used to filter station labels.
#' @param isOutside Logical, is the sensor located outside?
#' @param isParent Logical, is the sensor a parent station?
#' 
#' @description The incoming \code{pas} object is first filtered based on the 
#' values of \code{states}, \code{pattern}, \code{isOutside} and \code{isParent}.
#' The values associated with the \code{name} column are then returned.
#' 
#' This function is useful for returning values associated with specific
#' \emph{devices}, which are represented by records with \code{isParent = TRUE}.
#' 
#' @return Vector of values.
#' 
#' @seealso \code{\link{pas_getIDs}},  \code{\link{pas_getLabels}}
#' 
#' @examples
#' pas <- example_pas
#' latitude <- pas_getColumn(pas = pas, name = "latitude")
#' 
#' 
pas_getColumn <- function(
  pas = NULL,
  name = NULL,
  pattern = ".*",
  isOutside = TRUE,
  isParent = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(name)
  MazamaCoreUtils::stopIfNull(pattern)
  MazamaCoreUtils::stopIfNull(isOutside)
  MazamaCoreUtils::stopIfNull(isParent)
  
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
  
  # Name must match a column in the PAS
  if ( !name %in% names(pas) ) 
    stop(sprintf("'%s' is not a column name in the pas object", name))
  
  # ----- Filter data ----------------------------------------------------------
  
  # Filter by Outside/Inside
  if ( isOutside ) {
    sub_pas <- pas %>% pas_filter(.data$DEVICE_LOCATIONTYPE == "outside")
  } else {
    sub_pas <- pas %>% pas_filter(.data$DEVICE_LOCATIONTYPE != "outside")
  }
  
  # Filter by Parent
  if ( isParent ) {
    sub_pas <- sub_pas %>% pas_filter(is.na(.data$parentID))
  } else {
    sub_pas <- sub_pas %>% pas_filter(!is.na(.data$parentID))
  }
  
  # Filter by pattern
  sub_pas <- sub_pas %>% pas_filter(stringr::str_detect(.data$label, pattern))
  
  # ---- Return ----------------------------------------------------------------
  
  column <- dplyr::pull(sub_pas, name)
  
  return(column)
  
}






