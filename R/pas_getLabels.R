#' @export
#' @importFrom rlang .data
#' 
#' @title Return labels from filtered PurpleAir Synoptic objects
#' 
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param pattern Text pattern used to filter station labels.
#' @param isOutside Logical, is the sensor located outside?
#' @param isParent Logical, is the sensor a parent station?
#' 
#' @description The incoming \code{pas} object is first filtered based on the 
#' values of \code{stateCodes}, \code{patter}, \code{isOutside} and \code{isParent}.
#' The values associated with the \code{"label"} column are then returned.
#' 
#' This function is useful for returning values associated with specific
#' \emph{devices}, which are represented by records with \code{isParent = TRUE}.
#' 
#' @return Vector of values.
#' 
#' @seealso \code{\link{pas_getColumn}},  \code{\link{pas_getIDs}},  \code{\link{pas_getDeviceDeploymentIDs}}
#' 
#' @examples 
#' 
#' pas <- example_pas
#' 
#' labels <- pas_getLabels(pas = pas)
#' 
#' labels_back <- pas_getLabels(pas = pas, pattern = "back")
#' 
#' 
pas_getLabels <- function(
  pas = NULL,
  pattern = ".*",
  isOutside = TRUE,
  isParent = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  # Validation is handled by pas_getColumn()
  
  # ----- Get labels -----------------------------------------------------------
  
  labels <- pas_getColumn(
    pas, 
    name = "label", 
    pattern = pattern, 
    isOutside = isOutside, 
    isParent = isParent
  )
  
  # ---- Return ----------------------------------------------------------------
  
  return(labels)
}






