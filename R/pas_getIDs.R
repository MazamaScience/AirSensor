#' @export
#' @importFrom rlang .data
#' 
#' @title Return IDs from filtered PurpleAir Synoptic objects
#' 
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param pattern Text pattern used to filter station labels.
#' @param idPattern Text pattern used to filter \code{deviceDeploymentID}.
#' @param isOutside Logical, is the sensor located outside?
#' 
#' @description The incoming \code{pas} object is first filtered based on the 
#' values of \code{stateCodes}, \code{patter} and \code{isOutside}.
#' The values associated with the \code{"ID"} column are then returned.
#' 
#' @return Vector of values.
#' 
#' @seealso \code{\link{pas_getColumn}},  \code{\link{pas_getLabels}}
#' 
pas_getIDs <- function(
  pas = NULL,
  pattern = ".*",
  idPattern = ".*",
  isOutside = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  # Validation is handled by pas_getColumn()
  
  # ----- Get labels -----------------------------------------------------------
  
  IDs <- pas_getColumn(
    pas, 
    name = "ID", 
    pattern = pattern, 
    idPattern = idPattern,
    isOutside = isOutside
  )
  
  # ---- Return ----------------------------------------------------------------
  
  return(IDs)
}






