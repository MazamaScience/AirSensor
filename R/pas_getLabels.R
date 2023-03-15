#' @export
#' @importFrom rlang .data
#' 
#' @title Return labels from filtered PurpleAir Synoptic objects
#' 
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param pattern Text pattern used to filter station labels.
#' @param idPattern Text pattern used to filter \code{deviceDeploymentID}.
#' @param isOutside Logical, is the sensor located outside?
#' 
#' @description The incoming \code{pas} object is first filtered based on the 
#' values of \code{stateCodes}, \code{pattern} and \code{isOutside}.
#' The values associated with the \code{"label"} column are then returned.
#' 
#' @return Vector of values.
#' 
#' @seealso \code{\link{pas_getColumn}},  \code{\link{pas_getIDs}},  \code{\link{pas_getDeviceDeploymentIDs}}
#' 
#' @examples 
#' library(AirSensor)
#' 
#' pas <- example_pas
#' 
#' pas_getLabels(pas = pas) %>% head(10)
#' 
#' pas_getLabels(pas = pas, pattern = "back") %>% head(10)
#' 
#' 
pas_getLabels <- function(
  pas = NULL,
  pattern = ".*",
  idPattern = ".*",
  isOutside = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  # Validation is handled by pas_getColumn()
  
  # ----- Get labels -----------------------------------------------------------
  
  labels <- pas_getColumn(
    pas, 
    name = "label", 
    pattern = pattern, 
    idPattern = idPattern,
    isOutside = isOutside
  )
  
  # ---- Return ----------------------------------------------------------------
  
  return(labels)
}






