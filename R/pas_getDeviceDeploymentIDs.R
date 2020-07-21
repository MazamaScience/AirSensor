#' @export
#' @importFrom rlang .data
#' 
#' @title Return timeseries identifiers from filtered PurpleAir Synoptic objects
#' 
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param pattern Text pattern used to filter station labels.
#' @param idPattern Text pattern used to filter \code{deviceDeploymentID}.
#' @param isOutside Logical, is the sensor located outside?
#' @param isParent Logical, is the record associated with a the A channel?
#' 
#' @description The incoming \code{pas} object is first filtered based on the 
#' values of \code{stateCodes}, \code{pattern}, \code{isOutside} and \code{isParent}.
#' The values associated with the \code{"deviceDeploymentID"} column are then 
#' returned.
#' 
#' This function is useful for returning a vector of unique time series
#' identifiers. These are used in the names of pre-generated \emph{pat} files
#' found in data archives.
#' 
#' @return Vector of values.
#' 
#' @seealso \code{\link{pas_getColumn}},  \code{\link{pas_getLabels}}
#' 
pas_getDeviceDeploymentIDs <- function(
  pas = NULL,
  pattern = ".*",
  idPattern = ".*",
  isOutside = TRUE,
  isParent = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  # Validation is handled by pas_getColumn()
  
  # ----- Get labels -----------------------------------------------------------
  
  IDs <- pas_getColumn(
    pas, 
    name = "deviceDeploymentID",
    pattern = pattern, 
    idPattern = idPattern,
    isOutside = isOutside, 
    isParent = isParent
  )
  
  # ---- Return ----------------------------------------------------------------
  
  return(IDs)
}






