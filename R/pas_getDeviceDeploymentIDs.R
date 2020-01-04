#' @export
#' @importFrom rlang .data
#' 
#' @title Return timeseries identifiers from filtered PurpleAir Synoptic objects
#' 
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param states Vector of recognized ISO state codes.
#' @param pattern Text pattern used to filter station labels.
#' @param isOutside Logical, is the sensor located outside?
#' @param isParent Logigal, is the sensor a parent station?
#' 
#' @description The incoming \code{pas} object is first filtered based on the 
#' values of \code{states}, \code{patter}, \code{isOutside} and \code{isParent}.
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
  states = PWFSLSmoke::US_52,
  pattern = ".*",
  isOutside = TRUE,
  isParent = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  # Validation is handled by pas_getColumn()
  
  # ----- Get labels -----------------------------------------------------------
  
  IDs <- pas_getColumn(
    pas, 
    name = "deviceDeploymentID", 
    states = states, 
    pattern = pattern, 
    isOutside = isOutside, 
    isParent = isParent
  )
  
  # ---- Return ----------------------------------------------------------------
  
  return(IDs)
}






