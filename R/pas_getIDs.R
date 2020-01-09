#' @export
#' @importFrom rlang .data
#' 
#' @title Return IDs from filtered PurpleAir Synoptic objects
#' 
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param countryCodes ISO country codes used to subset the data.
#' @param stateCodes ISO country codes used to subset the data.
#' @param pattern Text pattern used to filter station labels.
#' @param isOutside Logical, is the sensor located outside?
#' @param isParent Logigal, is the sensor a parent station?
#' 
#' @description The incoming \code{pas} object is first filtered based on the 
#' values of \code{stateCodes}, \code{patter}, \code{isOutside} and \code{isParent}.
#' The values associated with the \code{"ID"} column are then returned.
#' 
#' This function is useful for returning values associated with specific
#' \emph{devices}, which are represented by records with \code{isParent = TRUE}.
#' 
#' @return Vector of values.
#' 
#' @seealso \code{\link{pas_getColumn}},  \code{\link{pas_getLabels}}
#' 
pas_getIDs <- function(
  pas = NULL,
  countryCodes = c('US'),
  stateCodes = NULL,
  pattern = ".*",
  isOutside = TRUE,
  isParent = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  # Validation is handled by pas_getColumn()
  
  # ----- Get labels -----------------------------------------------------------
  
  IDs <- pas_getColumn(
    pas, 
    name = "ID", 
    countryCodes = countryCodes,
    stateCodes = stateCodes, 
    pattern = pattern, 
    isOutside = isOutside, 
    isParent = isParent
  )
  
  # ---- Return ----------------------------------------------------------------
  
  return(IDs)
}






