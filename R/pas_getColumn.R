#' @export
#' @importFrom rlang .data
#' 
#' @title Return column of data from filtered PurpleAir Synoptic objects
#' 
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param name Name of the column to return.
#' @param pattern Text pattern used to filter sensor labels.
#' @param idPattern Text pattern used to filter \code{deviceDeploymentID}.
#' @param isOutside Logical, is the sensor located outside?
#' 
#' @description The incoming \code{pas} object is first filtered based on the 
#' values of \code{states}, \code{pattern} and \code{isOutside}.
#' The values associated with the \code{name} column are then returned.
#' 
#' @return Vector of values.
#' 
#' @seealso \code{\link{pas_getIDs}},  \code{\link{pas_getLabels}}
#' 
#' @examples
#' library(AirSensor)
#' 
#' example_pas %>%
#'   pas_getColumn(name = "latitude") %>%
#'   head(10)
#'   
 
pas_getColumn <- function(
  pas = NULL,
  name = NULL,
  pattern = ".*",
  idPattern = ".*",
  isOutside = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(name)
  MazamaCoreUtils::stopIfNull(pattern)
  MazamaCoreUtils::stopIfNull(isOutside)
  
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
    sub_pas <- pas %>% pas_filter(.data$location_type == "outside")
  } else {
    sub_pas <- pas %>% pas_filter(.data$location_type != "outside")
  }
  
  # Filter by pattern
  sub_pas <- sub_pas %>% pas_filter(stringr::str_detect(.data$label, pattern))
  
  # Filter by deviceDploymentID
  sub_pas <- 
    sub_pas %>% 
    pas_filter(stringr::str_detect(.data$deviceDeploymentID, idPattern))
  
  # ---- Return ----------------------------------------------------------------
  
  # NOTE:  Use !! to get the external variable defined by name rather than the
  # NOTE:  tibble column "name".
  column <- dplyr::pull(sub_pas, !!name)
  
  return(column)
  
}






