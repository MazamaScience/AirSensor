#' @export
#' @importFrom rlang .data
#' 
#' @title Create sensor-deployent IDs for specific sensors
#' 
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param ids Vector of sensor IDs.
#' 
#' @description The "sensor-deployment" ID is the unique time series identifier
#' used in \emph{pat} archive file names and in \emph{airsensor} objects. It is
#' created by combining a digest of the location (with an accuracy of < 0.1 
#' meters) with the device ID.
#' 
#' It is intended to be associated with the parent ID (A channel)
#' in the \code{pas} object.
#' 
#' @return Vector of time series identifiers.
#' 
pas_sensorDeploymentID <- function(
  pas = NULL,
  ids = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(ids)
  
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
  
  # ----- Create IDs -----------------------------------------------------------
  
  sub_pas <-
    pas %>%
    pas_filter(.data$ID %in% ids)
  
  # Check for parent
  if ( any(!is.na(sub_pas$parentID)) ) {
    badIDs <- 
      sub_pas %>% 
      pas_filter(!is.na(.data$parentID)) %>%
      dplyr::pull(.data$ID)
    
    stop(sprintf(
      "The following IDs are not associated with the A channel: %s",
      paste0(badIDs, collapse = ", ")
    ))
  }
  
  # Get location-specific identifier
  locationID <- MazamaLocationUtils::location_createID(sub_pas$longitude, sub_pas$latitude)
  
  # Create "deployment-sensor" identifier
  sensorDeploymentID <- paste0(locationID, "_", sub_pas$ID)
  
  # ---- Return ----------------------------------------------------------------
  
  return(sensorDeploymentID)
  
}






