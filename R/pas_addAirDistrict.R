#' @importFrom MazamaCoreUtils logger.isInitialized logger.trace logger.warn logger.error logger.fatal
#' @importFrom rlang .data
#'
#' @export
#'
#' @title Add an air district to PurpleAir Synoptic Data
#'
#' @description Adds an air district (if any) to a pa_synoptic object via the 
#' \code{MazamaSpatialUtils} Package using PurpleAir location coordinates to 
#' determine the air basin the sensor is in. 
#' 
#' @note As of 2020-04-14, only California air basins is supported. 
#'
#' @param pas PurpleAir Synoptic \emph{pas} object. 
#'
#' @return A pa_synoptic dataframe
#' 
#' @seealso \link{pas_enhanceData}
#'
#' @examples
#' \donttest{
#' library(AirSensor)
#' 
#' initializeMazamaSpatialUtils()
#' 
#' pas_enhanced <-
#'   example_pas_raw %>% 
#'   pas_addSpatialMetadata() %>%  
#'   pas_addAirDistrict() 
#' }

pas_addAirDistrict <- function(
  pas = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pas)
  
  if ( !pas_hasSpatial(pas) ) {
    stop("Parameter 'pas' does not contain required spatial metadata.
          See 'pas_addSpatialMetadata()' to add spatial meta data.")
  }
  
  # ----- CARB Air Districts ---------------------------------------------------
  
  # NOTE: Currently (2020-04) only California Air basins is supported. 
  
  if ( "CA" %in% pas$stateCode ) {
    
    result <- try({ 
      CA_AirBasins <- get(MazamaSpatialUtils::loadSpatialData("CA_AirBasins_01"))
    }, silent = TRUE)
    
    if ( "try-error" %in% class(result) ) {
      
      logger.warn("Unable to load spatial data 'CA_AirBasins'.")
      
    } else {
      
      pas_CA <- 
        pas %>% 
        dplyr::filter(.data$countryCode == "US" & .data$stateCode == "CA")
      
      if ( nrow(pas_CA) > 0 ) {
        
        pas_CA$airDistrict <- 
          MazamaSpatialUtils::getSpatialData(
            pas_CA$longitude,
            pas_CA$latitude,
            CA_AirBasins,
            useBuffering = TRUE
          ) %>%
          dplyr::pull("name")
        
        pas_nonCA <-  
          pas %>% 
          dplyr::filter(.data$countryCode != "US" | .data$stateCode != "CA")
        
        pas_nonCA$airDistrict <- as.character(NA)
        
        pas <- dplyr::bind_rows(pas_CA, pas_nonCA)
        
      }
      
    } # END of CA_AirBasins exists
    
  } # END of "CA" %in% pas$stateCode
 
  # ----- Return --------------------------------------------------------------- 
  
  # Add airDistrict if it hasn't already been added
  if ( !"airDistrict" %in% names(pas) ) {
    pas$airDistrict <- as.character(NA)
  }
  
  return(pas)
  
}