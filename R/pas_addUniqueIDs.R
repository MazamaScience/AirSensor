#' @export
#' 
#' @title Add Unique Identifiers to PurpleAir Synoptic Data
#' 
#' @description Generates and adds a unique identification vector to PurpleAir
#' sensors using the \code{MazamaLocationUtils} package, which creates a unique 
#' ID based upon coordinate location and device id. 
#' 
#' Adds the following vectors: 
#' \itemize{
#'   \item{deviceID --PurpleAir ID}
#'   \item{locationID -- MazamaLocationUtils generated location ID }
#'   \item{deviceDeploymentID -- A combination of device and location IDs}
#' }
#'
#' @param pas a pa_synoptic dataframe
#'
#' @return A dataframe with generated unique ID columns added.
#' 
#' @seealso \link{pas_addSpatialMetadata}
#' 
#' @examples
#' \dontrun{
#' example_pas_raw %>% 
#'   pas_addSpatialMetadata() %>% 
#'   pas_addUniqueIDs()
#'   }
 
pas_addUniqueIDs <- function(
  pas = NULL
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pas)

  if ( !pas_hasSpatial(pas) ) {
    stop('Parameter `pas` does not contain required spatial metadata. 
          See `pas_addSpatialMetadata()` to add spatial meta data.')
  }
  
  if ( !c('ID', 'parentID') %in% names(pas) ) {
    stop('Invalid pa_synotpic data: does not contain required columns.')
  }
  
  # ----- Generate Unique IDs --------------------------------------------------
  
  # Device ID based on A channel ID
  pas$deviceID <- pas$ID
  # Ensure that B channel records use the parent (A channel) ID
  childMask <- !is.na(pas$parentID)
  pas$deviceID[childMask] <- pas$parentID[childMask]
  
  # Location ID
  pas$locationID <- MazamaLocationUtils::location_createID(pas$longitude, pas$latitude)
  
  # Device Deployment ID
  pas$deviceDeploymentID <- paste0(pas$locationID, "_", pas$deviceID)
  
  return(pas)
  
}
