#' @importFrom MazamaCoreUtils logger.isInitialized logger.trace logger.warn logger.error logger.fatal
#' @importFrom rlang .data
#'
#' @export
#'
#' @title Add an air district to PurpleAir Synoptic Data
#'
#' @description Adds a community region (if any) to a pa_synoptic object via the 
#' pa_synotpic object via pre-defined labeling shcema. 
#' 
#' @note As of 2020-04-14, only California air basins is supported. 
#'
#' @param pas PurpleAir Synoptic \emph{pas} object. 
#'
#' @return A pa_synoptic dataframe
#' 
#' @seealso \link{pas_enhanceRawData}
#'
#' @examples
#' \donttest{
#' library(AirSensor)
#' 
#' initializeMazamaSpatialUtils()
#' 
#' pas_community <-
#'   example_pas_raw %>% 
#'   pas_enhanceRawData() %>% 
#'   pas_addCommunityRegion()
#' }

pas_addCommunityRegion <- function(
  pas = NULL
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pas)
  
  if ( !any(stringr::str_detect(names(pas), '^[lL]abel')) ) {
    stop('Invalid pa_synotpic data - does not contain required column: `label`')
  } 
  
  # Extract labels with `labels` or `Labels` column vector
  label <- tolower(pas[[stringr::str_which(names(pas), '^[lL]abel')]])
  
  # Create empty community region
  pas$communityRegion <- as.character(NA)
  
  #  ----- SCAQMD communities --------------------------------------------------
  
  # NOTE:  Need to match "sctv_15 (dawson canyon) b"
  scah_mask <- stringr::str_detect(label, "^scah_[0-9]{1,2}( ?.*$)")
  scan_mask <- stringr::str_detect(label, "^scan_[0-9]{1,2}( ?.*$)")
  scap_mask <- stringr::str_detect(label, "^scap_[0-9]{1,2}( ?.*$)")
  scbb_mask <- stringr::str_detect(label, "^scbb_[0-9]{1,2}( ?.*$)")
  scem_mask <- stringr::str_detect(label, "^scem_[0-9]{1,2}( ?.*$)")
  schs_mask <- stringr::str_detect(label, "^schs_[0-9]{1,2}( ?.*$)")
  sciv_mask <- stringr::str_detect(label, "^sciv_[0-9]{1,2}( ?.*$)")
  scnp_mask <- stringr::str_detect(label, "^scnp_[0-9]{1,2}( ?.*$)")
  scpr_mask <- stringr::str_detect(label, "^scpr_[0-9]{1,2}( ?.*$)")
  scsb_mask <- stringr::str_detect(label, "^scsb_[0-9]{1,2}( ?.*$)")
  scsc_mask <- stringr::str_detect(label, "^scsc_[0-9]{1,2}( ?.*$)")
  scsg_mask <- stringr::str_detect(label, "^scsg_[0-9]{1,2}( ?.*$)")
  scsh_mask <- stringr::str_detect(label, "^scsh_[0-9]{1,2}( ?.*$)")
  scsj_mask <- stringr::str_detect(label, "^scsj_[0-9]{1,2}( ?.*$)")
  sctv_mask <- stringr::str_detect(label, "^sctv_[0-9]{1,2}( ?.*$)")
  scuv_mask <- stringr::str_detect(label, "^scuv_[0-9]{1,2}( ?.*$)")
  
  pas$communityRegion[scah_mask] <- "SCAH"
  pas$communityRegion[scan_mask] <- "SCAN"
  pas$communityRegion[scap_mask] <- "Alhambra/Monterey Park"
  pas$communityRegion[scbb_mask] <- "Big Bear Lake"
  pas$communityRegion[scem_mask] <- "El Monte"
  pas$communityRegion[schs_mask] <- "Sycamore Canyon"   # typo on someone's part
  pas$communityRegion[sciv_mask] <- "Imperial Valley"
  pas$communityRegion[scnp_mask] <- "Nipomo"
  pas$communityRegion[scpr_mask] <- "Paso Robles"
  pas$communityRegion[scsb_mask] <- "Seal Beach"
  pas$communityRegion[scsc_mask] <- "Seal Beach"        # typo on someone's part
  pas$communityRegion[scsg_mask] <- "South Gate"
  pas$communityRegion[scsh_mask] <- "Sycamore Canyon"
  pas$communityRegion[scsj_mask] <- "San Jacinto"
  pas$communityRegion[sctv_mask] <- "Temescal Valley"
  pas$communityRegion[scuv_mask] <- "SCUV"
  
  # ----- Other Communties -----------------------------------------------------
  # NA 
  
  return(pas)
  
}
