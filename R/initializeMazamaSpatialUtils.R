#' @export
#' @importFrom MazamaCoreUtils logger.setup logger.setLevel WARN
#' 
#' @title Initialize MazamaSpatialUtils package
#' 
#' @param spatialDataDir Directory where spatial datasets are created.
#' @param stateCodeDataset MazamaSpatialUtils dataset returning ISO 3166-2 .
#' alpha-2 stateCodes
#' @param logLevel Logging level used if logging has not already been 
#' initialized.
#' 
#' @description Convenience function that wraps:
#' 
#' \preformatted{
#'   data("SimpleCountriesEEZ", package = "MazamaSpatialUtils")
#'   data("SimpleTimezones", package = "MazamaSpatialUtils")
#'   MazamaSpatialUtils::setSpatialDataDir('~/Data/Spatial')
#'   MazamaSpatialUtils::loadSpatialData('NaturalEarthAdm1')
#' }
#' 
#' This function should be run before using \code{pas_load()}, as 
#' \code{pas_load()} uses the spatial data loaded by 
#' \code{initializeMazamaSpatialUtils()} to enhance  raw synoptic data via 
#' \code{pas_enhanceData()}.
#' 
#' If file logging is desired, these commands should be run individually with
#' output log files specified as arguments to \code{logger.setup()} from the
#' \pkg{MazamaCoreUtils} package.
#' 

initializeMazamaSpatialUtils <- function(
  spatialDataDir = '~/Data/Spatial',
  stateCodeDataset = 'NaturalEarthAdm1',
  logLevel = WARN
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  # Set up logging if not already set up
  if ( !MazamaCoreUtils::logger.isInitialized() ) {
    MazamaCoreUtils::logger.setup()
    MazamaCoreUtils::logger.setLevel(logLevel)
  }
  
  # ----- Load spatial data ----------------------------------------------------
  
  utils::data("SimpleCountriesEEZ", package = "MazamaSpatialUtils")
  utils::data("SimpleTimezones", package = "MazamaSpatialUtils")
  MazamaSpatialUtils::setSpatialDataDir(spatialDataDir)
  MazamaSpatialUtils::loadSpatialData(stateCodeDataset)
  
}
