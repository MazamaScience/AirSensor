#' @export
#' 
#' @title Initialize Mazama Spatial Utils
#' 
#' @description Convenience function that wraps:
#' 
#' \preformatted{
#'   logger.setup()
#'   logger.setLevel(WARN)
#'   MazamaSpatialUtils::setSpatialDataDir('~/Data/Spatial')
#'   MazamaSpatialUtils::loadSpatialData('NaturalEarthAdm1')
#' }
#' 
#' If file logging is desired, these commands should be run individually with
#' output log files specified as arguments to \code{logger.setup()} from the
#' \pkg{MazamaCoreUtils} package.
#' 
#' @param spatialDataDir directory where spatial datasets are created
#' @param stateCodeDataset MazamaSpatialUtils dataset returning ISO 3166-2 
#' alpha-2 stateCodes
#' @param logLevel directory where spatial datasets are created

initializeMazamaSpatialUtils <- function(spatialDataDir = '~/Data/Spatial',
                                         stateCodeDataset = 'NaturalEarthAdm1',
                                         logLevel = WARN) {
  
  logger.setup()
  logger.setLevel(logLevel)
  MazamaSpatialUtils::setSpatialDataDir(spatialDataDir)
  MazamaSpatialUtils::loadSpatialData(stateCodeDataset)
  
}
