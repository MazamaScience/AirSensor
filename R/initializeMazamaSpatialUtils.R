#' @export
#' @importFrom MazamaCoreUtils logger.setup logger.setLevel WARN
#' 
#' @title Initialize MazamaSpatialUtils package
#' 
#' @description Convenience function that wraps:
#' 
#' \preformatted{
#'   logger.setup()
#'   logger.setLevel(WARN)
#'   data("SimpleCountriesEEZ", package = "MazamaSpatialUtils")
#'   data("SimpleTimezones", package = "MazamaSpatialUtils")
#'   MazamaSpatialUtils::setSpatialDataDir('~/Data/Spatial')
#'   MazamaSpatialUtils::loadSpatialData('NaturalEarthAdm1')
#' }
#' 
#' This function should be run before using \code{pas_load()}, as 
#' \code{pas_load()} uses the spatial data loaded by 
#' \code{initializeMazamaSpatialUtils()} to enhance  raw synoptic data via 
#' \code{enhanceSynopticData()}.
#' 
#' If file logging is desired, these commands should be run individually with
#' output log files specified as arguments to \code{logger.setup()} from the
#' \pkg{MazamaCoreUtils} package.
#' 
#' @param spatialDataDir directory where spatial datasets are created
#' @param stateCodeDataset MazamaSpatialUtils dataset returning ISO 3166-2 
#' alpha-2 stateCodes
#' 
#' @param logLevel logging level used

initializeMazamaSpatialUtils <- function(
  spatialDataDir = '~/Data/Spatial',
  stateCodeDataset = 'NaturalEarthAdm1',
  logLevel = WARN
) {
  
  logger.setup()
  logger.setLevel(logLevel)
  data("SimpleCountriesEEZ", package = "MazamaSpatialUtils")
  data("SimpleTimezones", package = "MazamaSpatialUtils")
  MazamaSpatialUtils::setSpatialDataDir(spatialDataDir)
  MazamaSpatialUtils::loadSpatialData(stateCodeDataset)
  
}
