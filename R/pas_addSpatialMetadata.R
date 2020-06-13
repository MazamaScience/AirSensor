#' @importFrom MazamaCoreUtils logger.isInitialized logger.trace logger.warn logger.error logger.fatal
#' @importFrom rlang .data
#'
#' @export
#'
#' @title Add Spatial Metadata to PurpleAIr Synoptic Data
#'
#' @description Adds spatial metadata to a pa_synoptic object via the 
#' \code{MazamaSpatialUtils} Package using PurpleAir location coordinates to 
#' determine country, state, and timezone.  
#'
#' @param pas PurpleAir Synoptic \emph{pas} object. 
#' @param countryCodes (optional) ISO country codes used to subset the data.
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
#'   pas_addSpatialMetadata()
#' }

pas_addSpatialMetadata <- function(
  pas = NULL, 
  countryCodes = NULL
) {
  
  # TODO: Implement solid error handling, side-stepping the use of pas_isPas or 
  # TODO: pas_isEmpty, as these require certain column vectors/names that may 
  # TODO: not have been added to the (yet to be) pa_synoptic dataframe. Should order matter? 
  
  MazamaCoreUtils::stopIfNull(pas)
  
  # Ensure coordinate names are explicitly `longitude` and `latitude` 
  colnames(pas)[stringr::str_which(names(pas), '^[lL]on')] <- 'longitude'
  colnames(pas)[stringr::str_which(names(pas), '^[lL]at')] <- 'latitude'
  
  # Count rows to preform logging validation 
  preValidationRows <- nrow(pas)
  
  # First, remove records with invalid locations
  pas <-
    pas %>%
    dplyr::filter( !is.na(.data$longitude) & !is.na(.data$latitude) ) %>%
    dplyr::filter( .data$longitude >= -180 & .data$longitude <= 180 ) %>%
    dplyr::filter( .data$latitude >= -90 & .data$latitude <= 90 )
  
  badLocationCount <- preValidationRows - nrow(pas)
  
  if ( badLocationCount > 0 ) {
    if ( logger.isInitialized() ) {
      logger.trace("%d records removed because of invalid location data.", 
                   badLocationCount)
    }
  }
  
  if ( logger.isInitialized() )
    logger.trace("Adding spatial metadata")
  
  # TODO:  Could optimize spatial data assignment by only calculating spatial
  # TODO:  data for the A channel and then copying that info to the B channel.
  # TODO:  This will result in more complex code but would shave a few seconds
  # TODO:  off of this step if that becomes critical.
  
  # Assign countryCodes
  result <- try({
    pas$countryCode <- MazamaSpatialUtils::getCountryCode(pas$longitude,
                                                          pas$latitude,
                                                          useBuffering = TRUE)
  }, silent = TRUE)
  
  if ( "try-error" %in% class(result) ) {
    err_msg <- geterrmessage()
    if ( logger.isInitialized() ) {
      logger.error(err_msg)
      logger.fatal("Unable to assign countryCodes.")
    }
    stop(paste0("Unable to assign countryCodes: ", err_msg))
  }
  
  # Suppress annoying 'deprecated' messages
  suppressWarnings({
    
    if ( !is.null(countryCodes) ) {
      
      # Guarantee uppercase codes
      countryCodes <- toupper(countryCodes)
      
      # Validate countryCodes
      if ( any(!(countryCodes %in% countrycode::codelist$iso2c)) ) 
        stop("parameter 'countryCodes' has values that are not recognized as ISO-2 country codes")
      
      #  Subset to countries of interest if country code provided
      pas <- subset(pas, pas$countryCode %in% countryCodes)
      
    }
    
    # Assign stateCodes
    # NOTE: Using the countryCodes added from logic above
    pas$stateCode <- 
      MazamaSpatialUtils::getStateCode(
        pas$longitude,
        pas$latitude,
        countryCodes = pas$countryCode,
        useBuffering = TRUE
      )
    
    # Assign timezones
    pas$timezone <-
      MazamaSpatialUtils::getTimezone(
        pas$longitude,
        pas$latitude,
        countryCodes = pas$countryCode,
        useBuffering = TRUE
      )
    
  })
  
  return(pas)
}
