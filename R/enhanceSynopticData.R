#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn
#' @importFrom rlang .data
#' 
#' @title Enhance synoptic data from Purple Air
#' 
#' @description Enhance raw synoptic data from PurpleAir to create a generally 
#' useful dataframe.
#'
#' Steps include:
#'
#' 1) Replace variable with more consistent, more human readable names.
#'
#' 2) Add spatial metadata for each monitor including:
#' \itemize{
#'   \item{timezone -- olson timezone}
#'   \item{countryCode -- ISO 3166-1 alpha-2}
#'   \item{stateCode -- ISO 3166-2 alpha-2}
#'   \item{airDistrict -- CARB air districts}
#' }
#'
#' 3) Convert data types from character to \code{POSIXct} and \code{numeric}.
#'
#' 4) Add distance and monitorID for the two closest PWFSL monitors
#' 
#' 5) Add additional metadata items:
#' \itemize{
#' \item{sensorManufacturer = "Purple Air"}
#' \item{targetPollutant = "PM"}
#' \item{technologyType = "consumer-grade"}
#' \item{communityRegion -- (where known)}
#' }
#'
#' Subsetting by country may be performed by specifying the \code{countryCodes} 
#' argument.
#'
#' Setting \code{outsideOnly = TRUE} will return only those records marked as 
#' 'outside'.
#' @note For data obtained on July 28, 2018 this will result in removal of all 
#' 'B' channels, even those whose parent 'A' channel is marked as 'outside'. 
#' This is useful if you want a quick, synoptic view of the network, e.g. for a 
#' map.
#' 
#' @param pas_raw Dataframe returned by \code{downloadParseSynopticData()}.
#' @param countryCodes ISO country codes used to subset the data.
#' @param includePWFSL Logical specifying whether to calculate distances from 
#' PWFSL monitors.
#' 
#' @return Enhanced Dataframe of synoptic PurpleAir data.
#' 
#' @seealso \link{downloadParseSynopticData}
#' 
#' @examples
#' \dontrun{
#' initializeMazamaSpatialUtils()
#' pas_raw <- downloadParseSynopticData()
#' pas <- enhanceSynopticData(pas_raw)
#' }

enhanceSynopticData <- function(
  pas_raw = NULL,
  countryCodes = c('US'),
  includePWFSL = TRUE
) {
  
  logger.debug("----- enhanceSynopticData() -----")
  
  # ----- Validate Parameters --------------------------------------------------
  
  if ( !('data.frame' %in% class(pas_raw)) )
    stop("parameter 'pas_raw' parameter is not a dataframe")
  
  # Guarantee uppercase codes
  countryCodes <- toupper(countryCodes)
  if ( any(!(countryCodes %in% countrycode::codelist$iso2c)) ) 
    stop("parameter 'countryCodes' has values that are not recognized as ISO-2 country codes")
  
  if ( !is.logical(includePWFSL) )
    stop("parameter 'includePWFSL' is not a logical value")
  
  logger.debug("----- enhanceSynopticData() -----")
  
  # On 2019-09-05, the data columns look like this:
  #
  # > sort(names(pas_raw))
  # [1] "A_H"                              "AGE"                              "DEVICE_LOCATIONTYPE"             
  # [4] "Flag"                             "Hidden"                           "humidity"                        
  # [7] "ID"                               "isOwner"                          "Label"                           
  # [10] "lastModified"                     "LastSeen"                         "Lat"                             
  # [13] "Lon"                              "ParentID"                         "pm"                              
  # [16] "PM2_5Value"                       "pressure"                         "temp_f"                          
  # [19] "THINGSPEAK_PRIMARY_ID"            "THINGSPEAK_PRIMARY_ID_READ_KEY"   "THINGSPEAK_SECONDARY_ID"         
  # [22] "THINGSPEAK_SECONDARY_ID_READ_KEY" "timeSinceModified"                "Type"                            
  # [25] "v"                                "v1"                               "v2"                              
  # [28] "v3"                               "v4"                               "v5"                              
  # [31] "v6"                              
  
  
  # ----- Discard unwanted columns ---------------------------------------------
  
  # NOTE:  A previously existing "State" column has been removed. (2019-09-05)
  
  if ( "State" %in% names(pas_raw) ) {
    pas_raw$State <- NULL
  }
  
  if ( "pm" %in% names(pas_raw) ) {
    pas_raw$pm <- NULL
  }
  
  # ----- Rename columns -------------------------------------------------------
  
  # Rename some things to have consistent lowerCamelCase and better human names
  # based on the information in the document "Using Purple Air Data".
  pas <-
    pas_raw %>%
    dplyr::rename(
      parentID = .data$ParentID,
      label = .data$Label,
      latitude = .data$Lat,
      longitude = .data$Lon,
      pm25 = .data$PM2_5Value,
      lastSeenDate = .data$LastSeen,
      sensorType = .data$Type,
      flag_hidden = .data$Hidden,
      flag_highValue = .data$Flag,
      flag_attenuation_hardware = .data$A_H,
      temperature = .data$temp_f,
      age = .data$AGE,
      pm25_current = .data$v,
      pm25_10min = .data$v1,
      pm25_30min = .data$v2,
      pm25_1hr = .data$v3,
      pm25_6hr = .data$v4,
      pm25_1day = .data$v5,
      pm25_1week = .data$v6,
      statsLastModifiedDate = .data$lastModified,
      statsLastModifiedInterval = .data$timeSinceModified
    )
  
  # ----- Add spatial metadata -------------------------------------------------
  
  preValidationRows <- nrow(pas)
  
  # First, remove records with invalid locations
  pas <-
    pas %>%
    dplyr::filter( !is.na(.data$longitude) & !is.na(.data$latitude) ) %>%
    dplyr::filter( .data$longitude >= -180 & .data$longitude <= 180 ) %>%
    dplyr::filter( .data$latitude >= -90 & .data$latitude <= 90 )
  
  badLocationCount <- preValidationRows - nrow(pas)
  
  if ( badLocationCount > 0 ) {
    logger.trace("%d records removed because of invalid location data.", 
                 badLocationCount)
  }
  
  # Suppress annoying 'deprecated' messages
  suppressWarnings({
    
    # Assign countryCodes and subset to countries of interest
    pas$countryCode <- MazamaSpatialUtils::getCountryCode(pas$longitude,
                                                          pas$latitude,
                                                          useBuffering = TRUE)
    pas <- subset(pas, pas$countryCode %in% countryCodes)
    
    # Assign stateCodes
    pas$stateCode <- MazamaSpatialUtils::getStateCode(pas$longitude,
                                                      pas$latitude,
                                                      countryCodes = countryCodes,
                                                      useBuffering = TRUE)
    
    # Assign timezones
    pas$timezone <- MazamaSpatialUtils::getTimezone(pas$longitude,
                                                    pas$latitude,
                                                    countryCodes = countryCodes,
                                                    useBuffering = TRUE)
    
    # CARB Air Districts
    result <- try({ 
      CA_AirBasins <- get(MazamaSpatialUtils::loadSpatialData("CA_AirBasins_01"))
    }, silent = TRUE)
    
    if ( "try-error" %in% class(result) ) {
      
      logger.warn("Unable to load spatial data `CA_AirBasins`.")
      
    } else {
      
      pas_CA <- 
        pas %>% 
        dplyr::filter(.data$countryCode == "US" & .data$stateCode == "CA")
      
      pas_CA$airDistrict <- 
        MazamaSpatialUtils::getSpatialData(
          pas_CA$longitude,
          pas_CA$latitude,
          CA_AirBasins,
          useBuffering = TRUE
        ) %>%
        pull("name")
      
      pas_nonCA <-  
        pas %>% 
        filter(.data$countryCode != "US" | .data$stateCode != "CA")
      
      pas_nonCA$airDistrict <- as.character(NA)
      
      pas <- bind_rows(pas_CA, pas_nonCA)
      
    }
    
  })
  
  # ----- Convert times to POSIXct ---------------------------------------------
  
  pas$lastSeenDate <- as.POSIXct(pas$lastSeenDate,
                                 tz = "UTC",
                                 origin = lubridate::origin)
  
  pas$statsLastModifiedDate <- as.POSIXct(pas$statsLastModifiedDate / 1000,
                                          tz = "UTC",
                                          origin = lubridate::origin)
  
  # ----- Convert to proper type -----------------------------------------------
  
  pas$ID <- as.character(pas$ID)
  pas$parentID <- as.character(pas$parentID)
  pas$pm25 <- as.numeric(pas$pm25)
  pas$pm25_current <- as.numeric(pas$pm25_current)
  pas$flag_hidden <- ifelse(pas$flag_hidden == 'true', TRUE, FALSE)
  pas$flag_highValue <- ifelse(pas$flag_highValue == 1, TRUE, FALSE)
  pas$flag_attenuation_hardware <- ifelse(pas$flag_attenuation_hardware == 'true', TRUE, FALSE)
  pas$temperature <- as.numeric(pas$temperature)
  pas$humidity <- as.numeric(pas$humidity)
  pas$pressure <- as.numeric(pas$pressure)
  
  # ----- Convert to internally standard units ---------------------------------
  
  pas$statsLastModifiedInterval <- pas$statsLastModifiedInterval / 1000   # seconds
  
  # ----- Round values to reflect resolution as specified in https://www.purpleair.com/sensors
  
  # TODO:  Figure out why rounding breaks outlier detection
  
  # pas$pm25 <- round(pas$pm25)
  # pas$pm25_current <- round(pas$pm25_current)
  # pas$temperature <- round(pas$temperature)
  # pas$humidity <- round(pas$humidity)
  # pas$pressure <- round(pas$pressure)
  
  # ----- Find nearby PWFSL monitors -------------------------------------------
  
  if ( includePWFSL ) {
    if ( !exists('pwfsl') ) (pwfsl <- PWFSLSmoke::loadLatest())
    
    pas$pwfsl_closestDistance <- as.numeric(NA)
    pas$pwfsl_closestMonitorID <- as.character(NA)
    
    for ( i in seq_len(nrow(pas)) ) {
      distances <- PWFSLSmoke::monitor_distance(pwfsl,
                                                pas$longitude[i],
                                                pas$latitude[i])
      minDistIndex <- which.min(distances)
      pas$pwfsl_closestDistance[i] <- distances[minDistIndex] * 1000 # To meters
      pas$pwfsl_closestMonitorID[i] <- names(distances[minDistIndex])
      
    }
    
  }

  # ----- Addditional metadata per SCAQMD request ------------------------------
  
  pas$sensorManufacturer <- "Purple Air"
  pas$targetPollutant <- "PM"
  pas$technologyType <- "consumer-grade"
  
  # ----- Add communityRegion --------------------------------------------------
  
  pas$communityRegion <- as.character(NA)
  label <- tolower(pas$label)
  
  # SCAQMD communities
  # NOTE:  Need to match "sctv_15 (dawson canyon) b"
  scah_mask <- stringr::str_detect(label, "^scah_[0-9][0-9]( ?.*$)")
  scan_mask <- stringr::str_detect(label, "^scan_[0-9][0-9]( ?.*$)")
  scap_mask <- stringr::str_detect(label, "^scap_[0-9][0-9]( ?.*$)")
  scbb_mask <- stringr::str_detect(label, "^scbb_[0-9][0-9]( ?.*$)")
  scem_mask <- stringr::str_detect(label, "^scem_[0-9][0-9]( ?.*$)")
  schs_mask <- stringr::str_detect(label, "^schs_[0-9][0-9]( ?.*$)")
  sciv_mask <- stringr::str_detect(label, "^sciv_[0-9][0-9]( ?.*$)")
  scnp_mask <- stringr::str_detect(label, "^scnp_[0-9][0-9]( ?.*$)")
  scpr_mask <- stringr::str_detect(label, "^scpr_[0-9][0-9]( ?.*$)")
  scsb_mask <- stringr::str_detect(label, "^scsb_[0-9][0-9]( ?.*$)")
  scsc_mask <- stringr::str_detect(label, "^scsc_[0-9][0-9]( ?.*$)")
  scsg_mask <- stringr::str_detect(label, "^scsg_[0-9][0-9]( ?.*$)")
  scsh_mask <- stringr::str_detect(label, "^scsh_[0-9][0-9]( ?.*$)")
  scsj_mask <- stringr::str_detect(label, "^scsj_[0-9][0-9]( ?.*$)")
  sctv_mask <- stringr::str_detect(label, "^sctv_[0-9][0-9]( ?.*$)")
  scuv_mask <- stringr::str_detect(label, "^scuv_[0-9][0-9]( ?.*$)")
  
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
  
  # ----- Return ---------------------------------------------------------------
  
  # Guarantee the class name still exists
  class(pas) <- c('pa_synoptic', class(pas))
  
  return(pas)
  
}
