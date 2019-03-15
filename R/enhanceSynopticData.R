#' @export
#' @importFrom MazamaCoreUtils logger.debug
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
#' }
#'
#' 3) Convert data types from character to \code{POSIXct} and \code{numeric}.
#'
#' 4) Add distance and monitorID for the two closest PWFSL monitors
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
  
  # Start with pas_raw (but keep it around for comparison while debugging)
  pas <- pas_raw
  
  # > names(pas_raw)
  # [1] "ID"                               "ParentID"                         "Label"
  # [4] "DEVICE_LOCATIONTYPE"              "THINGSPEAK_PRIMARY_ID"            "THINGSPEAK_PRIMARY_ID_READ_KEY"
  # [7] "THINGSPEAK_SECONDARY_ID"          "THINGSPEAK_SECONDARY_ID_READ_KEY" "Lat"
  # [10] "Lon"                              "PM2_5Value"                       "LastSeen"
  # [13] "State"                            "Type"                             "Hidden"
  # [16] "Flag"                             "isOwner"                          "A_H"
  # [19] "temp_f"                           "humidity"                         "pressure"
  # [22] "AGE"                              "v"                                "v1"
  # [25] "v2"                               "v3"                               "v4"
  # [28] "v5"                               "v6"                               "pm"
  # [31] "lastModified"                     "timeSinceModified"
  
  # Rename some things based on the information in the document "Using Purple Air Data"
  newNames <- c(
    "ID", "parentID", "label",
    "DEVICE_LOCATIONTYPE", "THINGSPEAK_PRIMARY_ID", "THINGSPEAK_PRIMARY_ID_READ_KEY",
    "THINGSPEAK_SECONDARY_ID", "THINGSPEAK_SECONDARY_ID_READ_KEY", "latitude",
    "longitude", "pm25", "lastSeenDate",
    "unused_State", "sensorType" , "flag_hidden",
    "flag_highValue" , "isOwner", "flag_attenuation_hardware",
    "temperature", "humidity", "pressure",
    "age", "pm25_current", "pm25_10min",
    "pm25_30min", "pm25_1hr", "pm25_6hr",
    "pm25_1day", "pm25_1week", "unused_pm",
    "statsLastModifiedDate", "statsLastModifiedInterval"
  )
  
  names(pas) <- newNames
  pas$unused_State <- NULL
  pas$unused_pm <- NULL
  
  # ----- Add spatial metadata:  countryCode, stateCode and timezone -----------
  
  # NOTE:  Some setup is involved before you can use MazamaSpatialUtils
  
  # Suppress annoying 'deprecated' messages
  suppressWarnings({
    
    # Assign countryCodes and subset to countries of interest
    pas$countryCode <- MazamaSpatialUtils::getCountryCode(pas$longitude,
                                                          pas$latitude)
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
    
  })
  
  # ----- Convert times to POSIXct ---------------------------------------------
  
  pas$lastSeenDate <- as.POSIXct(pas$lastSeenDate,
                                 origin = lubridate::origin)
  
  pas$statsLastModifiedDate <- as.POSIXct(pas$statsLastModifiedDate / 1000,
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
  
  pas$statsLastModifiedInterval <- pas$statsLastModifiedInterval / 1000     # seconds
  
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
  
  # Guarantee the class name still exists
  class(pas) <- c('pa_synoptic', class(pas))
  
  return(pas)
  
}
