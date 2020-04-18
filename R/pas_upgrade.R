#' @keywords pas
#' @export
#' 
#' @title Upgrade \emph{pa_synoptic} object format
#' 
#' @param pas A \emph{pa_synoptic} object.
#' @param verbose (logical) Display upgrade messages.
#' 
#' @return \code{TRUE} if \code{pas} has the correct structure, \code{FALSE} otherwise.
#' 
#' @description The \code{pas} is checked for the latest pa_synoptic format
#' and presence of core metadata columns:
#' \itemize{
#'   \item{ID -- Purple Air ID}
#'   \item{label -- location label}
#'   \item{DEVICE_LOCATIONTYPE -- location descriptor}
#'   \item{THINGSPEAK_PRIMARY_ID -- Thingspeak API access ID}
#'   \item{THINGSPEAK_PRIMARY_ID_READ_KEY -- Thingspeak API access key}
#'   \item{THINGSPEAK_SECONDARY_ID -- Thingspeak API access ID}
#'   \item{THINGSPEAK_SECONDARY_ID_READ_KEY -- Thingspeak API access key}
#'   \item{longitude -- decimal degrees E}
#'   \item{latitude -- decimal degrees N}
#'   \item{pm25 -- latest PM25}
#'   \item{lasteSeenDate -- last update datetime}
#'   \item{sensorType -- PurpleAir sensor type}
#'   \item{flag_hidden -- hidden flag}
#'   \item{isOwner -- owner logical}
#'   \item{humidity -- \%}
#'   \item{temperature -- deg F}
#'   \item{pressure -- mb}
#'   \item{age -- sensor age}
#'   \item{parentID -- device parent ID}
#'   \item{timezone -- Olson timezone}
#'   \item{flag_highValue -- out of spec flag}
#'   \item{flag_attenutation_hardware -- hardware failure flag}
#'   \item{Ozone1 -- latest ozone data}
#'   \item{Voc -- latest voc data}
#'   \item{pm25_current -- current PM2.5 data}
#'   \item{pm25_10min -- 10-minute average PM2.5 data}
#'   \item{pm25_30min -- 30-minute average PM2.5 data}
#'   \item{pm25_1hr -- 1-hour average PM2.5 data}
#'   \item{pm25_6hr -- 6-hour average PM2.5 data}
#'   \item{pm25_1day -- 1-day PM2.5 average data}
#'   \item{pm25_1week -- 1-week PM2.5 average data}
#'   \item{statsLastModifiedDate -- last modified date}
#'   \item{statsLastModifiedInterval -- interval between modified date}
#'   \item{deviceID -- unique device identifier}
#'   \item{locationID -- generated location ID}
#'   \item{deviveDeploymentID -- generated unique ID}
#'   \item{countryCode -- ISO 3166-1 alpha-2}
#'   \item{stateCode -- ISO 3166-2 alpha-2}
#'   \item{timezone -- location timezone}
#'   \item{airDistrict -- Air district, if any}
#'   \item{pwfsl_closestDistance -- nearest regulatory monitor distance, meters}
#'   \item{pwfsl_closestMonitorID -- nearest regularoty monitor ID}
#'   \item{sensorManufacturer -- hardware manufacturer}
#'   \item{targetPollutant -- target pollutant data}
#'   \item{technologyType -- type of sensor technology}
#'   \item{communityRegion -- defined regional community.}
#' }
#' 
#' @examples
#' \donttest{
#' # Use outdated pa_synoptic database
#' setArchiveBaseUrl('http://smoke.mazamascience.com/data/PurpleAir')
#' # Initialize the required spatial utilities
#' initializeMazamaSpatialUtils()
#' pas <- 
#'   pas_load() %>%
#'   pas_upgrade()
#' } 

pas_upgrade <- function(
  pas = NULL, 
  verbose = TRUE
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pas)
  MazamaCoreUtils::stopIfNull(verbose)
  
  # Check if Mazama Spatial Utilites has been initialized 
  if ( !as.logical(Sys.getenv("SPATIAL_IS_INITIALIZED")) || 
       Sys.getenv("SPATIAL_IS_INITIALIZED") == "" ) {
    stop("MazamaSpatialUtils is not initialized. 
         Please initialize with initializeMazamaSpatialUtils()")
  }
  
  # ----- pa_synoptic Upgrade --------------------------------------------------
  
  # NOTE:  Upgraded pa_synoptic data columns generated 2020-04-09
  # NOTE:  via `AirSensor::pas_createNew()`
  
  upgradedCols <- 
    c(
      "ID",                               "label",                            "DEVICE_LOCATIONTYPE",             
      "THINGSPEAK_PRIMARY_ID",            "THINGSPEAK_PRIMARY_ID_READ_KEY",   "THINGSPEAK_SECONDARY_ID",         
      "THINGSPEAK_SECONDARY_ID_READ_KEY", "latitude",                         "longitude",                       
      "pm25",                             "lastSeenDate",                     "sensorType",                      
      "flag_hidden",                      "isOwner",                          "humidity",                        
      "temperature",                      "pressure",                         "age",                             
      "parentID",                         "flag_highValue",                   "flag_attenuation_hardware",       
      "Ozone1",                           "Voc",                              "pm25_current",                    
      "pm25_10min",                       "pm25_30min",                       "pm25_1hr",                        
      "pm25_6hr",                         "pm25_1day",                        "pm25_1week",                      
      "statsLastModifiedDate",            "statsLastModifiedInterval",        "deviceID",                        
      "locationID",                       "deviceDeploymentID",               "countryCode",                     
      "stateCode",                        "timezone",                         "airDistrict",                     
      "pwfsl_closestDistance",            "pwfsl_closestMonitorID",           "sensorManufacturer",              
      "targetPollutant",                  "technologyType",                   "communityRegion"
    )
  
  
  if ( !all(upgradedCols %in% names(pas)) ) {
    # Does not contain all the columns -> upgrade
    
    # NOTE: This is where to add more 'intelligent' upgrading if necessary --
    # NOTE: which is essentially just "enhancing" but without an assumed 
    # NOTE: `pas_downloadParseData` structure.
    
    # Define the missing columns from the upgraded columns
    missingCols <- upgradedCols[!upgradedCols %in% names(pas)]
    
    # Add spatial metadata 
    if ( any(c('latitude', 'longitude', 'stateCode', 'countryCode', 'timezone') %in% missingCols) ) {
      if ( verbose ) {
        message('Adding spatial metadata...')
      }
      pas <- pas_addSpatialMetadata(pas)
    }

    # Add unique IDs
    if ( any(c('deviceID', 'locationID', 'deviceDeploymentID') %in% missingCols) ) {
      if ( verbose ) {
        message('Adding unique IDs...')
      }
      pas <- pas_addUniqueIDs(pas)
    }
    
    # Add air district
    if ( 'airDistrict' %in% missingCols ) {
      if ( verbose ) {
        message('Adding air district...')
      }
      pas <- pas_addAirDistrict(pas)
    }
    
    # Add community region 
    if ( 'communityRegion' %in% missingCols ) {
      if ( verbose ) {
        message('Adding community region...')
      }
      pas <- pas_addCommunityRegion(pas)
    }
    
    # Show user format has upgraded
    if ( verbose ) {
      message('pa_synoptic object format upgraded.')
    }
    
  } else {
    # Does contain all the columns -> skip. 
    
    # Show user format does not require upgrade.
    if ( verbose ) {
      message('pa_synoptic object does not require upgrade ... Skipping.')
    }
    
  }
  
  # Check for missing columns after "upgrading", add filled with NA if necessary
  missingColsCheck <- upgradedCols[!upgradedCols %in% names(pas)]
  for ( i in missingColsCheck ) {
    pas[[i]] <- NA
  }
  
  # NOTE: Above fixes vectorised missing data, whereas below fixes element-wise
  # NOTE: missing data. 
  # TODO: FIXME. Find out why errors are produced with dplyr 
  # Find missing entries
  # fixedMissingRecords <- 
  #   pas %>%  
  #   dplyr::filter(is.na(.data$stateCode) | is.na(.data$countryCode)) %>% 
  #   pas_addSpatialMetadata()
  # 
  # # Find non-missing entries
  # nonMissingRecords <- 
  #   pas %>% 
  #   dplyr::filter(!is.na(.data$stateCode) & !is.na(.data$countryCode))
  # 
  # # Combine the fixed with the valid
  # pas <- dplyr::bind_rows(nonMissingRecords,fixedMissingRecords)
  
  # Re-oder columns and remove any that are not valid
  pas <- pas[,upgradedCols]

  # ----- Post-upgrade validation ----------------------------------------------
  
  if ( !pas_isPas(pas) ) {
    stop('Error: pa_synoptic object failed to upgrade.')
  }
  if ( pas_isEmpty(pas) ) {
    stop("Required parameter 'pas' has no data.")
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(pas)
  
}


