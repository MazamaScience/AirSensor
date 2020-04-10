#' @keywords pas
#' @export
#' 
#' @title Test for correct structure in a \emph{pa_synoptic} object
#' 
#' @param pas A \emph{pa_synoptic} object.
#' 
#' @return \code{TRUE} if \code{pas} has the correct structure, \code{FALSE} otherwise.
#' 
#' @description The \code{pas} is checked for the "pas" class name
#' and presence of \strong{all} metadata columns:
#' 
#' \preformatted{
#' "ID",                               "label",
#' "DEVICE_LOCATIONTYPE",              "THINGSPEAK_PRIMARY_ID",
#' "THINGSPEAK_PRIMARY_ID_READ_KEY",   "THINGSPEAK_SECONDARY_ID",
#' "THINGSPEAK_SECONDARY_ID_READ_KEY", "latitude",
#' "longitude",                        "pm25",
#' "lastSeenDate",                     "sensorType",
#' "flag_hidden",                      "isOwner",
#' "humidity",                         "temperature",
#' "pressure",                         "age",
#' "parentID",                         "flag_highValue",
#' "flag_attenuation_hardware",        "Voc",
#' "Ozone1",                           "pm25_current",
#' "pm25_10min",                       "pm25_30min",
#' "pm25_1hr",                         "pm25_6hr",
#' "pm25_1day",                        "pm25_1week",
#' "statsLastModifiedDate",            "statsLastModifiedInterval",
#' "deviceID",                         "locationID",
#' "deviceDeploymentID",               "countryCode",
#' "stateCode",                        "timezone",
#' "airDistrict",                      "pwfsl_closestDistance",
#' "pwfsl_closestMonitorID",           "sensorManufacturer",
#' "targetPollutant",                  "technologyType",
#' "communityRegion"
#' }
#' 
#' The \code{pwfsl_}, "official", monitors are obtained from the USFS AirFire 
#' site using the \pkg{PWFSLSmoke} R package.
#' 
#' @examples
#' pas_isPas(example_pas)
#' pas_isPas(1:10)

pas_isPas <- function(
  pas = NULL
) {
  
  # Test a variety of things that could go wrong
  if ( is.null(pas) ) return(FALSE)
  if ( !"pa_synoptic" %in% class(pas) ) return(FALSE)
  
  parameters <- c(
    "ID",                               "label",                            
    "DEVICE_LOCATIONTYPE",              "THINGSPEAK_PRIMARY_ID",           
    "THINGSPEAK_PRIMARY_ID_READ_KEY",   "THINGSPEAK_SECONDARY_ID",          
    "THINGSPEAK_SECONDARY_ID_READ_KEY", "latitude",                        
    "longitude",                        "pm25",                             
    "lastSeenDate",                     "sensorType",                      
    "flag_hidden",                      "isOwner",                          
    "humidity",                         "temperature",                     
    "pressure",                         "age",                              
    "parentID",                         "flag_highValue",                  
    "flag_attenuation_hardware",        "Voc",                              
    "Ozone1",                           "pm25_current",                   
    "pm25_10min",                       "pm25_30min",                       
    "pm25_1hr",                         "pm25_6hr",                        
    "pm25_1day",                        "pm25_1week",                       
    "statsLastModifiedDate",            "statsLastModifiedInterval",       
    "deviceID",                         "locationID",                       
    "deviceDeploymentID",               "countryCode",                     
    "stateCode",                        "timezone",                         
    "airDistrict",                      "pwfsl_closestDistance",           
    "pwfsl_closestMonitorID",           "sensorManufacturer",               
    "targetPollutant",                  "technologyType",                  
    "communityRegion"
  )
  
  if ( !all(parameters %in% names(pas)) ) return(FALSE)
  
  # Nothing failed so return TRUE
  return(TRUE)
  
}


#' @export
#' 
#' @title Test for an empty \emph{pa_synoptic} object
#' 
#' @param pas A \emph{pa_synoptic} object.
#' 
#' @return \code{TRUE} if no data exist in \code{pas}, \code{FALSE} otherwise.
#' 
#' @description Convenience function for \code{nrow(pas) == 0}.
#' This makes for more readable code in functions that need to test for this.
#' 
#' @examples
#' pas <- example_pas
#' pas_isEmpty(pas)
#' pas <- pas %>% pas_filter(ID < 0)
#' pas_isEmpty(pas)

pas_isEmpty <- function(pas) {
  if (!pas_isPas(pas)) stop("Not a valid 'pas' object.")
  return( nrow(pas) == 0 )
}
