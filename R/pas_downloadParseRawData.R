#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.error logger.debug
#' 
#' @title Download synoptic data from PurpleAir
#' 
#' @param baseUrl base URL for synoptic data
#' @return Dataframe of synoptic PurpleAir data.
#' @description Download and parse synoptic data from the Purple Air network
#' of particulate sensors.
#'
#' The synoptic data provides a view of the entire Purple Air network and
#' includes both metadata and recent PM2.5 averages for each deployed sensor.
#' 
#' @references \href{https://www.purpleair.com/json?all=true}{json formatted PurpleAir data}
#' @seealso \link{pas_enhanceData}
#' @examples
#' \donttest{
#' library(AirSensor)
#' 
#' initializeMazamaSpatialUtils()
#' 
#' pas_raw <- pas_downloadParseRawData()
#' 
#' if ( interactive() ) {
#'   View(pas_raw[1:100,])
#' }
#' }

pas_downloadParseRawData <- function(
  baseUrl = "https://www.purpleair.com/json?all=true"
) {

  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(baseUrl)
  
  # ----- Download raw data ----------------------------------------------------
  
  # Strip off any final "/"
  baseUrl <- stringr::str_replace(baseUrl,"/$","")

  # Placeholder in case things get more complicated
  webserviceUrl <- baseUrl

  # NOTE:  using Hadley Wickham style: 
  # NOTE:  https://github.com/hadley/httr/blob/master/vignettes/quickstart.Rmd
  r <- httr::GET(webserviceUrl)
  
  # Handle the response
  status_code <- httr::status_code(r)
  content <- httr::content(r, as="text", encoding="UTF-8") # don't interpret

  if ( httr::http_error(r) ) {  # web service failed to respond
    
    # TODO:  Find a package with web servivce status codes
    
    # https://digitalocean.com/community/tutorials/how-to-troubleshoot-common-http-error-codes
    if ( httr::status_code(r) == 429 ) {
      err_msg <- paste0("web service error 429 from ", webserviceUrl, 
                        ": Too Many Requests")
    } else if ( httr::status_code(r) == 500 ) {
      err_msg <- paste0("web service error 500 from ", webserviceUrl, 
                        ": Internal Server Error")
    } else if ( httr::status_code(r) == 502 ) {
      err_msg <- paste0("web service error 502", webserviceUrl, 
                        ": Bad Gateway")
    } else if ( httr::status_code(r) == 503 ) {
      err_msg <- paste0("web service error 503", webserviceUrl, 
                        ": Service Unavailable")
    } else if ( httr::status_code(r) == 504 ) {
      err_msg <- paste0("web service error 504 from ", webserviceUrl, 
                        ": Gateway Timeout from ", 
                        webserviceUrl)
    } else {
      err_msg <- paste0("web service error ", httr::status_code(r), " from ", 
                        webserviceUrl)
    }
    
    logger.error("Web service failed to respond: %s", webserviceUrl)
    logger.error(err_msg)
    stop(err_msg, call.=TRUE)
    
  }
  
  # Convert JSON to an R list
  PAList <- jsonlite::fromJSON(content,
                               simplifyVector = TRUE,
                               simplifyDataFrame = TRUE,
                               simplifyMatrix = TRUE,
                               flatten = FALSE)

  # > names(PAList)
  # [1] "mapVersion"       "baseVersion"      "mapVersionString" "results"

  logger.debug("mapVersion = \"%s\"", PAList$mapVersion)
  logger.debug("baseVersion = \"%s\"", PAList$baseVersion)
  logger.debug("mapVersionString = \"%s\"", PAList$mapVersionString)
  
  # Pull out the dataframe of results
  resultsDF <- PAList$results
  
  # > dplyr::glimpse(resultsDF, width = 80)
  # Rows: 21,431
  # Columns: 23
  # $ ID                               <int> 14633, 14634, 25999, 26000, 14091, 1…
  # $ Label                            <chr> " Hazelwood canary ", " Hazelwood ca…
  # $ DEVICE_LOCATIONTYPE              <chr> "outside", NA, "outside", NA, "outsi…
  # $ THINGSPEAK_PRIMARY_ID            <chr> "559921", "559923", "694803", "69480…
  # $ THINGSPEAK_PRIMARY_ID_READ_KEY   <chr> "CU4BQZZ38WO5UJ4C", "DULWDNCI9M6PCIP…
  # $ THINGSPEAK_SECONDARY_ID          <chr> "559922", "559924", "694804", "69480…
  # $ THINGSPEAK_SECONDARY_ID_READ_KEY <chr> "D0YNZ1LM59LL49VQ", "EY2CNMYRUZHDW1A…
  # $ Lat                              <dbl> 37.27556, 37.27556, 30.05381, 30.053…
  # $ Lon                              <dbl> -121.964134, -121.964134, -95.494643…
  # $ PM2_5Value                       <chr> "38.47", "35.41", "18.87", "16.84", …
  # $ LastSeen                         <int> 1600185772, 1600185772, 1600185808, …
  # $ Type                             <chr> "PMS5003+PMS5003+BME280", NA, "PMS50…
  # $ Hidden                           <chr> "false", "false", "false", "false", …
  # $ isOwner                          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
  # $ humidity                         <chr> "50", NA, "49", NA, "52", NA, "70", …
  # $ temp_f                           <chr> "73", NA, "98", NA, "72", NA, "69", …
  # $ pressure                         <chr> "1011.9", NA, "1012.91", NA, "1006.1…
  # $ AGE                              <int> 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, …
  # $ Stats                            <chr> "{\"v\":38.47,\"v1\":38.76,\"v2\":39…
  # $ ParentID                         <int> NA, 14633, NA, 25999, NA, 14091, NA,…
  # $ Flag                             <int> NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, NA…
  # $ A_H                              <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, …
  # $ Ozone1                           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, …
  
  # ----- BEGIN convert results$Stats ------------------------------------------

  # NOTE:  Stats for current, 10 min, 30 min, 1 hr, 6 hr, 1 day, 1 week are 
  # NOTE:  stored in df$Stats

  # NOTE:  Some Stats are NA and we need to fill in a blank stats array for those
  emptyStatsList <- list(
    v = -999.9,
    v1 = -999.9,
    v2 = -999.9,
    v3 = -999.9,
    v4 = -999.9,
    v5 = -999.9,
    v6 = -999.9,
    pm = -999.9,
    lastModified = -999.9,
    timeSinceModified = -999 # int
  )

  emptyStatsJSON <- jsonlite::toJSON(emptyStatsList, 
                                     auto_unbox = TRUE, 
                                     null = "null", 
                                     na = "null", 
                                     pretty = FALSE)

  # Add empty JSON string where needed
  missingStatsMask <- is.na(resultsDF$Stats)
  resultsDF$Stats[missingStatsMask] <- emptyStatsJSON
  
  # NOTE:  This was generated on 2020-09-15
  # NOTE:  Aversion from July had an additional "VoC" column so it seems that
  # NOTE:  this json file is not an entirely static "AIP".
  
  # > dplyr::glimpse(resultsDF, width = 75)
  # Rows: 21,431
  # Columns: 23
  # $ ID                               <int> 14633, 14634, 25999, 26000, 140…
  # $ Label                            <chr> " Hazelwood canary ", " Hazelwo…
  # $ DEVICE_LOCATIONTYPE              <chr> "outside", NA, "outside", NA, "…
  # $ THINGSPEAK_PRIMARY_ID            <chr> "559921", "559923", "694803", "…
  # $ THINGSPEAK_PRIMARY_ID_READ_KEY   <chr> "CU4BQZZ38WO5UJ4C", "DULWDNCI9M…
  # $ THINGSPEAK_SECONDARY_ID          <chr> "559922", "559924", "694804", "…
  # $ THINGSPEAK_SECONDARY_ID_READ_KEY <chr> "D0YNZ1LM59LL49VQ", "EY2CNMYRUZ…
  # $ Lat                              <dbl> 37.27556, 37.27556, 30.05381, 3…
  # $ Lon                              <dbl> -121.964134, -121.964134, -95.4…
  # $ PM2_5Value                       <chr> "38.47", "35.41", "18.87", "16.…
  # $ LastSeen                         <int> 1600185772, 1600185772, 1600185…
  # $ Type                             <chr> "PMS5003+PMS5003+BME280", NA, "…
  # $ Hidden                           <chr> "false", "false", "false", "fal…
  # $ isOwner                          <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
  # $ humidity                         <chr> "50", NA, "49", NA, "52", NA, "…
  # $ temp_f                           <chr> "73", NA, "98", NA, "72", NA, "…
  # $ pressure                         <chr> "1011.9", NA, "1012.91", NA, "1…
  # $ AGE                              <int> 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1…
  # $ Stats                            <chr> "{\"v\":38.47,\"v1\":38.76,\"v2…
  # $ ParentID                         <int> NA, 14633, NA, 25999, NA, 14091…
  # $ Flag                             <int> NA, NA, NA, NA, 1, 1, 1, 1, 1, …
  # $ A_H                              <chr> NA, NA, NA, NA, NA, NA, NA, NA,…
  # $ Ozone1                           <chr> NA, NA, NA, NA, NA, NA, NA, NA,…
  
  # Convert to tibble and guarantee character data types for IDs
  resultsTbl <- 
    dplyr::as_tibble(resultsDF) %>%
    dplyr::mutate(
      ID = as.character(.data$ID),
      THINGSPEAK_PRIMARY_ID = as.character(.data$THINGSPEAK_PRIMARY_ID),
      THINGSPEAK_SECONDARY_ID = as.character(.data$THINGSPEAK_SECONDARY_ID),
      ParentID = as.character(.data$ParentID)
    )

  statsList <- lapply(resultsDF$Stats, function(x) { jsonlite::fromJSON(x) } )

  # NOTE:  At this point we have a statsList where every element is a list.
  # NOTE:  Some Stats are missing 'pm', 'lastModified' and 'timeSinceModified'
  # NOTE:  but bind_rows() will take care of this by filling in those columns
  # NOTE:  with NA.

  statsTbl <- dplyr::bind_rows(statsList)

  # Now convert -999.9 and -999 back to NA
  missingMask <- statsTbl <= -999
  statsTbl[missingMask] <- as.numeric(NA)

  # ----- END convert results$Stats --------------------------------------------

  # Now create a new dataframe using the important columns from results and stats

  # > sort(names(resultsDF))
  # [1] "A_H"                              "AGE"                             
  # [3] "DEVICE_LOCATIONTYPE"              "Flag"                            
  # [5] "Hidden"                           "humidity"                        
  # [7] "ID"                               "isOwner"                         
  # [9] "Label"                            "LastSeen"                        
  # [11] "Lat"                              "Lon"                             
  # [13] "Ozone1"                           "ParentID"                        
  # [15] "PM2_5Value"                       "pressure"                        
  # [17] "Stats"                            "temp_f"                          
  # [19] "THINGSPEAK_PRIMARY_ID"            "THINGSPEAK_PRIMARY_ID_READ_KEY"  
  # [21] "THINGSPEAK_SECONDARY_ID"          "THINGSPEAK_SECONDARY_ID_READ_KEY"
  
  # [23] "Type"                            
  # > names(statsTbl)
  # [1] "v"                 "v1"                "v2"                "v3"               
  # [5] "v4"                "v5"                "v6"                "pm"               
  # [9] "lastModified"      "timeSinceModified"
  
  tbl <- dplyr::bind_cols(resultsTbl, statsTbl)
  tbl$Stats <- NULL

  # ----- Return ---------------------------------------------------------------
  
  return(tbl)

}
