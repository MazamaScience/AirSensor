#' @export
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
#' @references \href{https://www.purpleair.com/json}{json formatted PurpleAir data}
#' @seealso \link{enhanceSynopticData}
#' @examples
#' \dontrun{
#' initializeMazamaSpatialUtils()
#' pas_raw <- downloadParseSynopticData()
#' View(pas_raw[1:100,])
#' }

downloadParseSynopticData <- function(
  baseUrl = "https://www.purpleair.com/json"
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

  # convert to tibble
  resultsTbl <- dplyr::as.tbl(resultsDF)

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
  # [1] "A_H"                              "AGE"                              "DEVICE_LOCATIONTYPE"             
  # [4] "Flag"                             "Hidden"                           "humidity"                        
  # [7] "ID"                               "isOwner"                          "Label"                           
  # [10] "LastSeen"                         "Lat"                              "Lon"                             
  # [13] "Ozone1"                           "ParentID"                         "PM2_5Value"                      
  # [16] "pressure"                         "Stats"                            "temp_f"                          
  # [19] "THINGSPEAK_PRIMARY_ID"            "THINGSPEAK_PRIMARY_ID_READ_KEY"   "THINGSPEAK_SECONDARY_ID"         
  # [22] "THINGSPEAK_SECONDARY_ID_READ_KEY" "Type"                             "Voc"                             
  
  # > names(statsTbl)
  # [1] "v"                 "v1"                "v2"                "v3"                "v4"
  # [6] "v5"                "v6"                "pm"                "lastModified"      "timeSinceModified"

  tbl <- dplyr::bind_cols(resultsTbl, statsTbl)
  tbl$Stats <- NULL

  # ----- Return ---------------------------------------------------------------
  
  return(tbl)

}
