#' @export
#' @title Download Sensor list from PurpleAir
#' @importFrom MazamaCoreUtils logger.debug logger.error
#' @param baseUrl base URL for Sensor archive
#' @description Download a complete list of the PurpleAir sensor archive. 
#' Thoeretically includes deprectaed/inactive sensors. The list can be manually
#' accessed via web browser provided in the reference. 
#' 
#' @return A data.frame of PurpleAir sensors (inactive or active)
#' @references \href{https://www.purpleair.com/sensorlist}{PurpleAir Sensor List}
#' 
#' @seealso \link{downloadParseSynopticData}
#' @examples
#' \dontrun{
#' initializeMazamaSpatialUtils()
#' sensor_list <- downloadParseSensorList()
#' }

downloadParseSensorList <- 
  function(
    baseUrl = "https://www.purpleair.com/sensorlist"
  ) {
    
    logger.debug("----- downloadParseSensorList() -----")
    
    # Placeholder in case things get more complicated
    webserviceUrl <- baseUrl
    
    # NOTE:  using Hadley Wickham style: 
    # NOTE:  https://github.com/hadley/httr/blob/master/vignettes/quickstart.Rmd
    resp <- httr::GET(webserviceUrl)
    
    # Handle the response
    status_code <- httr::status_code(resp)
    content <- httr::content(resp, as="text", encoding="UTF-8") # don't interpret
    
    if ( httr::http_error(resp) ) {  # web service failed to respond
      
      # https://digitalocean.com/community/tutorials/how-to-troubleshoot-common-http-error-codes
      if ( httr::status_code(resp) == 500 ) {
        err_msg <- paste0("web service error 500: Internal Server Error from ",
                          webserviceUrl)
      } else if ( httr::status_code(resp) == 502 ) {
        err_msg <- paste0("web service error 502: Bad Gateway from ", 
                          webserviceUrl)
      } else if ( httr::status_code(resp) == 503 ) {
        err_msg <- paste0("web service error 503: Service Unavailable from ", 
                          webserviceUrl)
      } else if ( httr::status_code(resp) == 504 ) {
        err_msg <- paste0("web service error 504: Gateway Timeout from ", 
                          webserviceUrl)
      } else {
        err_msg <- paste0('web service error ', 
                          httr::status_code(resp), 
                          " from ", 
                          webserviceUrl)
      }
      
      logger.error("Web service failed to respond: %s", webserviceUrl)
      logger.error(err_msg)
      stop(err_msg, call.=TRUE)
      
    }
    
    # Read the page 
    lines <- readr::read_lines(webserviceUrl)
    
    # Find download button - contains all necessary info
    downloadMask <- 
      which(stringr::str_detect(lines, "download_button"))
    
    # Remove rubbish
    downloadLines <- 
      stringr::str_replace_all(lines[downloadMask], "&#39;|'", "")
    
    # Regex pattern to match download string
    pattern1 <- ".*(download\\(.+\\)).*(download\\(.+\\)).*"
    
    parts <- unlist( stringr::str_match(downloadLines, pattern1) )
    
    # Useful parts 
    dl1 <- parts[,2]
    dl2 <- parts[,3]
    
    # Regex pattern to extract useful data from raw string
    pattern2 <- 
      "download\\(.+,.+,\\s?(.+)\\s?\\((.+) (.+)\\),(.+),\\s?(.+)\\s?,.+,.+,.+,.+\\)"
    
    # Bind matching strings into data.frame
    df <- 
      rbind(
        stringr::str_match(dl1, pattern2), 
        stringr::str_match(dl2, pattern2)
      )[,-1]
    
    # Rename columns
    colnames(df) <- c("label","latitude","longitude","ID","THINGSPEAK_KEY")
    
    return(df)
    
  }
