#' @export
#' @title Get Purple Air Timeseries data
#' @param pas Purple Air 'enhanced' synoptic data
#' @param name Purple Air 'label'
#' @param id Purple Air 'ID'
#' @param startdate desired start datetime (ISO 8601)
#' @param enddate desired end datetime (ISO 8601)
#' @param baseURL Base URL for Thingspeak API
#' @return List with \code{meta} and \code{data} elements
#' @description Timeseries data doe specific PurpleAie sensor are retrieved from the Thingspeak API.
#'
#' @seealso \link{downloadParseTimeseriesData}
#' @seealso \link{createPATimeseriesObject}
#' @examples
#' \dontrun{
#' initializeMazamaSpatialUtils()
#' pas <- pas_load()
#' weaverville <- pat_load(pas, 'CARB_SMOKE_NCUAQMD_WEAVERVILLE_70')
#' }

pat_load <- function(pas = NULL,
                     name = NULL,
                     id = NULL,
                     startdate = NULL,
                     enddate = NULL,
                     baseURL = "https://api.thingspeak.com/channels/") {
  
  # Download, parse and restructure timeseries data
  
  # Only one week of data can be loaded at a time. if over one week has been 
  # requested, loop over weeks to download all of it
  if ( !is.null(startdate) && !is.null(enddate) ) {
    sd <- parseDatetime(startdate)
    ed <- parseDatetime(enddate)
    dateSeq <- seq(sd, ed, by = lubridate::ddays(7)) 
    if (ed > tail(dateSeq, 1)){
      dateSeq <- c(dateSeq, ed)
    }
  } else {
    dateSeq <- NULL
  }
    
  pat_raw <- downloadParseTimeseriesData(pas, name, id, dateSeq[1], dateSeq[2], baseURL)
  if (length(dateSeq) > 2) {
    for ( i in 2:(length(dateSeq)-1) ) {
      new_pat_raw <- downloadParseTimeseriesData(pas, name, id, dateSeq[i], dateSeq[i+1], baseURL)
      pat_raw$data <- bind_rows(pat_raw$data, new_pat_raw$data)
    }
  }
    
  pat <- createPATimeseriesObject(pat_raw)
  
  return(pat)
  
}
