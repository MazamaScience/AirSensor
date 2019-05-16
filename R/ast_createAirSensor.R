#' @export
#' @importFrom rlang .data
#' 
#' @title Create a Air Sensor object
#' 
#' @param ast Air Sensor Timeseries object
#' @param parameter Which column of data from \code{ast} to use
#' @return List with \code{meta} and \code{data} elements on a uniform time 
#' axis.
#' 
#' @description Reformats an \code{ast} object into an "Air Sensor" object
#' that has appropriate metadata to be used with the *PWFSLSmoke* package.
#'
#' @return "as" list of aggregated time series PurpleAir data
#' 
#' @seealso \link{pat_createASTimeseries}
#' 
#' @examples 
#' \dontrun{
#' pat <- 
#'   AirSensor::example_pat %>%
#'   pat_filterDate(20180701, 20180901)
#' ast <- pat_createASTimeseries(pat, "1 hour")
#' as <- ast_createAirSensor(ast)
#' }

ast_createAirSensor <- function(
  ast = NULL,
  parameter = "pm25"
) {
  
  # ----- Validate Parameters --------------------------------------------------
  


  # ----- Create Air Sensor object  --------------------------------------------
  
  monitorID <- ast$meta$label
  
  data <- 
    ast$data %>%
    dplyr::select(.data$datetime, .data[[parameter]]) %>%
    as.data.frame()
  # TODO:  what is dplyr for:
  names(data) <- c("datetime", monitorID)
  
  # TODO: Determie what should be in meta
  meta <- 
    ast$meta %>% 
    dplyr::rename(monitorID = .data$label) %>%
    as.data.frame()
  
  # Add metadata found in PWFSLSmoke ws_monitor objects
  meta$elevation <- as.numeric(NA)
  meta$siteName <- meta$label
  meta$countyName <- as.character(NA)
  meta$msaName <- as.character(NA)
  meta$monitorType <- as.character(NA) # TODO -- we know this from the pas
  meta$siteID <- as.character(NA)
  meta$instrumentID <- as.character(NA)
  meta$aqsID <- as.character(NA)
  meta$pwfslID <- as.character(NA)
  meta$pwfslDataIngestSource <- as.character(NA)
  meta$telemetryAggregator <- as.character(NA)
  meta$telemetryUnitID <- as.character(NA)

  # NOTE:  As of 2019-05-14, the meta dataframe still has rownames
  rownames(meta) <- colnames(data)[-1]
  
  as_object <- list(meta = meta, data = data)
  class(as_object) <- c("airsensor", "ws_monitor")
  
  return(as_object)
  
}
