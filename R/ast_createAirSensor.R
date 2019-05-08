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
#' ast <- pat_createASTimeseriesObject(pat, "1 hour")
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
    dplyr::rename(monitorID = .data$ID) %>%
    as.data.frame()
  
  meta$elevation <- as.numeric(NA)
  
  as_object <- list(meta = meta, data = data)
  class(as_object) <- c("airsensor", "ws_monitor")
  
  return(as_object)
  
}
