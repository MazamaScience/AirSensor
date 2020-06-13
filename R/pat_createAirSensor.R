#' @export
#' @importFrom rlang .data
#' 
#' @title Create an Air Sensor object
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param parameter Parameter for which to create an univariate \emph{airsensor} 
#' object. See details.
#' @param FUN Algorithm applied to \emph{pat} object for hourly aggregation and 
#' quality control. See details.
#' @param ... (optional) Additional parameters passed into \code{FUN}.
#'  
#' @description Converts data from a \emph{pat} object with an irregular time 
#' axis to an \emph{airsensor} object where the numeric data has been aggregated 
#' along a standardized hourly time axis, as well as adding additional required 
#' metadata for compatibility with the *PWFSLSmoke* package.
#'
#' @details 
#' \code{FUN} allows users to provide custom aggregation and 
#' quality-control functions that are used to create an \emph{airsensor} object. 
#' The \code{FUN} must accept a \emph{pat} object as the first argument and 
#' return a dataframe with a regular hourly datetime axis. \code{FUN} 
#' can access and utilize any component of a standard \emph{pat} object 
#' (e.g pm25_A, temperature, etc.) as well as define new variables in the 
#' \emph{pat} data. See examples. 
#' 
#' \code{parameter} allows user to select which variable to use for the 
#' univariate \emph{airsensor} object (e.g 'pm25_A', 'humidity', etc.). 
#' Furthermore the \code{parameter} can be a new variable created via \code{FUN} 
#' evaluation. See examples.
#' 
#' Additional named parameters can be be passed to \code{FUN} through \code{...}.  
#'
#' @return An "airsensor" object of aggregated PurpleAir Timeseries data.
#' 
#' @seealso \link{PurpleAirQC_hourly_AB_02}
#' @seealso \link{pat_aggregate}
#' 
#' @examples
#' \donttest{
#' library(AirSensor)
#' 
#' # Default FUN = PurpleAirQC_hourly_AB_00
#' sensor <- pat_createAirSensor(example_pat)
#' 
#' PWFSLSmoke::monitor_timeseriesPlot(sensor, shadedNight = TRUE)
#' 
#' # Try out other package QC functions
#' example_pat %>%
#'   pat_createAirSensor(FUN = PurpleAirQC_hourly_AB_01) %>%
#'   PWFSLSmoke::monitor_timeseriesPlot(shadedNight = TRUE)
#'   
#' example_pat %>%
#'   pat_createAirSensor(FUN = PurpleAirQC_hourly_AB_02) %>%
#'   PWFSLSmoke::monitor_timeseriesPlot(shadedNight = TRUE)
#'   
#' # Custom FUN
#' humidity_correction <- function(pat, z = 0) {
#' 
#'   # Default hourly aggregation
#'   hourlyData <- 
#'     pat %>%
#'     pat_aggregate() %>%
#'     pat_extractData()
#'     
#'   # Create custom_pm variable 
#'   pm25 <- (hourlyData$pm25_A + hourlyData$pm25_B) / 2
#'   hum <- hourlyData$humidity
#'   temp <- hourlyData$temperature
#'   hourlyData$custom_pm <- pm25 - (pm25 * hum * z)
#'     
#'   return(hourlyData)
#'   
#' } 
#' 
#' # Evaluate custom FUN 
#' sensor <- pat_createAirSensor(
#'   example_pat, 
#'   parameter = "custom_pm", 
#'   FUN = humidity_correction,
#'   z = .005
#' )
#'
#' PWFSLSmoke::monitor_timeseriesPlot(sensor, shadedNight = TRUE)
#' }
#' 

pat_createAirSensor <- function(
  pat = NULL,
  parameter = "pm25", 
  FUN = PurpleAirQC_hourly_AB_02,
  ...
) {
  
  # ----- Validate input -------------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  MazamaCoreUtils::stopIfNull(parameter)
  
  if ( !pat_isPat(pat) ) {
    stop("Required parameter 'pat' is not a valid 'pa_timeseries' object.")
  }
  
  if ( pat_isEmpty(pat) ) {
    stop("Required parameter 'pat' has no data.") 
  }
  
  if ( is.null(FUN) ) {
    FUN <- PurpleAirQC_hourly_AB_00
  } else {
    if ( !rlang::is_closure(FUN) ) {
      stop(paste0("Provided 'FUN' is not a function.",
                  "(Pass in the function with no quotes and no parentheses.)"))
    }
  }

  # ----- Prepare 'pat' --------------------------------------------------------
  
  # Check if deviceDeploymentID is in the meta data. If not, add uniqueIDs.
  # NOTE: This is necessary as of 2020-04-20 to avoid errors with deprecated pas
  # NOTE: format. 

  if ( !'deviceDeploymentID' %in% names(pat$meta) ) {
    pat$meta <- pas_addUniqueIDs(pat$meta)
  }
  
  # Remove duplicate data records and out-of-spec values
  pat <- 
    pat %>%
    pat_distinct() %>%
    pat_qc(removeOutOfSpec = TRUE)
  
  # ----- Apply FUN ------------------------------------------------------------
  
  result <- try({
    
    hourlyData <- FUN(pat, ...) %>%
      dplyr::mutate_all( function(x) replace(x, which(is.nan(x)), NA) ) %>%
      dplyr::mutate_all( function(x) replace(x, which(is.infinite(x)), NA) ) 
    
  }, silent = TRUE)

  # Handle FUN errors
  if ( 'try-error' %in% class(result) ) {
    stop(paste0("FUN(pat) failed to evaluate. ",
                "Please check 'FUN' and see ?pat_createAirSensor for details."))
  }
  
  # ----- Validate hourlyData --------------------------------------------------
  
  # Check hourly axis
  
  # NOTE: Any missing hour is filled in with NA, so no gaps _other_ than 1 hour 
  # NOTE: and -23 should exist with index lag = 1. 
  
  if ( !all(diff(lubridate::hour(hourlyData$datetime)) == 1 | 
            diff(lubridate::hour(hourlyData$datetime)) == -23) ) {
    stop(paste0("Error: 'FUN(pat)' does not return regular hourly datetime axis. ", 
                "Please check 'FUN' and see ?pat_createAirSensor for details."))
  }
  
  # Check if parameter is defined in hourlyData
  if ( !parameter %in% names(hourlyData) ) {
    stop(paste0("'parameter' is not defined in 'FUN(pat)' output. ",
               "Please check 'FUN' and see ?pat_createAirSensor for details."))
  }
  
  # ----- Create data ----------------------------------------------------------
  
  # NOTE:  As of PWFSLSmoke version 1.2, both 'meta' must be a dataframe with
  # NOTE:  rownames.  Here we downgrade from tbl to dataframe.
  
  data <- hourlyData[, c("datetime", parameter)]

  names(data) <- c("datetime", pat$meta$deviceDeploymentID) 
  
  # Round the datetime axis to the nearest hour and convert to dataframe
  data$datetime <- 
    lubridate::round_date(data$datetime, 'hour')
  
  data <- as.data.frame(data)
  
  # ----- Create metadata  -----------------------------------------------------
  
  # NOTE:  As of PWFSLSmoke version 1.2, both 'meta' must be a dataframe with
  # NOTE:  rownames.  Here we downgrade from tbl to dataframe.
  
  # Copy metadata from pat object
  meta <-
    pat$meta %>%
    as.data.frame()
  
  # Add standard metadata found in PWFSLSmoke ws_monitor objects
  meta$monitorID <- meta$deviceDeploymentID
  meta$elevation <- as.numeric(NA)
  meta$siteName <- meta$label
  meta$countyName <- as.character(NA)
  meta$msaName <- as.character(NA)
  meta$monitorType <- meta$sensorType
  meta$siteID <- meta$locationID
  meta$instrumentID <- meta$deviceID
  meta$aqsID <- as.character(NA)
  meta$pwfslID <- as.character(NA)
  meta$pwfslDataIngestSource <- "ThingSpeak"
  meta$telemetryAggregator <- as.character(NA)
  meta$telemetryUnitID <- as.character(NA)
  
  # To match PWFSLSmoke version 1.2.x, 'meta' must have rownames
  
  rownames(meta) <- meta$monitorID
  
  # ----- Return ws_monitor object ---------------------------------------------
  
  as_object <- list(
    meta = meta, 
    data = data
  )
  
  class(as_object) <- c("airsensor", "ws_monitor")
  
  return(as_object)
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  library(AirSensor)

  pat <- example_pat
  parameter <- 'pm25'
  FUN = PurpleAirQC_hourly_AB_00
  FUN = PurpleAirQC_hourly_AB_01
  FUN = PurpleAirQC_hourly_AB_02
  
  
}
