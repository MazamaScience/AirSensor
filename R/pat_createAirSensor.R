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
#' return a \emph{pat} object with a regular hourly datetime axis. \code{FUN} 
#' can access and utilize any component of a standard \emph{pat} object 
#' (e.g pm25_A, temperature, etc.) as well as define new variables in the 
#' \emph{pat} data. See examples. 
#' 
#' \code{parameter} allows user to select which variable to use for the 
#' univariate \emph{airsensor} object (e.g 'pm25_A', 'humidity', etc.). 
#' Furthermore the \code{parameter} can be a new variable created via \code{FUN} 
#' evaluation. See examples.
#' 
#' \code{...} Additional optional parameters or data that a user may implement 
#' support for in the \code{FUN}.  
#'
#' @return An "airsensor" object of aggregated PurpleAir Timeseries data.
#' 
#' @seealso \link{PurpleAirQC_hourly_AB_02}
#' @seealso \link{pat_aggregate}
#' 
#' @examples 
#' # Default FUN
#' sensor <- pat_createAirSensor(example_pat)
#' 
#' # Package included aggregation/QC FUN
#' sensor <- pat_createAirSensor(
#'   example_pat, 
#'   parameter = 'pm25', 
#'   FUN = AirSensor::PurpleAirQC_hourly_AB_02
#' )
#' 
#' # Custom FUN
#' add_jitter <- function(pat, y) {
#'   # Create custom_pm variable 
#'   pat$data$custom_pm <- pat$data$pm25_A + y
#'   # Default hourly aggregation
#'   pat <- pat_aggregate(pat)
#'   return(pat)
#' } 
#' # Create noise
#' jitter <- rnorm(n = nrow(example_pat$data))
#' 
#' # Evaluate custom FUN with parameters 
#' sensor <- pat_createAirSensor(example_pat, parameter = 'custom_pm', FUN = add_jitter, y = jitter)

pat_createAirSensor <- function(
  pat = NULL,
  parameter = 'pm25', 
  FUN = NULL, 
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
  
  if ( !is.null(FUN) ) {
    if ( !rlang::is_closure(FUN) ) {
      stop(paste0("Provided 'FUN' is not a function.",
                  "(Pass in the function with no quotes and no parentheses.)"))
    }
  }
  
  # Check if deviceDeploymentID is in the meta data. If not, add uniqueIDs.
  # NOTE: This is necessary as of 2020-04-20 to avoid errors with deprecated pas
  # NOTE: format. 
  # NOTE: Perhaps use pas_upgrade instead?
  if ( !'deviceDeploymentID' %in% names(pat$meta) ) {
    pat$meta <- pas_addUniqueIDs(pat$meta)
  }
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  # Remove out of spec
  pat <- pat_qc(pat, removeOutOfSpec = TRUE, max_humidity = NULL)
  
  # ----- Apply FUN ------------------------------------------------------------
  
  # NOTE: If FUN is null: 
  # NOTE: use a mean aggregation with no qc and average both channels 
  if ( is.null(FUN) ) {
    FUN <- function(x, ...) {
      tmp_pat <- pat_aggregate(x, function(x_, ...) mean(x_, na.rm = TRUE, ...))
      tmp_pat$data <- 
        tmp_pat$data %>% 
        dplyr::mutate(pm25 = rowMeans(cbind(.data$pm25_A, .data$pm25_B), na.rm = TRUE))
      return(tmp_pat)
    }
  }
  
  # Evaluate FUN function
  result <- try(
    expr = { eval_pat <- match.fun(FUN)(pat, ...) }, #? FUN(...) or match.fun(FUN)(...)
    silent = TRUE 
  )
  
  # Handle FUN errors 
  if ( 'try-error' %in% class(result) ) {
    stop('`FUN(pat, ...)` failed to evaluate. 
         Please check `FUN` and see `?pat_createAirSensor` for details.')
  }
  
  # ----- Validate evaluated PAT -----------------------------------------------
  
  # Check hourly axis in eval pat
  # NOTE: Any missing hour is filled in with NA, so no gaps _other_ than 1 hour 
  # NOTE: and -23 should exist with index lag = 1. 
  if ( !all(diff(lubridate::hour(eval_pat$data$datetime)) == 1 | 
            diff(lubridate::hour(eval_pat$data$datetime)) == -23) ) {
    stop('Error: `FUN(pat, ...)` does not return regular hourly datetime axis. 
         Please see `?pat_createAirSensor` for details.')
  }
  
  # Check if parameter is defined in eval pat
  if ( !parameter %in% names(eval_pat$data) ) {
    stop('`parameter` is not defined in `FUN(pat, ...)` output. 
    Please see `?pat_createAirSensor` for details.')
  }
  
  # ----- Create data ----------------------------------------------------------
  
  # Select data and cleanup any NaN or Inf that might have snuck in
  data <-
    eval_pat$data %>%
    dplyr::select(.data$datetime, .data[[parameter]]) %>% 
    dplyr::mutate_all( function(x) replace(x, which(is.nan(x)), NA) ) %>%
    dplyr::mutate_all( function(x) replace(x, which(is.infinite(x)), NA) ) 
  
  names(data) <- c("datetime", pat$meta$deviceDeploymentID) 
  
  # Round the datetime axis to the nearest hour 
  data$datetime <- lubridate::round_date(data$datetime, 'hour')
  
  # ----- Create metadata  -----------------------------------------------------
  
  # Copy metadata from pat object
  meta <- 
    eval_pat$meta %>% 
    as.data.frame()
  
  # Add metadata found in PWFSLSmoke ws_monitor objects
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
  
  # ----- Return ws_monitor object ---------------------------------------------
  
  as_object <- list(
    meta = meta, 
    data = data
  )
  
  class(as_object) <- c("airsensor", "ws_monitor")
  
  return(as_object)
  
}
