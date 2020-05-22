#' @keywords pat
#' @export
#' 
#' @title Upgrade PurpleAir Timeseries 
#'
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param verbose (logical) Display messages.
#' 
#' @description The \code{pat} parameter is checked for the latest 
#' \code{pa_timeseries} format and presence of and/or addition of core 
#' data columns:
#' \itemize{ 
#' \item{datetime -- A datetime column}
#' \item{pm25_A -- Channel A PM2.5} 
#' \item{pm25_B -- Channel B PM2.5} 
#' \item{temperature -- Tmperature in Faarehniet} 
#' \item{humidity -- Relative Humidity}
#' \item{pressure -- Pressure in hectopascals (hPa)} 
#' \item{pm1_atm_A -- Channel A PM1.0} 
#' \item{pm25_atm_A -- Channel A PM2.5} 
#' \item{pm10_atm_A -- Channel A PM10.0} 
#' \item{pm1_atm_B -- Channel B PM1.0}
#' \item{pm25_atm_B -- Channel B PM2.5}  
#' \item{pm10_atm_B -- Channel B PM10.0} 
#' \item{uptime -- Sensor uptime in seconds}
#' \item{rssi --  Sensor WiFi signal strength in dBm} 
#' \item{memory -- Memory Usage}
#' \item{adc0 -- Voltage} 
#' \item{bsec_iaq -- ?} 
#' \item{datetime_A -- Record datetime of Channel B}
#' \item{datetime_B -- Record datetime of Channel A} 
#' }
#'
#' @return An upgraded \code{pa_timeseries} object.
#'

pat_upgrade <- function(
  pat = NULL, 
  verbose = TRUE
) {
  
  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(pat)
  MazamaCoreUtils::stopIfNull(verbose)
  
  # ----- Required Columns Check -----------------------------------------------
  
  # NOTE: Required columns updated to AirSensor v0.8 pat data model.
  # NOTE: Valid as of 2020-05-18
  reqCols <- c(
    "datetime", 
    "pm25_A", 
    "pm25_B", 
    "temperature", 
    "humidity",
    "pressure", 
    "pm1_atm_A", 
    "pm25_atm_A", 
    "pm10_atm_A", 
    "pm1_atm_B",
    "pm25_atm_B", 
    "pm10_atm_B", 
    "uptime", 
    "rssi", 
    "memory", 
    "adc0", 
    "bsec_iaq", 
    "datetime_A",
    "datetime_B" 
  )
  
  if ( all(reqCols %in% names(pat$data)) ) {
    # Contains all columns -> skip. 
    
    # Show user format does not require upgrade.
    if ( verbose ) {
      message('pa_timeseries object does not require upgrade ... Skipping.')
    }
    
  } else {
     # Columns missing -> upgrade
    
    tryCatch(
      expr = {
        # Define the missing columns from the upgraded columns
        missingCols <- reqCols[!reqCols %in% names(pat$data)]
        
        # Add missing cols to pat data and fill with NA
        pat$data[,missingCols] <- NA
        
        # Show user format has upgraded
        if ( verbose ) {
          message('pa_timeseries object upgraded.')
        }
        
        # * Fill any other missing columns with NA
        missingColsCheck <- reqCols[!reqCols %in% names(pat$data)]
        for ( i in missingColsCheck ) {
          pat$data[[i]] <- NA
        }
        
        # Re-oder columns and remove any that are not valid
        pat$data <- pat$data[,reqCols]
        
      }, 
      # Catch errors 
      error = function(e) {
        msg <- paste('Error upgrading timeseries: ', e)
        stop(msg)
      }
    )
    
  }
  
  # ----- Post-upgrade validation ----------------------------------------------
  
  if ( !pat_isPat(pat) ) {
    stop('Error: pa_timeseries object failed to upgrade.')
  }
  if ( pat_isEmpty(pat) ) {
    stop("Required parameter 'pat' has no data.")
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(pat)
    
}