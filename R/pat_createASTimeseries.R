#' @export
#' @importFrom rlang .data
#' 
#' @title Create a AirSensor Timeseries object
#' 
#' @param pat Purple Air timeseries data from \code{createPATimeseriesObject()}
#' @param period The time period to average to. Can be "sec", "min", "hour", 
#' "day". A number can also precede these options followed by a space 
#' (i.e. "15 min" or "1 hour").
#' @param pm25Source Which \code{pat} object columns to use when calculating
#' the "pm25" column: either \code{A}, \code{B} or \code{AB}
#' @param qcType named quality control method (currently none)
#' @param removeOutliers logical specifying whether to automatically remove
#' outliers using \code{\link{pat_outliers}} with default parameters.
#' @param ... Extra parameters to fine tune time-averaging function
#' 
#' @return List with \code{meta} and \code{data} elements on a uniform time 
#' axis.
#' 
#' @description Aggregates a PurpleAir timeseries object along the datetime
#' axis to produce a new dataframe period-aggregated timeseries data.
#' 
#' @return "as_timeseries" list of aggregated time series PurpleAir data
#' 
#' @examples 
#' \dontrun{
#' pat <- 
#'   AirSensor::example_pat %>%
#'   pat_filterDate(20180701, 20180901)
#' ast <- createASTimeseriesObject(pat, "1 hour")
#' }

pat_createASTimeseriesObject <- function(
  pat = NULL, 
  period = "1 hour", 
  pm25Source = "AB",
  qcType = NULL,
  removeOutliers = TRUE,
  ...
) {
  
  # ----- Validate Parameters --------------------------------------------------
  
  # Avoid opaque error message from openair when a user types "minute(s)"
  period <- stringr::str_replace(period, "ute?", "")
  
  # ----- Wrapped Time Average Function ----------------------------------------
  
  timeAverage <- function(
    df, 
    period, 
    dataThreshold = 0, 
    stats = "mean", 
    fill = FALSE, 
    ...
  ) {
    
    avg <- 
      df %>%
      openair::timeAverage(
        avg.time = period, 
        data.thresh = dataThreshold, 
        statistic = stats, 
        fill = fill, 
        ...
      ) 
    
    return(avg)
    
  }
  
  # ----- Remove outliers ------------------------------------------------------

  if ( removeOutliers ) 
    pat <- pat_outliers(pat, replace = TRUE, showPlot = FALSE)
  
  # ----- Create Dataframes ----------------------------------------------------
  
  # NOTE:  temperature, humidity and pm25_A are all found on the A channel and
  # NOTE:  are associated with datetime_A
  
  # Anything outside of 0:100 is aphysical
  humidity_df <-
    pat$data %>% 
    dplyr::select(.data$datetime_A, .data$humidity) %>% 
    dplyr::filter(.data$humidity >= 0 & .data$humidity <= 100) %>%
    dplyr::filter(!is.na(.data$humidity)) %>%
    dplyr::rename(date = .data$datetime_A) 
  
  # US temperature extremes are -80:134
  #   https://ggweather.com/climate/extremes_us.htm
  temperature_df <-
    pat$data %>% 
    dplyr::select(.data$datetime_A, .data$temperature) %>% 
    dplyr::filter(.data$temperature >= -80 & .data$temperature <= 140) %>%
    dplyr::filter(!is.na(.data$temperature)) %>% 
    dplyr::rename(date = .data$datetime_A)
    
  # Sensor range is 0:1000
  # TODO:  URL for sensor
  pm25_A_df <-
    pat$data %>% 
    dplyr::select(.data$datetime_A, .data$pm25_A) %>% 
    dplyr::filter(.data$pm25_A >= 0 & .data$pm25_A <= 1000) %>%
    dplyr::filter(!is.na(.data$pm25_A)) %>%
    dplyr::rename(date = .data$datetime_A, pm25 = .data$pm25_A)
  
  pm25_B_df <-
    pat$data %>% 
    dplyr::select(.data$datetime_B, .data$pm25_B) %>% 
    dplyr::filter(.data$pm25_B >= 0 & .data$pm25_B <= 1000) %>%
    dplyr::filter(!is.na(.data$pm25_B)) %>%
    dplyr::rename(date = .data$datetime_B, pm25 = .data$pm25_B)
  
  if ( pm25Source == "A" ) {
    pm25_df <- pm25_A_df
  } else if ( pm25Source == "B" ) {
    pm25_df <- pm25_B_df
  } else {
    pm25_df <- 
      dplyr::bind_rows(pm25_A_df, pm25_B_df) %>%
      dplyr::arrange(date)
  }
  
  # NOTE:  At this point you can easily plot any of these dataframes with e.g.:
  # NOTE:    plot(pm25, pch = 15, cex = 0.2)
  
  # ----- Columns for mean data (or any custom param) --------------------------
  
  humidity_avg <-
    humidity_df %>% 
    timeAverage(period = period, ...)
  
  temperature_avg <- 
    temperature_df %>% 
    timeAverage(period = period, ...)
  
  pm25_avg <- 
    pm25_df %>% 
    timeAverage(period = period, ...)
  
  # ----- Columns for Standard Deviation ---------------------------------------
  
  humidity_sd <-
    humidity_df %>% 
    timeAverage(period = period, stats = "sd", ...) %>% 
    dplyr::rename(humidity_sd = .data$humidity)
  
  temperature_sd <-
    temperature_df %>% 
    timeAverage(period = period, stats = "sd", ...) %>% 
    dplyr::rename(temperature_sd = .data$temperature)
  
  pm25_sd <-
    pm25_df %>% 
    timeAverage(period = period, stats = "sd", ...) %>% 
    dplyr::rename(pm25_sd = .data$pm25)
  
  # ----- Columns for freq count -----------------------------------------------
  
  humidity_ct <- 
    humidity_df %>% 
    timeAverage(period = period, stats = "frequency", ...) %>% 
    dplyr::rename(humidity_ct = .data$humidity)
  
  temperature_ct <- 
    temperature_df %>% 
    timeAverage(period = period, stats = "frequency", ...) %>% 
    dplyr::rename(temperature_ct = .data$temperature)
  
  pm25_ct <- 
    pm25_df %>% 
    timeAverage(period = period, stats = "frequency", ...) %>% 
    dplyr::rename(pm25_ct = .data$pm25)
  
  # ----- Columns for QC -------------------------------------------------------
  # NOTE: TEMP FILL WITH NA
  # TODO: Determine QC algorithm 
  
  humidity_qc <- 
    humidity_df %>% 
    timeAverage(period = period, dataThreshold = 100) %>% 
    dplyr::rename(humidity_qc = .data$humidity)
  
  temperature_qc <- 
    temperature_df %>% 
    timeAverage(period = period, dataThreshold = 100) %>% 
    dplyr::rename(temperature_qc = .data$temperature) 
  
  pm25_qc <-
    pm25_df %>% 
    timeAverage(period = period, dataThreshold = 100) %>% 
    dplyr::rename(pm25_qc = .data$pm25)
  
  # ----- Create as_timeseries object  -----------------------------------------
  
  data <- 
    plyr::join_all(
      list(
        pm25_avg, 
        humidity_avg,
        temperature_avg, 
        pm25_sd, 
        humidity_sd, 
        temperature_sd, 
        pm25_ct, 
        humidity_ct, 
        temperature_ct, 
        pm25_qc, 
        humidity_qc, 
        temperature_qc
      ), 
      by = "date", 
      type = "left"
    ) %>%
    dplyr::rename(datetime = date)
  
  # TODO: Determie what should be in meta
  meta <- 
    pat$meta %>% 
    dplyr::select(
      .data$ID, 
      .data$label,
      .data$longitude, 
      .data$latitude,
      .data$timezone,
      .data$countryCode, 
      .data$stateCode 
    )
  
  as_object <- list(meta = meta, data = data)
  class(as_object) <- c("as_timeseries", class(pat))
  
  return(as_object)
  
}
