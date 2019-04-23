#' @export
#' @importFrom rlang .data
#' @import dplyr
#' 
#' @title Create a AirSensor Timeseries object
#' 
#' @param pat Purple Air timeseries data from \code{createPATimeseriesObject()}
#' @param period The time period to average to. Can be "sec", "min", "hour", 
#' "day", "DSTday", "week", "month", or "year". A number can also
#' precede these options followed by a space (i.e. "2 day" or "37 min").
#' @param ... Extra parameters to fine tune time-averaging function
#' 
#' @return List with original \code{meta} and restructured \code{data} elements
#' 
#' @description Aggregates a PurpleAir timeseries object along the datetime
#' axis to produce a new dataframe with 13 columns of period-aggregated
#' timeseries data: averages, standard deviations, measurement frequency, and 
#' qualitity control. 
#' 
#' @return "as_timeseries" list of aggregated time series PurpleAir data
#' 
#' @seealso \link{pat_timeAverage()}
#' 
#' @examples 
#' \dontrun{
#' pat <- pat_load(pas, "Seattle", startdate = 20180701, enddate = 20180901)
#' seattle_AirSensor <- createASTimeseriesObject(pat, "2.5 hours")
#' }

createASTimeseriesObject <- function(
  pat, 
  period = "15 min", 
  ...
) {
  
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
  
  # ----- Make a data frame from pat and time average --------------------------
  
  make_df <- function(pat, parameter) { 
    
    df <- 
      pat$data %>% 
      select(.data$datetime, .data[[parameter]]) %>% 
      rename(date = .data$datetime) %>% 
      filter(!is.na(parameter))
    
    return(df)
    
  }
  
  # ----- Create Dataframes ----------------------------------------------------
  
  humidity_df <- 
    pat %>% 
    make_df(parameter = "humidity")
  
  temperature_df <- 
    pat %>% 
    make_df(parameter = "temperature")
  
  # TODO: Temporary - Merging A & B channels. CURRENTLY CHANNEL A 
  pm25_df <- 
    pat %>% 
    make_df(parameter = "pm25_A") %>% 
    dplyr::rename(pm25 = pm25_A)
  
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
    dplyr::rename(humidity_sd = humidity)
  
  temperature_sd <-
    temperature_df %>% 
    timeAverage(period = period, stats = "sd", ...) %>% 
    dplyr::rename(temperature_sd = temperature)
  
  pm25_sd <-
    pm25_df %>% 
    timeAverage(period = period, stats = "sd", ...) %>% 
    dplyr::rename(pm25_sd = pm25)
  
  # ----- Columns for freq count -----------------------------------------------
  
  humidity_ct <- 
    humidity_df %>% 
    timeAverage(period = period, stats = "frequency", ...) %>% 
    dplyr::rename(humidity_ct = humidity)
  
  temperature_ct <- 
    temperature_df %>% 
    timeAverage(period = period, stats = "frequency", ...) %>% 
    dplyr::rename(temperature_ct = temperature)
  
  pm25_ct <- 
    pm25_df %>% 
    timeAverage(period = period, stats = "frequency", ...) %>% 
    dplyr::rename(pm25_ct = pm25)
  
  # ----- Columns for QC -------------------------------------------------------
  # NOTE: TEMP FILL WITH NA
  # TODO: Determine QC algorithm 
  
  humidity_qc <- 
    humidity_df %>% 
    timeAverage(period = period, dataThreshold = 100) %>% 
    dplyr::rename(humidity_qc = humidity)
  
  temperature_qc <- 
    temperature_df %>% 
    timeAverage(period = period, dataThreshold = 100) %>% 
    dplyr::rename(temperature_qc = temperature) 
  
  pm25_qc <-
    pm25_df %>% 
    timeAverage(period = period, dataThreshold = 100) %>% 
    dplyr::rename(pm25_qc = pm25)
  
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
    )
  
  # TODO: Determie what should be in meta
  meta <- 
    pat$meta %>% 
    dplyr::select(
      .data$ID, 
      .data$label,
      .data$longitude, 
      .data$latitude, 
      .data$countryCode, 
      .data$stateCode 
    )
  
  as_object <- list(meta = meta, data = data)
  class(as_object) <- c("as_timeseries", class(pat))
  
  return(as_object)
  
}
