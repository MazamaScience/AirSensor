# Utility functions for the LRAPA colocation report

#' @export
#' @importFrom rlang .data
#' @importFrom stats lm
#' @import dplyr
#'
#' @title Linear model fitting of PurpleAir and federal PWFSL time series data
#'
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param monitor PWFSLSmoke Timeseries \emph{ws_monitor} object.
#'
#' @description Produces a linear model between data from PurpleAir and data
#' from the closest PWFSL monitor.
#'
#' @return A linear model, fitting the `pat` PurpleAir readings to the provided
#' PWFSL monitor readings.
#'
#' @examples
#' \donttest{
#' library(AirSensor)
#'
#' sensorMonitorFit(
#'   pat,
#'   monitor,
#'   startdate = 20200701,
#'   enddate = 20200708,
#'   modelParameters = c("pm25", "humidity")
#' )
#' }

sensorMonitorFit <- function(
  pat = NULL,
  monitor = NULL,
  startdate = NULL,
  enddate = NULL,
  modelParameters = c("pm25", "humidity")
) {

  AP1_hourlyData <-
    Amazon_Park %>%
    pat_filterDate(20200701, 20200708, timezone = "America/Los_Angeles") %>%
    pat_aggregate() %>%
    pat_extractData() %>%
    dplyr::mutate(pm25 = (pm25_A + pm25_B)/2) %>%
    dplyr::select(datetime, pm25, pm25_A, pm25_B, temperature, humidity)


  # Figure out which PWFSLSmoke monitor is needed and load that data for the appropriate time range
  monitor1_hourlyData <-
    monitor1 %>%
    PWFSLSmoke::monitor_subset(monitorIDs = "410391009_01", tlim=c(20200701, 20200708)) %>%
    PWFSLSmoke::monitor_extractData() %>%
    dplyr::rename(monitor_pm25 = "410391009_01")

}
