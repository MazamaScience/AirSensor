#' @export
#' @importFrom rlang .data
#' 
#' @title Join airsensor objects from different time periods
#' 
#' @param sensor1 An AirSensor object.
#' @param sensor2 An AirSensor object.
#' 
#' @description AirSensor objects are "joined end-to-end" so that time ranges
#' are extended for all sensors that appear in either \code{sensor1} and 
#' \code{sensor2}.
#' 
#' Only two \code{airsensor} objects at a time may be joined.
#'
#' @return An \emph{airsensor} object containing all data from both incoming objects.
#' 
#' @examples
#' \donttest{
#' library(AirSensor)
#' setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
#' 
#' jan <- sensor_loadMonth("scaqmd", 202001)
#' feb <- sensor_loadMonth("scaqmd", 202002)
#' mar <- sensor_loadMonth("scaqmd", 202003)
#' apr <- sensor_loadMonth("scaqmd", 202004)
#' 
#' feb_mar <- sensor_join(feb, mar)
#' PWFSLSmoke::monitor_timeseriesPlot(feb_mar, style = 'gnats')
#' 
#' # Gaps in the time axis are filled with NA
#' feb_apr <- sensor_join(feb, apr)
#' PWFSLSmoke::monitor_timeseriesPlot(feb_apr, style = 'gnats')
#' }

sensor_join <- function(
  sensor1 = NULL, 
  sensor2 = NULL 
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(sensor1)
  MazamaCoreUtils::stopIfNull(sensor2)
  
  if ( !PWFSLSmoke::monitor_isMonitor(sensor1) )
    stop("Parameter 'sensor' is not a valid 'airsensor' object.") 
  
  if ( PWFSLSmoke::monitor_isEmpty(sensor1) ) 
    stop("Parameter 'sensor' has no data.")
  
  if ( !PWFSLSmoke::monitor_isMonitor(sensor2) )
    stop("Parameter 'sensor' is not a valid 'airsensor' object.") 
  
  if ( PWFSLSmoke::monitor_isEmpty(sensor2) ) 
    stop("Parameter 'sensor' has no data.")
  
  # ----- Partition sensor objects ---------------------------------------------
  
  # Determine which one is earlier
  if ( sensor1$data$datetime[1] < sensor2$data$datetime[1] ) {
    a <- sensor1
    b <- sensor2
  } else {
    a <- sensor2
    b <- sensor1
  }

  # Partition into a_only, b_only and ab_shared  
  a_IDs <- a$meta$monitorID
  b_IDs <- b$meta$monitorID
  
  a_only_IDs <- setdiff(a_IDs, b_IDs)
  b_only_IDs <- setdiff(b_IDs, a_IDs)
  ab_shared_IDs <- intersect(a_IDs, b_IDs)
  
  a_only <- PWFSLSmoke::monitor_subset(a, monitorIDs = a_only_IDs, dropMonitors = FALSE)
  b_only <- PWFSLSmoke::monitor_subset(b, monitorIDs = b_only_IDs, dropMonitors = FALSE)
  a_shared <- PWFSLSmoke::monitor_subset(a, monitorIDs = ab_shared_IDs, dropMonitors = FALSE)
  b_shared <- PWFSLSmoke::monitor_subset(b, monitorIDs = ab_shared_IDs, dropMonitors = FALSE)
  
  if ( !all(names(a_shared$data) == names(b_shared$data)) ) {
    stop("a_shared and b_shared have disordered columns")
  }
  
  # ----- Join shared IDs ------------------------------------------------------
  
  # Start with an airsensor object which will have 'meta' and 'data' replaced
  ab_shared <- a
  
  # NOTE:  Create 'meta' with b first so later removal of duplicate IDs will keep
  # NOTE:  the more recent record.
  ab_shared$meta <- 
    dplyr::bind_rows(b_shared$meta, a_shared$meta) %>%
    dplyr::distinct()
  
  # Remove any rows that are duplicates because of, e.g. pwfsl_closestDistance
  if ( any(duplicated(ab_shared$meta$monitorID)) ) {
    keepMask <- !duplicated(ab_shared$meta$monitorID)
    ab_shared$meta <- ab_shared$meta[keepMask,]
  }
  
  # Trim a$data to end at start of b$data
  a_starttime <- a_shared$data$datetime[1]
  a_endtime <- b_shared$data$datetime[1] - lubridate::hours(1)
  a_shared <- PWFSLSmoke::monitor_subset(a_shared, tlim = c(a_starttime, a_endtime))
  
  # Replace ab_shared$data with combined a_shared$data and b_shared$data
  ab_shared$data <- dplyr::bind_rows(a_shared$data, b_shared$data)
  
  # ----- Combine a_only, b_only and ab_shared ---------------------------------
  
  # Combine and fill with NA
  c <- PWFSLSmoke::monitor_combine(list(a_only, ab_shared))
  d <- PWFSLSmoke::monitor_combine(list(c, b_only))
  
  # Guarantee proper ordering
  airsensor <- PWFSLSmoke::monitor_reorder(d, d$meta$monitorID)
  
  # ----- Return ---------------------------------------------------------------
  
  return(airsensor)
  
}
