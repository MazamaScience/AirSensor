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
  
  if ( !sensor_isSensor(sensor1) )
    stop("Parameter 'sensor' is not a valid 'airsensor' object.") 
  
  if ( sensor_isEmpty(sensor1) ) 
    stop("Parameter 'sensor' has no data.")
  
  if ( !sensor_isSensor(sensor2) )
    stop("Parameter 'sensor' is not a valid 'airsensor' object.") 
  
  if ( sensor_isEmpty(sensor2) ) 
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
  
  a_only <- a %>% sensor_filterMeta(.data$monitorID %in% a_only_IDs)
  b_only <- b %>% sensor_filterMeta(.data$monitorID %in% b_only_IDs)
  a_shared <- a %>% sensor_filterMeta(.data$monitorID %in% ab_shared_IDs)
  b_shared <- b %>% sensor_filterMeta(.data$monitorID %in% ab_shared_IDs)

  if ( !sensor_isEmpty(b_shared) ) {

    # Guarantee proper ordering
    b_shared <- PWFSLSmoke::monitor_reorder(b_shared, a_shared$meta$monitorID)
    
    if ( !all(names(a_shared$data) == names(b_shared$data)) ) {
      stop("a_shared and b_shared contain non-identical sensors")
    }
    
  }

  # ----- Create start- and endtimes -------------------------------------------
  
  # Create an overall time axis
  starttime <- min(a$data$datetime)
  endtime <- max(b$data$datetime)
  datetime <- seq(starttime, endtime, by = "hours")
  hourlyDF <- data.frame(datetime = datetime)
  
  # NOTE:  In case of overlaps, keep more of the b data and make a stop one
  # NOTE:  timepoint before b starts.
  
  a_starttime <- min(a$data$datetime)
  a_endtime <- max(a$data$datetime)
  b_starttime <- min(b$data$datetime)
  b_endtime <- max(b$data$datetime)
  ab_overlapCount <- 
    difftime(a_endtime, b_starttime, units = "hour") %>% 
    as.numeric() + 1

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
  
  # NOTE:  Use of sensor_filterDatetime() returns a dataframe that includes
  # NOTE:  starttime but omits endttime, [starttime, endtime), so there will be
  # NOTE:  no overlapping timesteps.
  
  # If overlapping, trim a_shared to end just before b_starttime
  if ( ab_overlapCount > 0 ) {
    a_shared <- sensor_filterDatetime(a_shared, a_starttime, b_starttime)
  }
  
  # Replace ab_shared$data with combined a_shared$data and b_shared$data
  ab_shared$data <- dplyr::bind_rows(a_shared$data, b_shared$data)
  
  # Fill in missing 
  if ( ab_overlapCount < 0 ) {
    ab_shared$data <- dplyr::full_join(hourlyDF, ab_shared$data, by = "datetime")
  }
  
  # ----- Combine a_only, b_only and ab_shared ---------------------------------
  
  # Combine and fill with NA
  c <- PWFSLSmoke::monitor_combine(list(a_only, ab_shared))
  d <- PWFSLSmoke::monitor_combine(list(c, b_only))
  
  # Guarantee proper ordering
  airsensor <- PWFSLSmoke::monitor_reorder(d, d$meta$monitorID)
  
  # ----- Return ---------------------------------------------------------------
  
  return(airsensor)
  
}
