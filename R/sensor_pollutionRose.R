#' @export
#' 
#' @title Pollution rose plot
#' 
#' @param sensor an 'airsensor' object
#' @param windData a dataframe containing columns "date", "ws", and "wd".
#' @param statistic The statistic to be applied to each data bin in the plot. 
#' Options currently include “prop.count”, “prop.mean” and “abs.count”. 
#' The default “prop.count” sizes bins according to the proportion of the 
#' frequency of measurements. Similarly, “prop.mean” sizes bins according to 
#' their relative contribution to the mean. “abs.count” provides the absolute 
#' count of measurements in each bin. 
#' @param key control of the scale key via drawOpenKey. See drawOpenKey for 
#' further details.
#' @param keyPosition location where the scale key is to plotted. Allowed 
#' arguments currently include “top”, “right”, “bottom” and “left”.
#' @param annotate If TRUE then the percentage calm and mean values are printed 
#' in each panel together with a description of the statistic below the plot. 
#' If " " then only the stastic is below the plot. Custom annotations may be 
#' added by setting value to c("annotation 1", "annotation 2").
#' @param angle default angle of “spokes” is 30. Other potentially useful angles
#' are 45 and 10. Note: the width of the wind speed interval may need adjusting
#' using width.
#' @param angleScale The wind speed scale is by default shown at a 315 degree 
#' angle. Sometimes the placement of the scale may interfere with an interesting
#' feature. The user can therefore set angle.scale to another value (between 0 
#' and 360 degrees) to mitigate such problems. For example angleScale = 45 will
#' draw the scale heading in a NE direction.
#' @param gridLine Grid line interval to use. If NULL, as in default, this is 
#' assigned by based on the available data range. However, it can also 
#' be forced to a specific value, e.g. gridLine = 10. grid.line can also be a 
#' list to control the interval, line type and colour. For example 
#' gridLine = list(value = 10, lty = 5, col = "purple").
#' @param breaks the number of break points for wind speed in pollutant
#' @param paddle Either TRUE (default) or FALSE. If TRUE plots rose using 
#' ‘paddle’ style spokes. If FALSE plots rose using ‘wedge’ style spokes.
#' @param seg determines with width of the segments. For example, seg = 0.5 will
#'  produce segments 0.5 * angle.
#' @param normalize if TRUE each wind direction segment is normalized to equal 
#' one. This is useful for showing how the concentrations (or other parameters) 
#' contribute to each wind sector when the proprtion of time the wind is from 
#' that direction is low. A line showing the probability that the wind.
#' @param verbose Logical controlling the generation of progress and error messages.
#' 
#' @description Plots a traditional wind rose plot for wind direction and PM2.5.
#'
#' @return a plot or a dataframe
#' 
#' @seealso 
#' \url{https://davidcarslaw.github.io/openair/reference/windRose.html}
#'
#' @examples
#' \donttest{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirSensor)
#' 
#' # Set default location of pre-generated data files
#' setArchiveBaseUrl("https://airsensor.aqmd.gov/PurpleAir/v1")
#' 
#' pas <- pas_load(archival = TRUE)
#' pat <- pat_loadMonth(label = "SCBB_02", pas = pas, datestamp = 202005)
#' sensor <- pat_createAirSensor(pat)
#' 
#' # Load wind data from NOAA
#' windData <- worldmet::importNOAA(
#'   code = "722975-53141", 
#'   year = 2020
#' )
#' windData <- dplyr::select(windData, c("date", "wd", "ws"))
#' 
#' # Plot rose using mean binning
#' sensor_pollutionRose(sensor, windData, statistic = "prop.mean")
#' 
#' }, silent = FALSE)
#' }

sensor_pollutionRose <- function(
  sensor = NULL, 
  windData = NULL,
  statistic = "prop.count",
  key = TRUE, 
  keyPosition = "right",
  annotate = TRUE,
  angle = 30,
  angleScale = 315,
  gridLine = NULL,
  breaks = 6, 
  paddle = FALSE, 
  seg = 0.9, 
  normalize = FALSE,
  verbose = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(sensor)
  
  if ( !sensor_isSensor(sensor) )
    stop("Parameter 'sensor' is not a valid 'airsensor' object.") 
  
  if ( sensor_isEmpty(sensor) ) 
    stop("Required parameter 'sensor' has no data.")
  
  if ( nrow(sensor$meta) == 0 )
    stop("Parameter 'sensor' contains no sensors")
  
  if ( nrow(sensor$meta) > 1 )
    stop("Parameter 'sensor' contains more than one sensor")
  
  # ----- Get wind data --------------------------------------------------------
  
  # Find wind data readings from the closest NOAA site if none are provided
  if ( is.null(windData) ) {
    
    # TODO:  Using only the first entry's datetime for the year will be
    # TODO:  problematic if the timeframe spans more than one year.
    
    year <- lubridate::year(sensor$data$datetime[1])
    lon <- sensor$meta$longitude[1]
    lat <- sensor$meta$latitude[1]
    
    # Get first two nearest met data
    closestSites <- worldmet::getMeta(lon = lon, lat = lat, n = 2, plot = FALSE)
    
    siteData <- worldmet::importNOAA(
      code = closestSites$code[1], 
      year = year, 
      hourly = TRUE,
      n.cores = 1,
      quiet = !verbose,
      path = NA 
    )
    
    # Check if the first is NA to avoid errors
    if ( all(is.na(siteData$ws) | is.na(siteData$wd)) ) {
      
      siteData <- worldmet::importNOAA(
        code = closestSites$code[2], 
        year = year, 
        hourly = TRUE,
        n.cores = 1,
        quiet = !verbose,
        path = NA 
      )
      
    }
    
    windData <- dplyr::select(siteData, c("date", "wd", "ws"))
    
  }
  
  # Data must be the same length
  pollutantData <- 
    dplyr::tibble(
      "date" = sensor$data[[1]], 
      "pm25" = sensor$data[[2]] 
    )
  
  # Trim wind data to the sensor's time range
  windData <- dplyr::filter(windData, 
                            date >= min(sensor$data$datetime),
                            date <= max(sensor$data$datetime))
  
  # Combine df's 
  data <- 
    dplyr::left_join(
      x = windData, 
      y = pollutantData, 
      by = "date"
    )
  
  # Pollution Rose
  return({
    
    openair::pollutionRose(
      mydata = data,
      pollutant = "pm25",
      key.position = keyPosition,
      key = key,
      annotate = annotate,
      breaks = breaks,
      paddle= paddle,
      seg = seg,
      normalise = normalize,
      angle = angle,
      angle.scale = angleScale,
      statistic = statistic,
      grid.line = gridLine
    )
    
  })
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  library(AirSensor)
  setArchiveBaseUrl("https://airsensor.aqmd.gov/PurpleAir/v1")
  
  sensor <- NULL 
  windData <- NULL
  statistic <- "prop.mean"
  key <- TRUE 
  keyPosition <- "right"
  annotate <- TRUE
  angle <- 30
  angleScale <- 315
  gridLine <- NULL
  breaks <- 6 
  paddle <- FALSE 
  seg <- 0.9 
  normalize <- FALSE
 
  sensor <- 
    sensor_loadMonth(datestamp = 201809) %>%
    sensor_filterMeta(label == "SCUV_09")
  
  windsites <- 
    worldmet::getMeta(lat = sensor$meta$latitude, lon = sensor$meta$longitude, n = 3, plot = FALSE)
  
  windData <- 
    worldmet::importNOAA(code = windsites$code[1], year = 2018) %>%
    dplyr::select(windData, c("date", "wd", "ws"))
  
  sensor_pollutionRose(sensor, windData, statistic = "prop.mean")
  
  
  #####
  # Compare with
  #####
  
  tempdata <- data.frame(sensor$data$datetime, sensor$data$SCUV_09)
  colnames(tempdata) <- c("date", "SCUV_09")
  tempdata <- merge(tempdata, windData, by = "date")
  
  pollutionRose(tempdata, pollutant = "SCUV_09", statistic = "prop.mean")
  
}
