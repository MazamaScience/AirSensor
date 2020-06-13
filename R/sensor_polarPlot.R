#' @title Plot bivariate polar plots with guassian smoothing
#'
#' @param sensor an 'airsensor' object
#' @param windData  a dataframe containing columns "date", "ws", and "wd".
#' @param statistic The statistic that should be applied to each wind 
#' speed/direction bin. Because of the smoothing involved, the colour scale for 
#' some of these statistics is only to provide an indication of overall pattern 
#' and should not be interpreted in concentration units e.g. for 
#' statistic = "weighted.mean" where the bin mean is multiplied by the bin 
#' frequency and divided by the total frequency. In many cases using polarFreq 
#' will be better. Setting statistic = "weighted.mean" can be useful because it 
#' provides an indication of the concentration * frequency of occurrence and 
#' will highlight the wind speed/direction conditions that dominate the overall 
#' mean. Can be: “mean” (default), “median”, “max” (maximum), “frequency”. 
#' “stdev” (standard deviation), “weighted.mean”
#' @param resolution Two plot resolutions can be set: “normal” and “fine” 
#' (the default), for a smoother plot. It should be noted that plots with a 
#' “fine” resolution can take longer to render.
#' @param colors Colours to be used for plotting. Options include “default”, 
#' “increment”, “heat”, “jet” and RColorBrewer colours — see the openair 
#' openColours function for more details. For user defined the user can supply a
#' list of color names recognised by R (type colors() to see the full list). 
#' An example would be color = c("yellow", "green", "blue"). Can also take the 
#' values "viridis", "magma", "inferno", or "plasma" which are the viridis 
#' colour maps ported from Python's Matplotlib library.
#' @param alpha The alpha transparency to use for the plotting surface (a value 
#' between 0 and 1 with zero being fully transparent and 1 fully opaque).
#' @param angleScale The wind speed scale is by default shown at a 315 degree 
#' angle. Sometimes the placement of the scale may interfere with an interesting
#' feature. The user can therefore set angleScale to another value 
#' (between 0 and 360 degrees) to mitigate such problems. For example 
#' angle.scale = 45 will draw the scale heading in a NE direction.
#' @param normalize If TRUE concentrations are normalised by dividing by their 
#' mean value. This is done after fitting the smooth surface. This option is 
#' particularly useful if one is interested in the patterns of concentrations 
#' of PM2.5.
#' @param key Fine control of the scale key via drawOpenKey. See drawOpenKey for
#' further details.
#' @param keyPosition Location where the scale key is to plotted. Allowed 
#' arguments currently include "top", "right", "bottom" and "left".
#' @param ws_spread An integer used for the weighting kernel spread for wind 
#' speed when correlation or regression techniques are used. Default is 15.
#' @param wd_spread An integer used for the weighting kernel spread for wind 
#' direction when correlation or regression techniques are used. Default is 4.
#' 
#' @description Function for plotting PM2.5 concentration in polar coordinates 
#' showing concentration by wind speed and direction. 
#' @seealso 
#' \url{http://davidcarslaw.github.io/openair/reference/polarPlot.html} 
#' 
#' @return a plot and dataframe
#' @export
#'
#' @examples
#' \donttest{
#' library(AirSensor)
#' 
#' # Polar plot
#' sensor_polarPlot(example_sensor, resolution = "normal")
#' }

sensor_polarPlot <- function(
  sensor = NULL, 
  windData = NULL, 
  statistic = "mean", 
  resolution = "fine",
  colors = "default", 
  alpha = 1, 
  angleScale = 315,
  normalize = FALSE,
  key = TRUE, 
  keyPosition = "right", 
  ws_spread = 15, 
  wd_spread = 4
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(sensor)
  
  if ( !PWFSLSmoke::monitor_isMonitor(sensor) )
    stop("Parameter 'sensor' is not a valid 'airsensor' object.") 
  
  if ( PWFSLSmoke::monitor_isEmpty(sensor) ) 
    stop("Required parameter 'sensor' has no data.")
  
  if ( nrow(sensor$meta) == 0 )
    stop("Parameter 'sensor' contains no SC sensors")
  
  if ( nrow(sensor$meta) > 1 )
    stop("Parameter 'sensor' contains more than one SC sensor")
  
  # ----- Download wind data ---------------------------------------------------
  
  # Find wind data readings from the closest NOAA site if none are provided
  if ( is.null(windData) ) {
    # Using only the first entry's datetime for the year will be problematic 
    # if the timeframe spans more than one year...
    year <- lubridate::year(sensor$data$datetime[1])
    lon <- sensor$meta$longitude[1]
    lat <- sensor$meta$latitude[1]
    
    # Get first two nearest met data
    closeSites <- worldmet::getMeta(lon = lon, lat = lat, n = 2, plot = FALSE)
    
    siteCodes <- paste0(closeSites$USAF, "-", closeSites$WBAN)
    
    siteData <- worldmet::importNOAA( code = siteCodes[1], 
                                      year = year, 
                                      parallel = FALSE )
    # Check if the first is NA to avoid errors
    if ( all(is.na(siteData$ws) | is.na(siteData$wd)) ) {
      
      siteData <- worldmet::importNOAA( code = siteCodes[2], 
                                        year = year, 
                                        parallel = FALSE )
      
    }
    windData <- dplyr::select(siteData, c("date", "wd", "ws"))
  }
  
  # ----- Assemble data --------------------------------------------------------
  
  # PM2.5 df 
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
  
  # ----- Create plot ----------------------------------------------------------
  
  return({
    
    openair::polarPlot(
      mydata = data,
      pollutant = "pm25",
      statistic = statistic, 
      resolution = resolution,
      cols = colors, 
      alpha = alpha, 
      angle.scale = angleScale,
      normalise = normalize,
      key = key, 
      key.position = keyPosition, 
      ws_spread = ws_spread, 
      wd_spread = wd_spread
    )
    
  })
  
  # === Debug ===
  if (FALSE) {
    sensor = pas_load() %>% 
      pat_createNew(label='POLK GULCH', startdate = 20181001, enddate = 20181201) %>%
      pat_createAirSensor() 
    windData = NULL 
    statistic = "mean" 
    resolution = "fine"
    colors = "default" 
    alpha = 1 
    angleScale = 315
    normalize = FALSE
    key = TRUE 
    keyPosition = "right" 
    ws_spread = 15 
    wd_spread = 4
  }
  
}
