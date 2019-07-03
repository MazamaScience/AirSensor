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
#' \dontrun{
#' sensor_polarPlot(sensor = sensor, windData = wind, resolution = "normal")
#' }
#' 

sensor_polarPlot <- 
  function(
    sensor, 
    windData, 
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
    
    # Validate Parameters
    if ( !PWFSLSmoke::monitor_isMonitor(sensor) )
      stop("Parameter 'sensor' is not a valid 'airsensor' object.") 
    
    if ( PWFSLSmoke::monitor_isEmpty(sensor) ) 
      stop("Required parameter 'sensor' has no data.")
    
    if ( is.null(windData) ) 
      stop("Required parameter 'windData' is NULL")
    
    if ( !all((c("wd", "ws") %in% names(windData))) ) 
      stop("Parameter 'windData' does not contain necessary columns")
    
    # PM2.5 df 
    pollutantData <- 
      dplyr::tibble(
        "date" = sensor$data[[1]], 
        "pm25" = sensor$data[[2]] 
      )
    
    # Combine df's 
    data <- 
      dplyr::left_join(
        x = windData, 
        y = pollutantData, 
        by = "date"
      )
    
    # Polar Plot
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
    
  }
