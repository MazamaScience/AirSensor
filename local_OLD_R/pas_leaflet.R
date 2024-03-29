#' @export
#' 
#' @title Leaflet interactive map of PurpleAir sensors
#' 
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param parameter Value to plot, e.g. \code{pm25_1hr}.
#' @param paletteName Predefined color palette name. Can be of the following:
#' \itemize{
#' \item{"AQI"}
#' \item{"humidity}
#' \item{"temperature}
#' \item{"distance"}
#' }
#' @param radius Radius (pixels) of monitor circles.
#' @param opacity Opacity of monitor circles.
#' @param maptype Optional name of leaflet ProviderTiles to use, e.g. \code{terrain}. 
#' @param outsideOnly Logical specifying subsetting for monitors marked as 'outside'.
#' 
#' @description This function creates interactive maps that will be displayed in 
#'   RStudio's 'Viewer' tab.
#'
#' Typical usage would be to use the \code{parameter} argument to display pm25 
#' values from one of:
#' \itemize{
#' \item{"pm25_current"}
#' \item{"pm25_10min"}
#' \item{"pm25_30min"}
#' \item{"pm25_1hr"}
#' \item{"pm25_6hr"}
#' \item{"pm25_1day"}
#' \item{"pm25_1week"}
#' }
#' 
#' Auxiliary \code{parameter} arguments can be used to display various Purple Air 
#' sensor data. Currently supported \code{parameter} arguments include:
#' \itemize{
#' \item{"humidity"}
#' \item{"pressure"}
#' \item{"temperature"}
#' \item{"pwfsl_closestDistance"}
#' } 
#' 
#' @details The \code{maptype} argument is mapped onto leaflet "ProviderTile" 
#'   names. Current mappings include:
#' \enumerate{
#' \item{"roadmap"}{ -- "OpenStreetMap"}
#' \item{"satellite"}{ -- "Esri.WorldImagery"}
#' \item{"terrain"}{ -- "Esri.WorldTopoMap"}
#' \item{"toner"}{ -- "Stamen.Toner"}
#' }
#'
#' If a character string not listed above is provided, it will be used as the 
#' underlying map tile if available. See 
#' \url{https://leaflet-extras.github.io/leaflet-providers/} for a list of 
#' "provider tiles" to use as the background map.
#' 
#' @note The \code{paletteName} parameter can take the name of an RColorBrewer
#' paeltte, \emph{e.g.} \code{"BuPu"} or \code{"Greens"}.
#' 
#' @return A leaflet "plot" object which, if not assigned, is rendered in 
#' Rstudio's 'Viewer' tab.
#' 
#' @examples
#' \donttest{
#' library(AirSensor)
#' setArchiveBaseUrl("https://airsensor.aqmd.gov/PurpleAir/v1")
#' 
#' # California
#' ca <-
#'   pas_load() %>%
#'   pas_filter(stateCode == 'CA')
#' 
#' if ( interactive() ) {
#'   pas_leaflet(ca, parameter = "pm25_1hr")
#' 
#'   pas_leaflet(ca, parameter = "temperature")
#' 
#'   pas_leaflet(ca, parameter = "humidity")
#' 
#'   pas_leaflet(ca, parameter = "pwfsl_closestDistance", maptype = "satellite")
#' }
#' }

pas_leaflet <- function(
  pas = NULL,
  parameter = "pm25_1hr",
  paletteName = NULL,
  radius = 10,
  opacity = 0.8,
  maptype = "terrain",
  outsideOnly = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pas)
  
  if ( !pas_isPas(pas) ) {
    stop("Required parameter 'pas' is not a valid 'pa_synoptic' object.")
  }
  if ( pas_isEmpty(pas) ) {
    stop("Required parameter 'pas' has no data.") 
  }
  if ( !(parameter %in% c('pm25_current', 
                          'pm25_10min', 
                          'pm25_30min', 
                          'pm25_1hr',
                          'pm25_6hr', 
                          'pm25_1day', 
                          'pm25_1week', 
                          'humidity',
                          'pressure', 
                          'temperature', 
                          'pwfsl_closestDistance')) ) {
    stop("Required parameter 'parameter' is invalid.")
  }
  if ( !is.logical(outsideOnly) ) {
    stop(paste0('outsideOnly parameter should be TRUE/FALSE'))
  }
  if ( !is.numeric(radius) ) {
    stop(paste0('radius parameter is non-numeric'))
  }
  
  # ----- outsideOnly subsetting -----------------------------------------------
  
  if ( outsideOnly ) {
    pas <- subset(pas, pas$DEVICE_LOCATIONTYPE == 'outside')
  }
  
  # ----- Figure out names for a legend and colors for each point --------------
  
  # Ignore warnings from RColorBrewer as leaflet::colorBin does the right thing
  suppressWarnings({
    
    if ( stringr::str_detect(tolower(parameter), "^pm25") ) { # AQI
      
      if ( tolower(paletteName) == "aqi" || is.null(paletteName) ) {
        
        colorInfo <- pas_palette(pas, "aqi", parameter)
        cols <- colorInfo$colors
        
        colors <- colorInfo$key[,2]
        labels <- colorInfo$key[,1]
        legendTitle <- 'AQI'
        value <- round(pas[[parameter]], 1)
        unit <- '\U00B5g/m3'
        
      } else { # Other Palettes
        
        colorInfo <- pas_palette(
          pas, 
          paletteName = paletteName, 
          parameter = parameter, 
          reverse = TRUE
        )
        
        cols <- colorInfo$colors
        
        colors <- colorInfo$key[,2]
        labels <- c(
          "0-12", 
          "12-35", 
          "35-55", 
          "55-150", 
          "150-250", 
          ">250"
        )
        
        legendTitle <- '\U00B5g/m^3'
        value <- round(pas[[parameter]], 2)
        unit <- '\U00B5g/m3'
      }
      
      
      
    } else if ( parameter == "temperature" ) {       # Temperature
      
      colorInfo <- 
        pas_palette(
          pas, 
          "temperature", 
          reverse = TRUE
        )
      
      cols <- colorInfo$colors
      labels <- colorInfo$key[,1]
      colors <- colorInfo$key[,2]
      
      legendTitle <- 'Temp in \U2109'
      value <- round(pas[[parameter]], 0)
      unit <- '\U2109'
      
    } else if ( parameter == "humidity" ) {          # Humidity
      
      colorInfo <- 
        pas_palette(
          pas, 
          "humidity", 
          reverse = FALSE
        )
      
      cols <- colorInfo$colors
      labels <- colorInfo$key[,1]
      colors <- colorInfo$key[,2]
      
      value <- round(pas[[parameter]], 0)
      legendTitle <- 'Relative Humidity'
      unit <- '%'
      
    } else if ( stringr::str_detect(parameter, "[dD]istance$") ) { # Distance
      
      colorInfo <- 
        pas_palette(
          pas, 
          "distance", 
          reverse = TRUE
        )
      
      cols <- colorInfo$colors
      labels <- colorInfo$key[,1]
      colors <- colorInfo$key[,2]
      
      
      legendTitle <- parameter
      value <- round(pas[[parameter]], 0)
      unit <- 'm'
      
    } else if ( parameter %in% c("r2_a", "r2_b") ) {
      
      bins <- c(0, .5, .75, .9, 1)
      domain <- c(0,1)
      colors <- c("#ffffd4","#fed98e","#fe9929","#cc4c02")
      colorFunc <- leaflet::colorBin(colors, domain = domain, 
                                     bins = bins, na.color = "#bbbbbb")
      cols <- colorFunc(pas[[parameter]])
      labels <- c('0-.5', '.5-.75', '.75-.9', '.9-1')
      legendTitle <- "R2"
      value <- round(pas[[parameter]], 2)
      unit <- ''
      
    } else if ( parameter %in% c("slope_a", "slope_b") ) {
      
      colors <- c("#7f3b08", 
                  "#b35806",
                  "#e08214",
                  "#fdb863",
                  "#fee0b6",
                  "#d8daeb",
                  "#b2abd2",
                  "#8073ac",
                  "#542788",
                  "#2d004b")
      colorFunc <- leaflet::colorBin(colors, 
                                     domain = range(pas[[parameter]]), 
                                     bins = 10, 
                                     na.color = "#bbbbbb")
      cols <- colorFunc(pas[[parameter]])
      breaks <- seq(domain[1],domain[2],length.out = 11)
      offset <- diff(breaks)[1] / 2
      levels <- signif(seq(breaks[1]+offset, breaks[11]-offset,length.out=10), digits=2)
      labels <- as.character(levels)
      legendTitle <- "Scale Factor"
      value <- round(pas[[parameter]], 2)
      unit <- ''
      
    } else {
      
      # All other parameters
      domain <- range(pas[[parameter]], na.rm = TRUE)
      colorFunc <- leaflet::colorNumeric("Purples", 
                                         domain = domain, 
                                         na.color = "#bbbbbb", 
                                         reverse = FALSE)
      cols <- colorFunc(pas[[parameter]])
      breaks <- seq(domain[1], domain[2], length.out = 6)
      offset <- diff(breaks)[1] / 2
      levels <- signif(seq(breaks[1]+offset, breaks[6]-offset,length.out=5), digits=4)
      colors <- leaflet::colorBin("Purples", domain=range(breaks), bins=breaks, reverse=FALSE)(levels)
      labels <- as.character(levels)
      value <- signif(pas[[parameter]], 4)
      legendTitle <- parameter
      unit <- ''
    }
    
    # Create popupText
    pas$popupText <- paste0(
      "<b>", pas$label, "</b><br/>",
      "<b>deviceDeploymentID = ", pas$deviceDeploymentID, "</b><br/>",
      "<b>", parameter, " = ", value, " ", unit, "</b><br/>",
      "temperature = ", round(pas$temperature, 0), " F<br/>",
      "humidity = ", round(pas$humidity, 0), "%<br/>",
      "pm25_1hr = ", round(pas$pm25_1hr, 1), " \U00B5g/m3<br/>",
      "pm25_1day = ", round(pas$pm25_1day, 1), " \U00B5g/m3<br/>",
      "location_type = ", pas$DEVICE_LOCATIONTYPE, "<br/>"
    )
    
  })
  
  # Extract view information
  lonRange <- range(pas$longitude, na.rm = TRUE)
  latRange <- range(pas$latitude, na.rm = TRUE)
  maxRange <- max(diff(lonRange),diff(latRange), na.rm = TRUE)
  # Determine appropriate zoom level
  if (maxRange > 20) {
    zoom <- 4
  } else if (maxRange > 10) {
    zoom <- 5
  } else if (maxRange > 5) {
    zoom <- 6
  } else if (maxRange > 2) {
    zoom <- 7
  } else if (maxRange > 1) {
    zoom <- 8
  } else if (maxRange > 0.5) {
    zoom <- 9
  } else if (maxRange > 0.2) {
    zoom <- 10
  } else if (maxRange > 0.1) {
    zoom <- 11
  } else {
    zoom <- 12
  }
  
  # Convert locations to SpatialPointsDataFrame
  pas <- pas[!is.na(pas$latitude),]
  SPDF <- sp::SpatialPointsDataFrame(coords = cbind(pas$longitude,pas$latitude),
                                     data = as.data.frame(pas))
  
  # Convert maptype to a character string that addProviderTiles can read
  if ( missing(maptype) || maptype == 'terrain') {
    providerTiles <- "Esri.WorldTopoMap"
  } else if ( maptype == "roadmap" ) {
    providerTiles <- "OpenStreetMap"
  } else if ( maptype == "toner" ) {
    providerTiles <- "Stamen.Toner"
  } else if (maptype == "satellite" ) {
    providerTiles <- "Esri.WorldImagery"
  } else {
    providerTiles <- maptype
  }
  
  # Create leaflet map
  m <- leaflet::leaflet(SPDF) %>%
    leaflet::setView(lng=mean(lonRange), lat=mean(latRange), zoom=zoom) %>%
    leaflet::addProviderTiles(providerTiles) %>%
    leaflet::addCircleMarkers(
      radius=radius,
      fillColor=cols,
      fillOpacity=opacity,
      stroke=FALSE,
      popup=pas$popupText,
      layerId = pas$label
      ) %>%
    leaflet::addLegend(
      position='bottomright',
      colors=rev(colors), # show low levels at the bottom
      labels=rev(labels),  # show low levels at the bottom
      opacity = 1,
      title=legendTitle)
  
  # ----- Return ---------------------------------------------------------------
  
  return(m)
  
}
