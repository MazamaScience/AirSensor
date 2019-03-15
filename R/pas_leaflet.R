#' @export
#' 
#' @title Leaflet interactive map of Purple Air sensors
#' 
#' @description This function creates interactive maps that will be displayed in 
#' RStudio's 'Viewer' tab.
#'
#' Typical usage would be to use the \code{param} argument to display pm25 
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
#' @details The \code{maptype} argument is mapped onto leaflet "ProviderTile" 
#' names. Current mappings include:
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
#' @param pas Enhanced dataframe of PurpleAir synoptic data.
#' @param param Value to plot -- defautls to \code{get('pm25_1hr')}.
#' @param radius Radius (pixels) of monitor circles.
#' @param opacity Opacity of monitor circles.
#' @param maptype Optional name of leaflet ProviderTiles to use, e.g. \code{terrain}. 
#' @param outsideOnly Logical specifying subsetting for monitors marked as 'outside'.
#' 
#' @return A leaflet "plot" object which, if not assigned, is rendered in 
#' Rstudio's 'Viewer' tab.
#' 
#' @examples
#' \dontrun{
#' # Methow Valley Clean Air Ambassador
#' initializeMazamaSpatialUtils()
#' pas <- pas_load()
#' mvcaa <- filter(pas, stringr::str_detect(pas$label, '^MV Clean Air'))
#' pas_leaflet(mvcaa, param="pm25_1hr")
#' }

pas_leaflet <- function(
  pas = NULL,
  param = "pm25_1hr",
  radius = 10,
  opacity = 0.8,
  maptype = "terrain",
  outsideOnly = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  # RUBY:  The dplyr::filter function used in the example removes the
  # RUBY: 'pas_synoptic' class so this test has to go. Not sure right now how
  # RUBY:  to get around that unless we provide our own 'filter' function that
  # RUBY:  adds it back. But that would mean duplicating lots of dplyr
  # RUBY:  functionality.
  # RUBY: 
  # RUBY:  For now, we'll just change this check to test for "data.frame".
  
  if ( !("data.frame" %in% class(pas)) ) 
    stop(paste0("First argument is not of class 'data.frame'."))
  
  if ( nrow(pas) == 0 || ncol(pas) == 0 )
    stop(paste0("One or both dimensions of the pa_synoptic object has length 0."))
  
  if ( !(param %in% c('pm25_current', 
                      'pm25_10min', 
                      'pm25_30min', 
                      'pm25_1hr',
                      'pm25_6hr', 
                      'pm25_1day', 
                      'pm25_1week', 
                      'humidity',
                      'pressure', 
                      'temperature', 
                      'pwfsl_closestDistance')) ) 
    stop(paste0('param value is invalid. See documentation to see valid options'))
  
  if ( !is.logical(outsideOnly) )
    stop(paste0('outsideOnly parameter should be TRUE/FALSE'))
  
  if ( !is.numeric(radius) )
    stop(paste0('radius parameter is non-numeric'))
  
  
  # ----- outsideOnly subsetting -----------------------------------------------
  
  if ( outsideOnly ) {
    pas <- subset(pas, pas$DEVICE_LOCATIONTYPE == 'outside')
  }
  
  # ----- Figure out names for a legend and colors for each point --------------
  
  # Ignore warnings from RColorBrewer as leaflet::colorBin does the right thing
  suppressWarnings({
    
    if ( stringr::str_detect(param, "^pm25") ) { # PM2.5
      colorFunc <- leaflet::colorBin(PWFSLSmoke::AQI$colors, 
                                     bins = PWFSLSmoke::AQI$breaks_24, 
                                     na.color = "#bbbbbb")
      cols <- colorFunc(pas[[param]])
      colors <- PWFSLSmoke::AQI$colors
      labels <- PWFSLSmoke::AQI$names
      legendTitle <- 'AQI Level'
      value <- round(pas[[param]], 1)
      unit <- '\U00B5g/m3'
    } else if ( param == "temperature" ) {       # Temperature
      colorFunc <- leaflet::colorNumeric("RdYlBu", domain = c(-50,130), 
                                         na.color = "#bbbbbb", reverse=TRUE)
      cols <- colorFunc(pas[[param]])
      breaks <- seq(-20,120,length.out=15)
      levels <- seq(-15,115,length.out=14)
      colors <- leaflet::colorBin("RdYlBu", domain=range(breaks), 
                                  bins=breaks, reverse=TRUE)(levels)
      labels <- c('<-10',
                  '-10-0',
                  '0-10',
                  '10-20',
                  '10-20',
                  '20-30',
                  '30-40',
                  '40-50',
                  '50-60',
                  '70-80',
                  '80-90',
                  '90-100',
                  '100-110',
                  '>110')
      legendTitle <- 'Temp in \U2109'
      value <- round(pas[[param]], 0)
      unit <- '\U2109'
    } else if ( param == "humidity" ) {          # Humidity
      colorFunc <- leaflet::colorNumeric("BrBG", domain = c(0,100), 
                                         na.color = "#bbbbbb", reverse=FALSE)
      cols <- colorFunc(pas[[param]])
      breaks <- seq(0,100,length.out=11)
      levels <- seq(5,95,length.out=10)
      colors <- leaflet::colorBin("BrBG", domain=range(breaks), 
                                  bins=breaks, reverse=FALSE)(levels)
      labels <- c('<10%',
                  '10-20%',
                  '20-30%',
                  '30-40%',
                  '40-50%',
                  '50-60%',
                  '60-70%',
                  '70-80%',
                  '80-90%',
                  '>90%')
      value <- round(pas[[param]], 0)
      legendTitle <- 'Relative Humidity'
      unit <- '%'
    } else if ( stringr::str_detect(param, "[dD]istance$") ) { # Distance
      # NOTE:  dplyr::arrange_at() works with names of variables but desc() doesn't
      pas$inverse_distance <- 1 / pas[[param]]
      pas <- dplyr::arrange_at(pas, 'inverse_distance') # low values 'last'
      # NOTE:  Use definied palette and bins
      bins <- c(0,100,200,500,1000,2000,3000,4000,5000,10000)
      domain <- range(bins)
      oranges <- rev(RColorBrewer::brewer.pal(9,'Oranges'))
      purples <- rev(RColorBrewer::brewer.pal(9,'Purples'))
      colors <- c(oranges[4:1],purples[3:7])
      colorFunc <- leaflet::colorBin(colors, domain = domain, 
                                     bins = bins, na.color = "#bbbbbb")
      cols <- colorFunc(pas[[param]])
      labels <- c('<100 m',
                  '100-200 m',
                  '200-500 m',
                  '0.5-1 km',
                  '1-2 km',
                  '2-3 km',
                  '3-4 km',
                  '4-5 km',
                  '5:10 km')
      legendTitle <- param
      value <- round(pas[[param]], 0)
      unit <- 'm'
      
    } else if ( param %in% c("r2_a", "r2_b") ) {
      bins <- c(0, .5, .75, .9, 1)
      domain <- c(0,1)
      colors <- c("#ffffd4","#fed98e","#fe9929","#cc4c02")
      colorFunc <- leaflet::colorBin(colors, domain = domain, 
                                     bins = bins, na.color = "#bbbbbb")
      cols <- colorFunc(pas[[param]])
      labels <- c('0-.5', '.5-.75', '.75-.9', '.9-1')
      legendTitle <- "R2"
      value <- round(pas[[param]], 2)
      unit <- ''
    } else if ( param %in% c("slope_a", "slope_b") ) {
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
                                     domain = range(pas[[param]]), 
                                     bins = 10, 
                                     na.color = "#bbbbbb")
      cols <- colorFunc(pas[[param]])
      breaks <- seq(domain[1],domain[2],length.out=11)
      offset <- diff(breaks)[1] / 2
      levels <- signif(seq(breaks[1]+offset, breaks[11]-offset,length.out=10), digits=2)
      labels <- as.character(levels)
      legendTitle <- "Scale Factor"
      value <- round(pas[[param]], 2)
      unit <- ''
    } else {                                    # Other
      domain <- range(pas[[param]], na.rm=TRUE)
      colorFunc <- leaflet::colorNumeric("Purples", 
                                         domain = domain, 
                                         na.color = "#bbbbbb", 
                                         reverse=FALSE)
      cols <- colorFunc(pas[[param]])
      breaks <- seq(domain[1],domain[2],length.out=6)
      offset <- diff(breaks)[1] / 2
      levels <- signif(seq(breaks[1]+offset, breaks[6]-offset,length.out=5), digits=4)
      colors <- leaflet::colorBin("Purples", domain=range(breaks), bins=breaks, reverse=FALSE)(levels)
      labels <- as.character(levels)
      value <- signif(pas[[param]], 4)
      legendTitle <- param
      unit <- ''
    }
    
    # Create popupText
    pas$popupText <- paste0(
      "<b>", pas$label, "</b><br/>",
      "<b>", param, " = ", value, " ", unit, "</b><br/>",
      "temperature = ", round(pas$temperature, 0), " F<br/>",
      "humidity = ", round(pas$humidity, 0), "%<br/>",
      "pm25_1hr = ", round(pas$pm25_1hr, 1), " \U00B5g/m3<br/>",
      "pm25_1day = ", round(pas$pm25_1day, 1), " \U00B5g/m3<br/>",
      "location_type = ", pas$DEVICE_LOCATIONTYPE, "<br/"
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
  SPDF <- sp::SpatialPointsDataFrame(coords=cbind(pas$longitude,pas$latitude),
                                     data=as.data.frame(pas))
  
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
      popup=pas$popupText) %>%
    leaflet::addLegend(
      position='bottomright',
      colors=rev(colors), # show low levels at the bottom
      labels=rev(labels),  # show low levels at the bottom
      opacity = 1,
      title=legendTitle)
  
  return(m)
  
}
