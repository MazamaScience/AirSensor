#' @export
#'
#' @title Leaflet interactive map of PurpleAir sensors
#'
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param parameter Value to plot, e.g. \code{pm2.5_60minute}.
#' @param paletteName \pkg{RColorBrewer} palette name to use when \code{parameter}
#' is something other than:
#' \itemize{
#' \item{"pm2.5_~"}
#' \item{"humidity}
#' \item{"temperature}
#' }
#' @param radius Radius (pixels) of monitor circles.
#' @param opacity Opacity of monitor circles.
#' @param maptype Optional name of leaflet ProviderTiles to use, e.g. \code{terrain}.
#'
#' @description This function creates interactive maps that will be displayed in
#' RStudio's 'Viewer' tab.
#'
#' Typical usage would be to use the \code{parameter} argument to display PM2.5
#' values from one of:
#' \itemize{
#' \item{"pm2.5_10minute"}
#' \item{"pm2.5_30minute"}
#' \item{"pm2.5_60minute" (aka "pm25")} 
#' \item{"pm2.5_6hour"}
#' \item{"pm2.5_24hour"}
#' \item{"pm2.5_1week"}
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
#' @note The \code{paletteName} parameter can take the name of an RColorBrewer
#' paeltte, \emph{e.g.} \code{"BuPu"} or \code{"Greens"}.
#'
#' @return A leaflet "plot" object which, if not assigned, is rendered in
#' Rstudio's 'Viewer' tab.
#'
#' @examples
#' library(AirSensor)
#'
#' if ( interactive() ) {
#'   pas_leaflet(example_pas, parameter = "pm25")
#'
#'   pas_leaflet(example_pas, parameter = "temperature")
#'
#'   pas_leaflet(example_pas, parameter = "humidity")
#' }

pas_leaflet <- function(
  pas = NULL,
  parameter = "pm25",
  paletteName = NULL,
  radius = 10,
  opacity = 0.8,
  maptype = "terrain"
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(pas)

  if ( !"pa_synoptic" %in% class(pas) )
    stop("parameter 'pas' is not a valid 'purple_air_synoptic' object.")

  if ( nrow(pas) == 0 )
    stop("parameter 'pas' has no data")

  if ( !parameter %in% names(pas) )
    stop(sprintf("parameter = '%s' is not found in the 'pas' object", parameter))

  if ( !is.numeric(radius) )
    stop(paste0("parameter 'radius' must be numeric"))

  # ----- Choose colors and title ----------------------------------------------

  # Ignore warnings from RColorBrewer as leaflet::colorBin() does the right thing
  suppressWarnings({

    if ( stringr::str_detect(tolower(parameter), "^pm2\\.5") ||
         stringr::str_detect(tolower(parameter), "^pm25"))  { # AQI

      colorInfo <- pas_palette(pas, "aqi", parameter)

      cols <- colorInfo$colors
      labels <- colorInfo$key[,1]
      colors <- colorInfo$key[,2]

      legendTitle <- 'AQI'
      value <- round(pas[[parameter]], 1)
      unit <- '\U00B5g/m3'

    } else if ( parameter == "temperature" ) {       # Temperature

      colorInfo <- pas_palette(pas, "temperature", reverse = TRUE)

      cols <- colorInfo$colors
      labels <- colorInfo$key[,1]
      colors <- colorInfo$key[,2]

      legendTitle <- 'Temp in \U2109'
      value <- round(pas[[parameter]], 0)
      unit <- '\U2109'

    } else if ( parameter == "humidity" ) {          # Humidity

      colorInfo <- pas_palette(pas, "humidity", reverse = FALSE)

      cols <- colorInfo$colors
      labels <- colorInfo$key[,1]
      colors <- colorInfo$key[,2]

      value <- round(pas[[parameter]], 0)
      legendTitle <- 'Relative Humidity'
      unit <- '%'

    } else {

      # All other parameters
      domain <- range(pas[[parameter]], na.rm = TRUE)
      colorFunc <-
        leaflet::colorNumeric(
          "Purples",
          domain = domain,
          na.color = "#bbbbbb",
          reverse = FALSE
        )
      cols <- colorFunc(pas[[parameter]])
      breaks <- seq(domain[1], domain[2], length.out = 6)
      offset <- diff(breaks)[1] / 2
      levels <- signif(seq(breaks[1] + offset, breaks[6] - offset, length.out = 5), digits = 4)
      colors <- leaflet::colorBin("Purples", domain = range(breaks), bins = breaks, reverse = FALSE)(levels)
      labels <- as.character(levels)
      value <- signif(pas[[parameter]], 4)
      legendTitle <- parameter
      unit <- ''

    }

  })

  # * Create popupText -----

  if ( "pm2.5_60_minute" %in% names(pas) ) {
    # New pas created with version >= 1.1
    pas$popupText <- paste0(
      "<b>", pas$locationName, "</b><br/>",
      pas$deviceDeploymentID, "<br/>",
      "location_type = ", pas$location_type, " <br/>",
      "temperature = ", round(pas$temperature, 0), " \U2109<br/>",
      "humidity = ", round(pas$humidity, 0), "%<br/>",
      "pm2.5_60minute = ", round(pas$pm2.5_60minute, 1), " \U00B5g/m3<br/>",
      "pm2.5_24hour = ", round(pas$pm2.5_24hour, 1), " \U00B5g/m3<br/>",
      "<br/>",
      "<b>", parameter, " = ", value, " ", unit, "</b>"
    )
  } else {
    # Old pas created with version < 1.1
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
  }
  
  # * Extract view information -----

  lonRange <- range(pas$longitude, na.rm = TRUE)
  latRange <- range(pas$latitude, na.rm = TRUE)
  maxRange <- max(diff(lonRange), diff(latRange), na.rm = TRUE)
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

  # ----- Create leaflet map ---------------------------------------------------

  if ( "pm2.5_60_minute" %in% names(pas) ) {
    # New pas created with version >= 1.1
    m <-
      leaflet::leaflet(dplyr::select(pas, c("longitude", "latitude"))) %>%
      leaflet::setView(lng = mean(lonRange), lat = mean(latRange), zoom = zoom) %>%
      leaflet::addProviderTiles(providerTiles) %>%
      leaflet::addCircleMarkers(
        radius = radius,
        fillColor = cols,
        fillOpacity = opacity,
        stroke = FALSE,
        popup = pas$popupText,
        layerId = pas$locationName
      ) %>%
      leaflet::addLegend(
        position = 'bottomright',
        colors = rev(colors), # show low levels at the bottom
        labels = rev(labels),  # show low levels at the bottom
        opacity = 1,
        title = legendTitle)
  } else {
    m <-
      leaflet::leaflet(dplyr::select(pas, c("longitude", "latitude"))) %>%
      leaflet::setView(lng = mean(lonRange), lat = mean(latRange), zoom = zoom) %>%
      leaflet::addProviderTiles(providerTiles) %>%
      leaflet::addCircleMarkers(
        radius = radius,
        fillColor = cols,
        fillOpacity = opacity,
        stroke = FALSE,
        popup = pas$popupText,
        layerId = pas$label
      ) %>%
      leaflet::addLegend(
        position = 'bottomright',
        colors = rev(colors), # show low levels at the bottom
        labels = rev(labels),  # show low levels at the bottom
        opacity = 1,
        title = legendTitle)
  }

  # ----- Return ---------------------------------------------------------------

  return(m)

}
