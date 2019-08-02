#'
#' @export
#' 
#' @title Leaflet interactive map for use with AirShiny
#' 
#' @description This function creates interactive maps that will be displayed in 
#' AirShiny web-app. 
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
#' @param parameter Value to plot, e.g. \code{pm25_1hr}.
#' @param paletteName A predefined color palette name. 
#' @param radius Radius (pixels) of monitor circles.
#' @param opacity Opacity of monitor circles.
#' @param maptype Optional name of leaflet ProviderTiles to use, e.g. \code{terrain}. 
#' 
#' @return A leaflet "plot" object which, if not assigned, is rendered in 
#' Rstudio's 'Viewer' tab.
#' 

AirShiny_leaflet <- function(
  pas = NULL,
  parameter = "pm25_1hr",
  paletteName = NULL,
  radius = 11,
  opacity = 0.8,
  maptype = "terrain"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !("data.frame" %in% class(pas)) ) 
    stop(paste0("First argument is not of class 'data.frame'."))
  
  if ( nrow(pas) == 0 || ncol(pas) == 0 )
    stop(paste0("One or both dimensions of the pa_synoptic object has length 0."))
  
  if ( !is.numeric(radius) )
    stop(paste0('radius parameter is non-numeric'))
  
  colorInfo <- pas_palette(
    pas, 
    paletteName = paletteName, 
    parameter = parameter, 
    reverse=TRUE
  )
  
  cols <- colorInfo$colors
  
  # # Create popupText
  # pas$popupText <- 
  #   paste0(
  #     "<b>", pas$label, "</b><br/>",
  #     pas$statsLastModifiedDate, "<br/>",
  #     "PM2.5 = ", round(pas$pm25_1day, 1), " \U00B5g/m3<br/>",
  #     "Temperature = ", round(pas$temperature, 0), " F<br/>",
  #     "Humidity = ", round(pas$humidity, 0), "%<br/>"
  #   )
  
  # Extract view information
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
  
  # Convert locations to SpatialPointsDataFrame
  pas <- pas[!is.na(pas$latitude),]
  
  pas <- pas[which(!stringr::str_detect(pas$label, " B")),]
  SPDF <- 
    sp::SpatialPointsDataFrame(
      data = as.data.frame(pas),
      coords = cbind(
        pas$longitude, 
        pas$latitude
      )
    )
  
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
  map <- 
    leaflet::leaflet(SPDF)
  
  map <-  
    leaflet::setView(
      map, 
      lng=mean(lonRange), 
      lat=mean(latRange),
      zoom=zoom
    )
  map <-
    leaflet::addProviderTiles(map, providerTiles)
  
  map <-
    leaflet::addCircleMarkers(
      map,
      radius=radius,
      fillColor=cols,
      fillOpacity=opacity,
      stroke=FALSE,
      # popup=pas$popupText,
      layerId = pas$label
    )
  
  return(map)
  
}

#' 
#' @export
#' @title Bar plot for AirShiny
#' 
#' @param pat PurpleAir Timeseries "pat" object from \code{pat_createNew()}
#' @param period The time period to average to. Can be "sec", "min", "hour", 
#' "day", "DSTday", "week", "month", "quarter" or "year". A number can also
#'  precede these options followed by a space (i.e. "2 day" or "37 min").
#' @param startdate The start date. Used to provide the Datetime domain.
#' @param enddate The end date. Used to provide the Datetime domain.
#' @param ylim Y-axis limits. 
#' 
#' @description A barplot that bins by period and preforms an average of 
#' Channel A & B, or seperately. Intended for use with the AirShiny web app.  
#' @details The \code{ylim} is used to fix the Y-axis domain in order to have 
#' consistent visualization.The X-axis is determined by the 
#' datetime interval. 
#' 
#' 

AirShiny_barplot <- 
  function(
    pat, 
    period, 
    startdate, 
    enddate, 
    ylim = NULL
    ) { 

    # ----- Validate parameters ------------------------------------------------
    
    if ( pat_isEmpty(pat) ) 
      stop("Required Purple Air time series is missing") 
    
    if ( !pat_isPat(pat) )
      stop("Required parameter 'pat' is not a valid 'pa_timerseries' object.")
    
    logger.debug(" # AirShiny_barplot() # ")
    
    ast <- 
      AirSensor::pat_createAirSensor(
        pat = pat, 
        period = period, 
        channel = "ab"
      )
    
    # Create color palette 
    palette <- grDevices::colorRampPalette(colors = rev(c("#9733ee", "#da22ff")))

    # Plot
    pm25_plot <- 
      ast$data %>% 
      ggplot2::ggplot(
        ggplot2::aes(
          x = lubridate::interval(
            startdate, 
            enddate, 
            tzone = "America/Los_Angeles" 
          ), 
          y = ast$data[[2]], 
          colours = ast$data[[2]]
        )
      ) +
      ggplot2::ggtitle(
        label = "PM2.5"
      ) + 
      ggplot2::coord_cartesian(ylim = ylim) + 
      ggplot2::xlab("Datetime") + 
      ggplot2::ylab("\u03bcg / m\u00b3") + 
      ggplot2::theme_minimal() + 
      ggplot2::scale_fill_gradientn(colors = palette(100), limits = c(0, 200)) + 
      ggplot2::scale_x_datetime(date_breaks = "1 day") + 
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = ifelse(enddate - startdate > 10, 25, 0), 
          hjust = 1, 
          size = 12), 
        axis.text.y = ggplot2::element_text(size = 12),
        axis.title.x = ggplot2::element_text(size = 14),
        axis.title.y = ggplot2::element_text(size = 14)
      )
    
    
    
    # Average PM2.5 barplot
    pm25_avg_bar <- 
      ggplot2::geom_bar(                   
        data = ast$data,
        mapping = ggplot2::aes(
          x = .data$datetime, 
          y = ast$data[[2]],
          fill = ast$data[[2]]), 
        stat = "identity",
        color = "gray95", 
        show.legend = FALSE
      )
    
    gg <- 
      pm25_plot + 
      pm25_avg_bar
     
    
    return(gg)
    
  }
