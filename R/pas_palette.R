#' @export
#' 
#' @title Color palettes for PurpleAir
#' 
#' @description Generates color palettes for PurpleAir synoptic data with the 
#' intention of having a reproducible functional color generator. 
#'
#' @param pas Enhanced data frame of PurpleAir synoptic data.
#' @param pal A predefined color palette. Can be of the following:
#' \itemize{
#' \item{"AQI"}
#' \item{"humidity}
#' \item{"temperature}
#' \item{"distance"}
#' }
#' @return An object that consists of a label and color dataframe, and
#' calculated color values from PurpleAir sensors

pas_palette <- function(
  pas,
  pal,
  param = NULL,
  ...
) {
  
  options(warn = -1)
  
  if ( pal == "humidity" ) { # HUMIDITY
    
    colorFunc <- 
      leaflet::colorNumeric(
        "BrBG", 
        domain = c(0,100), 
        na.color = "grey50",
        ...
      )
    
    breaks <- seq(0,100,length.out=11)
    levels <- seq(5,95,length.out=10)
    
    colorBreaks <- 
      leaflet::colorBin(
        "BrBG", 
        domain=range(breaks), 
        bins=breaks, 
        ...)(levels)
    
    labels <- 
      c(
        '<10%',
        '10-20%',
        '20-30%',
        '30-40%',
        '40-50%',
        '50-60%',
        '60-70%',
        '70-80%',
        '80-90%',
        '>90%'
      )
    
    sensorColor <- colorFunc(pas$humidity)
    
  } else if ( pal == "temperature" ) { # TEMPERATURE
    
    colorFunc <- 
      leaflet::colorNumeric(
        "RdYlBu", 
        domain = c(-50,130), 
        na.color = "grey50", 
        ...
      )
    
    breaks <- seq(-20,120,length.out=15)
    levels <- seq(-15,115,length.out=14)
    
    colorBreaks <- 
      leaflet::colorBin(
        "RdYlBu", 
        domain=range(breaks), 
        bins=breaks, 
        ...)(levels)
    
    labels <- 
      c(
        '<-10',
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
        '>110'
      )
    
    sensorColor <- colorFunc(round(pas$temperature))
    
  } else if ( pal == "aqi" ) { # AQI COLORS
    
    colorFunc <- 
      leaflet::colorBin(
        PWFSLSmoke::AQI$colors, 
        bins = PWFSLSmoke::AQI$breaks_24, 
        na.color = "grey50"
      )
    
    colorBreaks <- PWFSLSmoke::AQI$colors
    
    labels <- PWFSLSmoke::AQI$names
    
    sensorColor <- colorFunc(pas$pm25_1hr)
    
  } else if ( tolower(pal == "distance") ) { # DISTANCE
    
    bins <- c(0,100,200,500,1000,2000,3000,4000,5000,10000)
    
    oranges <- rev(RColorBrewer::brewer.pal(9,'Oranges'))
    purples <- rev(RColorBrewer::brewer.pal(9,'Purples'))
    
    colorBreaks <- c(oranges[4:1],purples[3:7])
    
    colorFunc <- 
      leaflet::colorBin(
        colorBreaks, 
        domain = range(bins), 
        bins = bins,
        na.color = "grey50"
      )
    
    labels <- 
      c(
        '<100 m',
        '100-200 m',
        '200-500 m',
        '0.5-1 km',
        '1-2 km',
        '2-3 km',
        '3-4 km',
        '4-5 km',
        '5-10 km'
      )
    
    sensorColor <- colorFunc(pas$pwfsl_closestDistance)
    
  } else { # GENERIC COLOR FUNC
    
    colorFunc <- 
      leaflet::colorNumeric(
        palette = pal, 
        domain = c(0,200), 
        na.color = "grey50", 
        ... 
      )
    
    breaks <- seq(0,200,length.out=7)
    levels <- seq(5,195,length.out=6)
    
    colorBreaks <- 
      leaflet::colorBin(
        palette = pal, 
        domain=range(breaks), 
        bins=breaks, 
        ...)(levels)
    
    labels <- PWFSLSmoke::AQI$names
    
    if ( is.null(param) )( stop("Must provide a parameter."))
    
    sensorColor <- colorFunc(pas[[param]])
    
  }
  
  # ----- Return color palette -------------------------------------------------
  
  options(warn = 0)
  
  palette <- list(key = cbind(labels, colorBreaks),  colors = sensorColor)
  
  return(palette)
  
}
