#' @export
#' @import graphics
#' 
#' @title Matrix scatter plot variables in a data frame
#' 
#' @description Creates a multi-panel scatterplot comparing all variables in the
#' data frame object. If any variables have not valid data, they are omitted 
#' from the plot.
#' 
#' @param data data frame
#' @param parameters the columns of the data frame to plot
#' @param sampleSize the integer sample number of rows 
#' @param sampleFraction the fractional sample of rows 
#' @param shape symbol to use for points
#' @param size size of points
#' @param color color of points
#' @param alpha opacity of points
#'

scatterplot <- function(
  data,  
  parameters = NULL, 
  sampleSize = 5000,
  sampleFraction = NULL,
  shape = 18, 
  size = 1.5, 
  color = "black", 
  alpha = 0.5
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(data)
  
  # ----- Allow parameter selection --------------------------------------------
  
  if ( !is.null(parameters) ) {
    
    # Validation 
    if ( !all(parameters %in% names(data)) ) {
      
      paramString <- paste(parameters, ",")
      namesString <- paste(names(data), ",")
      stop(paste0("Ivalid parameter in: ", paramString, 
                  "\nAvailable parameters include: ", namesString))
      
    } else {
      
      data <- 
        data %>% 
        dplyr::select(parameters) 
      
    }
    
  }
  
  # ----- Sample if large ------------------------------------------------------
  
  data <- 
    .sample(
      data = data,
      sampleSize = sampleSize, 
      sampleFraction = sampleFraction
    )
  
  # ----- Create plot ----------------------------------------------------------
  
  gg_scatterplot <- 
    GGally::ggpairs( 
      data,
      mapping = ggplot2::aes(alpha = 0.15),
      lower = list(
        continuous = GGally::wrap(
          "points", 
          size = size, 
          shape = shape,
          color = color,
          alpha = alpha)),
      diag = list(
        continuous = GGally::wrap(
          "densityDiag")), 
      upper = list(continuous = "cor")
    ) + 
    ggplot2::theme_bw()
  
  return(gg_scatterplot)
  
}

#' @export
#' @importFrom rlang .data
#' @import graphics
#' @title Display multiple plots on one page
#' @param ... any number of ggobjects to be plotted
#' @param plotList a list() of any number of ggplot objects to plot on a single pane
#' @param cols Number of columns in the plot layout
#' @param layout A matrix specifying the layout. If present, 'cols' is ignored.
#' 
#' @description # A plotting function that uses ggplot2 to display multiple 
#' ggplot objects in a single pane. 
#' 
#' @note Additional documentation of the multiplot algorithm is available at 
#' cookbook-r.com.

multi_ggplot <- function(
  ..., 
  plotList = NULL, 
  cols = 1, 
  layout = NULL
) {
  
  plots <- c(list(...), plotList)
  numPlots <- length(plots)
  
  if ( is.null(layout) ) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if ( numPlots == 1 ) {
    print(plots[[1]])
    
  } else {
    grid::grid.newpage()
    grid::pushViewport(
      grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))) )
    
    for ( i in 1:numPlots ) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
    }
  }
  
  # TODO:  Does multi_ggplot() return anything?
  
} 

#' @export
#' 
#' @title Color palettes for PurpleAir
#' 
#' @description Generates color palettes for PurpleAir synoptic data with the 
#' intention of having a reproducible functional color generator. 
#'
#' @param pas Enhanced data frame of PurpleAir synoptic data.
#' @param paletteName A predefined color palette name. Can be of the following:
#' \itemize{
#' \item{"AQI"}
#' \item{"humidity}
#' \item{"temperature}
#' \item{"distance"}
#' }
#' @param parameter Value to generate colors for, e.g. \code{pm25_1hr}.
#' @param ... Additional arguments passed on to \code{leaflet::color~} functions.
#' 
#' @return An object that consists of a label and color dataframe, and
#' calculated color values from PurpleAir sensors

pas_palette <- function(
  pas = NULL,
  paletteName = "AQI",
  parameter = "pm25_1hr",
  ...
) {
  
  options(warn = -1)
  
  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(pas)
  
  validPaletteNames <- c("aqi", "humidity", "temperature", "distance")
  
  if ( (!tolower(paletteName) %in% validPaletteNames) && is.null(parameter) ) {
    stop("Parameter 'parameter' is required for generic palette names.")
  }
  
  # ----- Create color/legend info ---------------------------------------------
  
  if ( tolower(paletteName) == "humidity" ) { # HUMIDITY
    
    colorFunc <- 
      leaflet::colorNumeric(
        "BrBG", 
        domain = c(0,100), 
        na.color = "#bbbbbb",
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
    
  } else if ( tolower(paletteName) == "temperature" ) { # TEMPERATURE
    
    colorFunc <- 
      leaflet::colorNumeric(
        "RdYlBu", 
        domain = c(-50,130), 
        na.color = "#bbbbbb", 
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
    
  } else if ( tolower(paletteName) == "aqi" ) { # AQI COLORS
    
    colorFunc <- 
      leaflet::colorBin(
        PWFSLSmoke::AQI$colors, 
        bins = PWFSLSmoke::AQI$breaks_24, 
        na.color = "#bbbbbb"
      )
    
    colorBreaks <- PWFSLSmoke::AQI$colors
    
    labels <- PWFSLSmoke::AQI$names
    
    sensorColor <- colorFunc(pas[[parameter]])
    
  } else if ( tolower(paletteName) == "distance" ) { # DISTANCE
    
    bins <- c(0,100,200,500,1000,2000,3000,4000,5000,10000)
    
    oranges <- rev(RColorBrewer::brewer.pal(9,'Oranges'))
    purples <- rev(RColorBrewer::brewer.pal(9,'Purples'))
    
    colorBreaks <- c(oranges[4:1],purples[3:7])
    
    colorFunc <- 
      leaflet::colorBin(
        colorBreaks, 
        domain = range(bins), 
        bins = bins,
        na.color = "#bbbbbb"
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
        palette = paletteName, 
        domain = c(0,200), 
        na.color = "#bbbbbb", 
        ... 
      )
    
    breaks <- seq(0,200,length.out=7)
    levels <- seq(5,195,length.out=6)
    
    colorBreaks <- 
      leaflet::colorBin(
        palette = paletteName, 
        domain=range(breaks), 
        bins=breaks, 
        ...)(levels)
    
    labels <- PWFSLSmoke::AQI$names
    
    sensorColor <- colorFunc(pas[[parameter]])
    
  }
  
  # ----- Return colorInfo -----------------------------------------------------
  
  options(warn = 0)
  
  colorInfo <- list(key = cbind(labels, colorBreaks),  colors = sensorColor)
  
  return(colorInfo)
  
}
