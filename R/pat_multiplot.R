#' @export
#' @import graphics
#' @title Display multiple plots on one page
#' @param ... ggplot objects 
#' @param pat Purple Air Timeseries "pat" object from \code{createPATimeseriesObject()}
#' @param plottype Quick-reference plot types: "all", "aux", "pm25"
#' @param cols Number of columns in the plot layout
#' @description # A plotting function that uses ggplot2 to display multiple 
#' ggplot objects in a single pane. Can either be passed individual ggplot 
#' objects or a pat object and a plot type. 
#' 
#' Typical usage would be to supply \code{pat} and use the \code{plottype} 
#' argument to quickly display preformatted plots. 
#' \itemize{
#' \item{"all": humidity, temperature, pm25_A, pm25_B}
#' \item{"aux": auxillary data (humidity, temperature)}
#' \item{"pm25": PM2.5 from Channel A and Channel B}
#' } 
#' @note Additional documentation of the multiplot algorithm is available at 
#' cookbook-r.com.

pat_multiplot <- function(
  pat = NULL, 
  plottype = NULL, 
  cols = NULL, 
  ...
) {
  
  options(warn = -1)
  
  # A very useful multiplot Function -------------------------------------------
  
  # The R-cookbook
  gg_multiplot <- function(..., plotlist = NULL, cols = 1, layout = NULL) {
    plots <- c(list(...), plotlist)
    numPlots <- length(plots)
    if ( is.null(layout) ) {
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    if ( numPlots == 1 ) {
      print(plots[[1]])
    } else {
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), 
                                                                   ncol(layout))) )
      for ( i in 1:numPlots ) {
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                              layout.pos.col = matchidx$col))
      }
    }
  }
  
  # ----------------------------------------------------------------------------
  
  if ( !is.null(pat) && !is.null(plottype) ) { 
    
    df <- 
      dplyr::tibble(datetime = pat$data$datetime,
                    pm25_A = pat$data$pm25_A, 
                    pm25_B = pat$data$pm25_B, 
                    humidity = pat$data$humidity, 
                    temp = pat$data$temperature)
    
    channelA <- 
      df %>% 
      ggplot2::ggplot(ggplot2::aes(x = .data$datetime, y = .data$pm25_A)) + 
      ggplot2::geom_point(size = 1, shape = 18, alpha = 1/10) + 
      ggplot2::ggtitle(expression("Channel A PM"[2.5])) + 
      ggplot2::xlab("Date") + ggplot2::ylab("\u03bc g / m\u00b3") 
    channelB <-   
      df %>% 
      ggplot2::ggplot(ggplot2::aes(x = .data$datetime, y = .data$pm25_B)) + 
      ggplot2::geom_point(size = 1, shape = 18, alpha = 1/10) + 
      ggplot2::ggtitle(expression("Channel B PM"[2.5])) + 
      ggplot2::xlab("Date") + ggplot2::ylab("\u03bc g / m\u00b3") 
    temperature <-   
      df %>% 
      ggplot2::ggplot(ggplot2::aes(x = .data$datetime, y = .data$temp)) + 
      ggplot2::geom_point(size = 1, shape = 18) + 
      ggplot2::ggtitle("Temperature") + 
      ggplot2::xlab("Date") + ggplot2::ylab("\u2103")
    humidity <-   
      df %>% 
      ggplot2::ggplot(ggplot2::aes(x = .data$datetime, y = .data$humidity)) + 
      ggplot2::geom_point(size = 1, shape = 18) + 
      ggplot2::ggtitle("Humidity") + 
      ggplot2::xlab("Date") + ggplot2::ylab("RH%")
    
    if ( plottype == "pm25" ) {
      gg_multiplot(channelA, channelB)
    } else if ( plottype == "aux" ) { 
      gg_multiplot(temperature, humidity, cols = 2)
    } else if ( plottype == "all") { 
      gg_multiplot(channelA, channelB, humidity, temperature, cols = 2)
    }
    
  }
  
  if ( length(list(...)) != 0 ) {
    gg_multiplot(plotlist = list(...))
  }
  
  options(warn=0)
  
}
