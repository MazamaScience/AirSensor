#' @export
#' @importFrom rlang .data
#' @import graphics
#' 
#' @title Display multiple plots on one page
#' 
#' @param pat Purple Air Timeseries "pat" object from \code{createPATimeseriesObject()}
#' @param plottype Quick-reference plot types: "all", "aux", "pm25"
#' @param plotlist a list() of any number of ggplot objects to plot on a single pane
#' @param cols Number of columns in the plot layout
#' 
#' @description # A plotting function that uses ggplot2 to display multiple 
#' ggplot objects in a single pane. Can either be passed individual ggplot 
#' objects or a pat object and a plot type. 
#' Typical usage would be to supply \code{pat} and use the \code{plottype} 
#' argument to quickly display preformatted plots. 
#' \itemize{
#' \item{"all": pm25_A, pm25_B, temperature, humidity}
#' \item{"pm25": PM2.5 from Channel A and Channel B}
#' \item{"aux": auxillary data (temperature, humidity)}
#' } 
#' 
#' @note Additional documentation of the multiplot algorithm is available at 
#' cookbook-r.com.

pat_multiplot <- function(
  pat = NULL, 
  plottype = "all", 
  plotlist = NULL,
  cols = NULL
) {
  
  options(warn = -1)
  
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
      multi_ggplot(channelA, channelB, cols = 1)
    } else if ( plottype == "aux" ) { 
      multi_ggplot(temperature, humidity, cols = 1)
    } else if ( plottype == "all") { 
      multi_ggplot(channelA, humidity, channelB, temperature, cols = 2)
    }
    
  }
  
  if ( length(plotlist) != 0 ) {
    
    if ( is.null(cols) )( cols = length(plotlist) )
    
    multi_ggplot(plotlist = plotlist, cols = cols)
  }
  
  options(warn=0)
  
}
