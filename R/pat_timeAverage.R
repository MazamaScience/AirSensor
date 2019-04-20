#' @keywords pa_timeseries
#' @export
#' @importFrom rlang .data
#' @title Time averages for PrupleAir time series
#' 
#' @param pat Purple Air Timeseries "pat" object from \code{createPATimeseriesObject()}
#' @param param Data to display: "pm25", "humidity", "temperature"
#' @param avg.time The time period to average to. Can be "sec", "min", "hour", 
#' "day", "DSTday", "week", "month", "quarter" or "year". For much increased 
#' flexibility a number can precede these options followed by a space (i.e. "2 days")
#' @param statistic The statistic to apply when aggregating the data; default is the 
#' mean. Can be one of "mean", "max", "min", "median", "frequency", "sd", "percentile".
#' @param showPlot Boolean option to show time averaged plot
#' @param plottype The type of plot to display. Can be "point", "boxplot", "ribbon".
#' @param ... optional ggplot2 parameters for plotting, such as color, size, shape, etc.
#'   
#' @description Function to flexibly aggregate or expand data frames by different
#' time periods, calculating vector-averages for a PurpleAir time series object. 
#' 
#' @return Something
#' 
#' @examples
#' \dontrun{
#' 
#' }

pat_timeAverage <- function(
  pat = NULL, 
  parameter = NULL,
  avg.time = NULL, 
  statistic = NULL, 
  showPlot = TRUE, 
  plottype = NULL, 
  ...
) { 
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !pat_isPat(pat) )
    stop("parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("parameter 'pat' has no data.")
  
  # ----- Time Average Fucntion ------------------------------------------------
  
  timeAverage <- function(df, avg.time, statistic) { 
    
    foo <- 
      df %>% 
      openair::timeAverage(
         avg.time = avg.time, 
         data.thresh = 0,
         statistic = statistic, 
         type = "default", 
         percentile = NA,
         start.date = NA, 
         end.date = NA, 
         interval = NA, 
         vector.ws = FALSE,
         fill = FALSE
        )
    
    return(foo)
    
  }
  
  # ----- Plot function --------------------------------------------------------
  
  plot_timeAverage <- function(avg_, plottype = NULL, title = NULL, ylab = NULL, ...) { 
    
    plot <- 
      avg_ %>% 
      ggplot2::ggplot(ggplot2::aes(x = .data$date, y = avg_[[2]])) + 
      ggplot2::labs(title = title, x = "Date", y = ylab) 
    
    if ( tolower(plottype) == "point" ) {
      
      print(plot + ggplot2::geom_point(...))
    
    } else if ( tolower(plottype) == "boxplot" ) {
    
      print(plot + ggplot2::geom_boxplot())
    
    }
    
  }
  
  # ----- Prepare Data ---------------------------------------------------------
  
  if ( tolower(parameter) == "pm25" ) {
    
    pm25_A <- 
      pat$data %>%       
      select(datetime_A, pm25_A) %>% 
      rename(date = datetime_A, pm25 = pm25_A) %>% 
      filter(!is.na(pm25))
    
    pm25_B <- 
      pat$data %>% 
      select(datetime_B, pm25_B) %>% 
      rename(date = datetime_B, pm25 = pm25_B) %>% 
      filter(!is.na(pm25))
    
    pm25 <- 
      bind_rows(pm25_A, pm25_B) %>% 
      arrange(date)
    
    avg_pm25 <- 
      timeAverage(pm25, statistic = statistic, avg.time = avg.time)
    
    if ( showPlot ) 
      plot_timeAverage(avg_pm25, plottype = plottype, ylab = "PM2.5", ...)
    
    return(avg_pm25)
    
  } else if ( tolower(parameter) == "temperature" || tolower(parameter) == "temp") {
     
    temperature <- 
      pat$data %>% 
      select(datetime, temperature) %>% 
      rename(date = datetime) %>%
      filter(!is.na(temperature))
    
    avg_temp <- 
      timeAverage(temperature, statistic = statistic, avg.time = avg.time)
    
    if ( showPlot ) 
      plot_timeAverage(avg_temp, plottype = plottype, ylab = "Temperature", ...)
    
    return(avg_temp)
  
  } else if ( tolower(parameter) == "humidity" ) { 
    
    humidity <- 
      pat$data %>% 
      select(datetime, humidity) %>% 
      rename(date = datetime) %>%
      filter(!is.na(humidity))
    
    avg_humidity <- 
      timeAverage(humidity, statistic = statistic, avg.time = avg.time)
    
    if ( showPlot ) 
      plot_timeAverage(avg_humidity, plottype = plottype, ylab = "Humidity", ...)
    
    return(avg_humidity)
    
  }

}
  
  