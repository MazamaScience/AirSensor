#' @keywords pa_timeseries
#' @export
#' @importFrom rlang .data
#' @title Time averages for PrupleAir time series
#' 
#' @param pat Purple Air Timeseries "pat" object from \code{createPATimeseriesObject()}
#' @param parameter Data to display: "pm25", "humidity", "temperature"
#' @param sampleSize Either an integer or fraction to determine sample size
#' @param title title text
#' @param xlab optional title for the x axis
#' @param ylab optional title for the y axis
#' @param tlim optional vector with start and end times (integer or character
#'   representing YYYYMMDD[HH])
#' @param rollPeriod rolling mean to be applied to the data
#' @param showLegend logical to toggle display of the legend
#' @param colors string vector of colors to be used for plotting
#' 
#' @description Function to flexibly aggregate or expand data frames by different
##' time periods, calculating vector-averaged ---
#' 
#' The list of available parameters include:
#' 
#' \itemize{
#' \item{\code{pm25} -- A and B channel PM2.5 (ug/m3)}
##' \item{\code{temperature} -- temperature (F)}
#' \item{\code{humidity} -- humidity (\%)}
#' }
#' 
#' @return Something
#' 
#' @examples
#' \dontrun{
#' pas <- example_pas
#' nb <- pat_load(pas, "North Bend Weather", startdate = 20180801, enddate = 20180901)
#' subset_nb <- pat_sample(pat=nb, sampleSize = 1000, setSeed = 1)
#' pat_dygraph(pat = subset_nb, xlab = "2018", rollPeriod = 7)
#' }

pat_timeAverage <- function(
  pat = NULL, 
  param = NULL,
  avg.time = NULL, 
  stat = NULL, 
  show.plot = TRUE, 
  plot.type = NULL, 
  ...
) { 
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.")
  
  # ----- Time Average Fucntion ------------------------------------------------
  
  timeAverage <- function(df, avg.time, statistic) { 
    
    foo <- 
      df %>% 
      openair::timeAverage(
         avg.time = avg.time, 
         data.thresh = 0,
         statistic = stat, 
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
  
  plot_timeAverage <- function(avg_, plot.type = NULL, title = NULL, ylab = NULL, ...) { 
    
    plot <- 
      avg_ %>% 
      ggplot2::ggplot(ggplot2::aes(x = .data$date, y = avg_[[2]])) + 
      ggplot2::labs(title = title, x = "Date", y = ylab) 
    
    if ( tolower(plot.type) == "point" ) {
      
      print(plot + ggplot2::geom_point(...))
    
    } else if ( tolower(plot.type) == "boxplot" ) {
    
      print("no")
    
    }
    
  }
  
  # ----- Prepare Data ---------------------------------------------------------
  
  if ( tolower(param) == "pm25" ) {
    
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
      timeAverage(pm25, stat = stat, avg.time = avg.time)
    
    if ( show.plot ) 
      plot_timeAverage(avg_pm25, plot.type = "point", ylab = "PM2.5", ...)
    
    return(avg_pm25)
    
  } else if ( tolower(param) == "temperature" || tolower(param) == "temp") {
     
    temperature <- 
      pat$data %>% 
      select(datetime, temperature) %>% 
      rename(date = datetime) %>%
      filter(!is.na(temperature))
    
    avg_temp <- 
      timeAverage(temperature, stat = stat, avg.time = avg.time)
    
    return(avg_temp)
  
  } else if ( tolower(param) == "humidity" ) { 
    
    humidity <- 
      pat$data %>% 
      select(datetime, humidity) %>% 
      rename(date = datetime) %>%
      filter(!is.na(humidity))
    
    avg_humidity <- 
      timeAverage(humidity, stat = stat, avg.time = avg.time)
    
    if ( show.plot ) ( plot_timeAverage(avg_humidity) )
    
    return(avg_humidity)
    
  }

}
  
  