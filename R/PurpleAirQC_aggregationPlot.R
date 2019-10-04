#' @export
#' @importFrom rlang .data
#' @importFrom grDevices rgb 
#' 
#' @title Plot the output from the pat_aggregateOutlierCounts() function
#' 
#' @param aggregationStats PurpleAir Timeseries \emph{aggregationStats} object.
#' @param parameterGroup Quick-reference plot types: "all", "humidity", 
#' "pm25_A", "pm25_B", "temperature" 
#' @param ylim Either "free_y" which scales automatically for each plot or 
#' "fixed" where the y limits of each plot are identical
#' 
#' @description A plotting function that uses ggplot2 to display a plot of each 
#' output category from the pat_aggregateOutlierCounts() function. Created to 
#' have a quick look at all the stats to help identify necessary quality control
#' methods on PurpleAir Timeseries \emph{aggregationStats} objects.
#' 


PurpleAirQC_aggregationPlot <- function(
  aggregationStats = NULL, 
  parameterGroup = "all",
  parameters = NULL,
  ylim = "fixed"
  
) {
  
  # ----- Validate paramters ---------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(aggregationStats)
  
  if ( is.null(parameters) ) {
    
  #------ Separate by groups of wanted data:
  if (parameterGroup == "all"){

    #------ Sorting names for plot order
    parameters <- sort(names(aggregationStats))
    parameters <- parameters[!parameters %in% c("pm25_df", "pm25_p", "pm25_t")]
    parameters <- append(parameters, c("pm25_df", "pm25_p", "pm25_t"))
    nrow <- 5
    
    
  } else if (parameterGroup == "humidity") {
    parameters <- c("humidity_count", "humidity_max", "humidity_mean", 
                    "humidity_median", "humidity_min", "humidity_outlierCount", 
                    "humidity_sd")
    nrow <- 3

    
  } else if (parameterGroup == "pm25_A") {
    parameters <- c( "pm25_A_count", "pm25_A_max", "pm25_A_mean",
                     "pm25_A_median", "pm25_A_min", "pm25_A_outlierCount", 
                     "pm25_A_sd")
    nrow <- 3

    
  } else if (parameterGroup == "pm25_B") {
    parameters <- c("pm25_B_count", "pm25_B_max", "pm25_B_mean", 
                    "pm25_B_median", "pm25_B_min", "pm25_B_outlierCount", 
                    "pm25_B_sd")
    nrow <- 3

  } else if (parameterGroup == "temperature") {
    parameters <- c( "temperature_count", "temperature_max", "temperature_mean", 
                    "temperature_median", "temperature_min", 
                    "temperature_outlierCount", "temperature_sd")
    nrow <- 3
  }
    
    parameters <- unique(c("datetime", parameters))
    subset <- aggregationStats[,parameters]
    data_long <- subset %>%
      gather(param, value, -datetime) 
    param <- factor(data_long$param, levels = parameters)
  } 
  
  #------ Custom parameter viewing
  
  else if (length(setdiff(parameters, names(aggregationStats))) == 0) {
    parameters <- unique(c("datetime", parameters))
    subset <- aggregationStats[,parameters]
    data_long <- subset %>%
      gather(param, value, -datetime)
    param <- factor(data_long$param)
    nrow <- length(parameters)
  }
  
  #------ Ylim assignments
  if (ylim == "fixed") {
    scales <- "fixed"
    
  }else if ( ylim == "free_y"){
    scales <- "free_y"
  }
  
  #------ Plot
  gg <- ggplot(data_long, aes(x = datetime, y = value)) +
    geom_line() +
    labs(title="Aggregation Statistics") +
    #facet_wrap(~param, scales = "free_y")
    #facet_wrap(param, nrow = nrow, scales = "free_y" )
    facet_wrap(param, nrow = nrow, scales = scales ) 
  
  #------ Return, what do we return here?
  return(gg)
  
  
}

