#' @export
#' @importFrom rlang .data
#' @importFrom grDevices rgb 
#' 
#' @title Plot the output from the pat_aggregateOutlierCounts() function
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param plottype Quick-reference plot types: "all", "humidity", "pm25_A", 
#' "pm25_B", "temperature" 
#' @param ylim Either "free_y" which scales automatically for each plot or 
#' "fixed" where the y limits of each plot are identical
#' 
#' @description A plotting function that uses ggplot2 to display a plot of each 
#' output category from the pat_aggregateOutlierCounts() function. Created to 
#' have a quick look at all the stats to help identify necessary quality control
#' methods on PurpleAir Timeseries \emph{pat} objects.
#' 


pat_aggregate_statPlot <- function(
  pat = NULL, 
  plottype = "all",
  ylim = "fixed"
  
) {
  
  #------ Sorting names for plot order
  a <- sort(names(pat))
  a <- a[!a %in% c("pm25_df", "pm25_p", "pm25_t")]
  a <- append(a, c("pm25_df", "pm25_p", "pm25_t"))
  
  #------ Separate by groups of wanted data:
  if (plottype == "all"){
    data_long <- pat %>%
      gather(param, value, -datetime)  
    param <- factor(data_long$param, levels = a)
    nrow <- 5
    
  } else if (plottype == "humidity") {
    pat  = select(pat, datetime, humidity_count, humidity_max, humidity_mean, humidity_median,
                  humidity_min, humidity_outlierCount, humidity_sd)
    data_long <- pat %>%
      gather(param, value, -datetime) 
    param <- factor(data_long$param)
    nrow <- 3
    
  } else if (plottype == "pm25_A") {
    pat  = select(pat, datetime, pm25_A_count, pm25_A_max, pm25_A_mean, pm25_A_median,
                  pm25_A_min, pm25_A_outlierCount, pm25_A_sd)
    data_long <- pat %>%
      gather(param, value, -datetime) 
    param <- factor(data_long$param)
    nrow <- 3
    
  } else if (plottype == "pm25_B") {
    pat  = select(pat, datetime, pm25_B_count, pm25_B_max, pm25_B_mean, pm25_B_median,
                  pm25_B_min, pm25_B_outlierCount, pm25_B_sd)
    data_long <- pat %>%
      gather(param, value, -datetime) 
    param <- factor(data_long$param)
    nrow <- 3
    
  } else if (plottype == "temperature") {
    pat  = select(pat, datetime, temperature_count, temperature_max, temperature_mean, 
                  temperature_median, temperature_min, temperature_outlierCount, 
                  temperature_sd)
    data_long <- pat %>%
      gather(param, value, -datetime) 
    param <- factor(data_long$param)
    nrow <- 3
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

