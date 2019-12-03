#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains 
#' 
#' @title Daily State of Health metric plot
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' 
#' @description This function plots as subset of the most useful State of Health 
#' metrics calculated by the \code{pat_dailySoH} function. The function 
#' runs \code{pat_dailySoH} internally and uses the output to create 
#' the plot.
#' 
#' 
#' @examples
#' 
#' pat_dailySoHIndexPlot(example_pat_failure_A)
#' 
#' 


pat_dailySoHIndexPlot <- function(
  pat = NULL
) {
  
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  
  # ----- Create the SoH object, and SoH plot ----------------------------------
  
  # calculate the SoH_index
  index <- pat_dailySoHIndex(pat)
  
  station_name <- pat$meta$label
  
  colors <- factor(index$SoH_index_bin)
  
  #calculate the plot offset for plotting location of the index
  plot_offset <- 0.05*(max(pat$data$pm25_A, pat$data$pm25_B, na.rm = TRUE))
  
  # NOTES: the value of the daily index bin is represented with color so the 
  # NOTES: value is multiplied by 0 since all that matters for display is color 
  # NOTES: and datetime. The index is also shifted to the right by 12 hours 
  # NOTES: because the symbol is a lorge square that is plotted (centered) at 
  # NOTES: midnight for the day, so to make the square better represent the time
  # NOTES: chunk for the day, we shift the symbol to be centered at noon for the day.
  gg <- ggplot2::ggplot(pat$data) +
    ggplot2::geom_point(aes(pat$data$datetime, pat$data$pm25_A), 
               color= "red",
               pch = 16, 
               cex = 1) +
    ggplot2::geom_point(aes(pat$data$datetime, pat$data$pm25_B), 
               color= "blue",
               pch = 16, 
               cex = 1) +
    ggplot2::geom_point(data = index, 
               aes(index$datetime+lubridate::dhours(12), (index$SoH_index_bin*0)-plot_offset, color = colors), 
               pch = 15, 
               cex =5) +
    ggplot2::scale_color_manual(values=c("0" = "firebrick", 
                                "1" = "goldenrod1", 
                                "2" = "mediumseagreen")) +
    ggplot2::labs(title = paste0("SoH Index - ", station_name)) +
    ggplot2::xlab("datetime")+
    ggplot2::ylab("\u03bcg / m\u00b3") +
    ggplot2::labs(color = "Daily \nIndex Bin")
  
  
  
  # ----- Return -------------------------------------------------------------
  return(gg)
  
}



