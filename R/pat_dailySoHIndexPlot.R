#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains 
#' 
#' @title Daily State of Health metric plot
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param minPctReporting Percent reporting threshold for A and B channels.
#' @param breaks Breaks used to convert index values into index bins.
#' @param SoHIndex_FUN Function used to create \code{SoHIndex} tibble. (Not quoted.)
#' 
#' @description This function plots a subset of the most useful State of Health 
#' metrics calculated with \code{SoHIndex_FUN}.
#' 
#' Both \code{minPctReporting} and \code{breaks} are passed to 
#' \code{SoHIndex_FUN}.
#' 
#' @seealso \link{pat_dailySoHIndex_00}
#' 
#' @examples
#' library(AirSensor)
#' 
#' pat_dailySoHIndexPlot(example_pat_failure_A)
#' 

pat_dailySoHIndexPlot <- function(
  pat = NULL,
  minPctReporting = 50,
  breaks = c(0, .2, .8, 1),
  SoHIndex_FUN = pat_dailySoHIndex_00
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  MazamaCoreUtils::stopIfNull(minPctReporting)
  MazamaCoreUtils::stopIfNull(breaks)
  MazamaCoreUtils::stopIfNull(SoHIndex_FUN)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  
  
  # ----- Create the SoHIndex object -------------------------------------------
  
  # Calculate the SoH_index
  SoHIndex <- SoHIndex_FUN(
    pat = pat,
    minPctReporting = minPctReporting,
    breaks = breaks
  )
  
  # ----- Create plot variables ------------------------------------------------
  
  localTime <- lubridate::with_tz(pat$data$datetime, tzone = pat$meta$timezone)
  
  colors <- c("firebrick", "goldenrod1", "seagreen3")
  fillColors <- colors[SoHIndex$index_bin]
  
  sensorLabel <- pat$meta$label
  
  # Calculate locations the index color bar
  dataRange <- max(pat$data$pm25_A, pat$data$pm25_B, na.rm = TRUE)
  indexBar_thickness <- 0.025*(dataRange)
  
  # Calculate locations of the legend bars
  tlim <- range(localTime)
  dayCount <- as.numeric(difftime(tlim[2], tlim[1], units = "days"))
  legendBar_xmin <- tlim[2] - lubridate::ddays(dayCount/20)
  legendBar_xmax <- tlim[2] + lubridate::ddays(dayCount/20)
  legendBar_ymin <- c(0.60, 0.64, 0.68) * dataRange
  legendBar_ymax <- legendBar_ymin + indexBar_thickness
  
  legendText_x <- tlim[2]
  legendText_y <- legendBar_ymin
  legendText_label <- c("Poor", "Fair", "Good")
  
  xlab <- strftime(localTime[1], "Local Time (%Z)")
  
  # ----- Create plot ----------------------------------------------------------
  
  gg <- 
    # A and B channel PM2.5
    ggplot2::ggplot(pat$data) +
    ggplot2::geom_point(aes(localTime, pat$data$pm25_A), 
                        color= "red", 
                        pch = 16, 
                        cex = 0.5) +
    ggplot2::geom_point(aes(localTime, pat$data$pm25_B), 
                        color= "blue", 
                        pch = 16, 
                        cex = 0.5) +
    
    # Add State-of-Health bin colors underneath
    ggplot2::annotate(
      "rect",
      xmin = SoHIndex$datetime,
      xmax = SoHIndex$datetime + lubridate::dhours(24),
      ymin = (SoHIndex$index_bin*0)-2*indexBar_thickness,
      ymax = (SoHIndex$index_bin*0)-indexBar_thickness,
      fill = fillColors,
      alpha = 1.0
    ) +

    # Add SoH legend boxes on the right
    ggplot2::annotate(
      "rect",
      xmin = legendBar_xmin,
      xmax = legendBar_xmax,
      ymin = legendBar_ymin,
      ymax = legendBar_ymax,
      fill = colors,
      alpha = 1.0
    ) +
    
    # Add SoH legend boxes text
    ggplot2::annotate(
      "text",
      x = legendText_x,
      y = legendText_y,
      label = legendText_label,
      vjust = -0.2,
      color = "white"
    ) +
    
    # Add SoH legend title
    ggplot2::annotate(
      "text",
      x = legendText_x,
      y = legendText_y[3] + 2*indexBar_thickness,
      label = "Sensor Performance",
      vjust = 0.5,
      color = "black"
    ) +
    
    # Title and axis labels
    ggplot2::labs(title = paste0("SoH Index - ", sensorLabel)) +
    ggplot2::xlab(xlab)+
    ggplot2::ylab("\u03bcg / m\u00b3")

  # ----- Return ---------------------------------------------------------------
  
  return(gg)
  
}
