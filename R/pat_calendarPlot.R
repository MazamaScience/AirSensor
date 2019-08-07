#' @export 
#' @importFrom rlang .data
#' 
#' @title Calendar Heat Map Plot
#'
#' @param pat a pat object
#' @param palette a palette
#' @param ncol columns of plot
#' @param channel Data channel to use for PM2.5 -- one of "a", "b or "ab".
#' @param qc_algorithm Named QC algorithm to apply to hourly aggregation stats.
#' @param min_count Aggregation bins with fewer than `min_count` measurements
#' will be marked as `NA`.
#' 
#' @description Plot a calendar heat map of daily PM2.5 average.
#' 
#' Current QC algorithms exist for \code{channel = "ab"} and include:
#' \itemize{
#' \item{\code{hourly_AB_00}}
#' \item{\code{hourly_AB_01}}
#' }
#'
#' @note This function is currently optimized for annual time periods. 
#' 
#' @return ggobject
#' 
#' @seealso \link{PurpleAirQC_hourly_AB_00}
#' @seealso \link{PurpleAirQC_hourly_AB_01}
#' 
#' @examples
#' \donttest{
#' pat_calendarPlot(pat = example_pat)
#' }
#' 
pat_calendarPlot <- function(
  pat = NULL, 
  palette = NULL, 
  ncol = 3,
  channel = "ab", 
  qc_algorithm = "hourly_AB_01",
  min_count = 20
) {
  
  # ===== DEBUGGING ============================================================
  
  if ( FALSE ) {
    
    pat <- example_pat 
    palette <- NULL 
    ncol <- 3
    channel <- "ab"
    qc_algorithm <- "hourly_AB_01"
    min_count <- 20
    
  }
  
  # ----- Validate parameters --------------------------------------------------
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  
  # ----- Create hourly aggregated data ----------------------------------------
  
  airsensor <- 
    pat_createAirSensor(
      pat,
      period = "day", 
      parameter = "pm25",
      channel = channel, 
      qc_algorithm = qc_algorithm,
      min_count = min_count
    )
  
  # Creat data frame
  df <- airsensor$data
  
  # ----- Prepare plot data ----------------------------------------------------
  
  # Rename the data column to "pm25"
  names(df)[2] <- "pm25"
  
  # Create calendar plot handler data frame 
  df$datetime <- zoo::as.Date(df$datetime)  # format date
  df$day <- as.numeric(strftime(df$datetime, format = "%d"))
  df$yearmonth <- zoo::as.yearmon(df$datetime)
  df$yearmonthf <- factor(df$yearmonth)
  df$week <- as.numeric(strftime(df$datetime, format = "%W"))
  df$year <- as.numeric(strftime(df$datetime, format = "%Y"))
  df$month <- as.numeric(strftime(df$datetime, format = "%m"))
  df$monthf <- months.Date(df$datetime, abbreviate = TRUE)
  df$weekdayf <- weekdays.Date(df$datetime, abbreviate = TRUE)
  df$weekday <- as.numeric(strftime(df$datetime, format = "%d"))
  df$weekd <- ordered(df$weekdayf, levels=(c( "Mon", 
                                              "Tue", 
                                              "Wed", 
                                              "Thu", 
                                              "Fri", 
                                              "Sat", 
                                              "Sun"
                                              ) 
                                           )
                      )
  df$monthweek <- as.numeric(NA) # placeholder

  # Compute week number for each month                                          
  df <- 
    df %>%
    plyr::ddply(
      .variables = plyr::.(yearmonthf), 
      .fun = transform,
      monthweek = 1 + week - min(week) 
    )
  
  # Capture only whats needed
  df <- 
    df[, c( "year", 
            "yearmonthf", 
            "monthf", 
            "week", 
            "monthweek", 
            "weekdayf", 
            "weekd",
            "day",
            "pm25"
    )]
  
  # Create the title
  title <- paste0(
    unique(range(df$year)),
    ": ",
    pat$meta$label
  )
  
  # ----- Create plot ----------------------------------------------------------
  
  ggplot <- 
    ggplot2::ggplot(
      df, 
      ggplot2::aes(
        stats::reorder(monthweek, dplyr::desc(.data$monthweek)), 
        .data$weekd, 
        fill = df$pm25
      )
    ) + 
    ggplot2::geom_tile(color = "grey88", size=0.5) + 
    ggplot2::facet_wrap(drop = F, ncol = ncol, dir = "h",
                        factor(monthf, levels = month.abb) ~ .
    ) +
    ggplot2::labs(
      title = title,
      fill="PM2.5") + 
    ggplot2::geom_text(
      ggplot2::aes(label=.data$day), 
      size = 3, 
      fontface = "bold"
    ) +
    ggplot2::theme_classic() + 
    ggplot2::coord_flip() + 
    ggplot2::scale_fill_viridis_c(
      alpha = 0.8, 
      direction = 1, 
      begin = 0.15
    ) + 
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(), 
      axis.text.x = ggplot2::element_text(size = 7),
      axis.ticks.y = ggplot2::element_blank(), 
      axis.title.x = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      legend.position = "bottom",
      aspect.ratio = 4/5
    ) 
  
  return(ggplot)
  
}    

