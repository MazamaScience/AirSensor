#' @title Calendar Heat Map Plot
#'
#' @param pat a pat object
#' @param palette a palette
#' @param ncol columns of plot
#' @description Plot a calendar heat map of daily PM2.5 average. Can be any 
#' duration, but optimized for annual analysis. 
#' @return ggobject
#' @export 
#' @examples
#' \donttest{
#' pat_calendarPlot(pat = example_pat)
#' }
pat_calendarPlot <- 
  function(pat, palette = NULL, ncol = 2) {
    
  # Creat AST
    ast <- 
      pat_createAirSensor(
        pat,
        period = "day", 
        parameter = "pm25", 
        channel = "ab", 
        min_count = NULL
      )
    
    # Creat data frame
    df <- ast$data
    
    # Find label
    label <- names(df)[[2]]
    
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
                                                ) ))
    
    # Compute week number of month                                          
    df <- 
      plyr::ddply(
        .data = df,
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
              label
      )]

    # Plot
    plot <- 
      ggplot2::ggplot(
        df, 
        ggplot2::aes(
          reorder(monthweek, dplyr::desc(monthweek)), 
          weekd, 
          fill = df[[label]]
        )
      ) + 
      ggplot2::geom_tile(color = "grey88", size=0.5) + 
      ggplot2::facet_wrap(drop = F, ncol = ncol, dir = "v",
                          factor(monthf, levels = month.abb) ~ .
      ) +
      ggplot2::labs(
        title = unique(range(df$year)),
        fill="PM2.5") + 
      ggplot2::geom_text(
        ggplot2::aes(label=day), 
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
    
    return(plot)
    
  }    

