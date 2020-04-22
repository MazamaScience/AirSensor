#' @export
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes_ geom_point geom_line geom_area labs facet_wrap 
#' 
#' @title Faceted plot of a timeseries tibble 
#' 
#' @param tbl Tibble with a \code{datetime}.
#' @param pattern Pattern used to match groups of parameters.
#' @param parameters Custom vector of aggregation parameters to view.
#' @param nrow Number of rows in the faceted plot.
#' @param ncol Number of columns in the faceted plot.
#' @param autoRange Logical specifying whether to scale the y axis separately
#' for each plot or to use a common y axis.
#' @param ylim Vector of (lo,hi) y-axis limits.
#' @param style Style of plot: ("point", "line", "area") 
#' 
#' @description A plotting function that uses ggplot2 to display a suite of
#' timeseries plots all at once.
#' 
#' @note Specification of \code{ylim} will override the choice of 
#' \code{autoRange}.
#' 
#' @examples 
#' 
#' tbl <- pat_aggregateOutlierCounts(example_pat_failure_A)
#' timeseriesTbl_multiplot(tbl, 
#'                         pattern = "humidity_m|temperature_m", 
#'                         nrow = 2)
#' 

timeseriesTbl_multiplot <- function(
  tbl = NULL, 
  pattern = NULL,
  parameters = NULL,
  nrow = NULL,
  ncol = NULL,
  autoRange = TRUE,
  ylim = NULL,
  style = "line"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(tbl)
  
  # Check for existence of "datetime"
  if ( !"datetime" %in% names(tbl) || (!"POSIXct" %in% class(tbl$datetime)) )
    stop("Parameter 'tbl' must have a column named 'datetime' of class 'POSIXct'.")
  
  # Check for existence of parameters
  if ( !is.null(parameters) ) {
    unknownParameters <- setdiff(parameters, names(tbl))
    if ( length(unknownParameters) > 0 ) {
      parameterString <- paste0(unknownParameters, collapse = ", ")
      err_msg <- paste0("Parameters not found in tbl:\n\t",
                        parameterString)
      stop(err_msg)
    }
  }
  
  if ( is.null(nrow) && is.null(ncol) )
    ncol <- 1
  
  # ----- Determine parameters to plot -----------------------------------------
  
  if ( is.null(parameters) ) {
    
    parameters <- sort(names(tbl))
    
    # Special ordering for aggegationStatus
    if ( all( c("pm25_df", "pm25_p", "pm25_t") %in% parameters ) ) {
      # Start by sorting names alphabetically but put pm25_~ at the end
      parameters <- parameters[!parameters %in% c("pm25_df", "pm25_p", "pm25_t")]
      parameters <- append(parameters, c("pm25_df", "pm25_p", "pm25_t"))
    }
    
    # Subset if requested
    if ( !is.null(pattern) ) {
      parameters <- stringr::str_subset(parameters, pattern)
    }
    
  }
  
  # Otherwise just use the incoming parameters
  
  # Make sure 'datetime' is included, but only once
  parameters <- unique(c("datetime", parameters))
  
  tidyData <- 
    tbl[,parameters] %>%
    tidyr::gather("parameter", "value", -.data$datetime)
  
  # ----- Create plot ----------------------------------------------------------
  
  # Y axis
  if ( autoRange && is.null(ylim) ) {
    scales <- "free_y"
  } else {
    scales <- "fixed"
  }
  
  # Facets
  facets <- factor(tidyData$parameter, levels = parameters)
  
  # NOTE:  Using ggplot in a package requires special attention:
  # NOTE:    https://ggplot2.tidyverse.org/reference/aes_.html
  # NOTE:    https://bookdown.org/rdpeng/RProgDA/non-standard-evaluation.html
  
  gg <- 
    ggplot(tidyData, aes_(x = ~datetime, y = ~value))
  
  if ( style == "point" ) {
    gg <- gg + geom_point()
  } else if ( style == "line" ) {
    gg <- gg + geom_line()
  } else {
    gg <- gg + geom_area()
  }
  
  gg <- gg + 
    facet_wrap(facets, nrow = nrow, ncol = ncol, scales = scales ) 
  
  if ( !is.null(ylim) ) {
    gg <- gg + ylim(ylim)
  }
  
  # ----- Return ---------------------------------------------------------------
  
  return(gg)
  
}

