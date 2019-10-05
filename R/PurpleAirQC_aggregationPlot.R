#' @export
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes_ geom_line labs facet_wrap 
#' 
#' @title Faceted plot of aggregation statistics 
#' 
#' @param aggregationStats PurpleAir Timeseries \emph{aggregationStats} object.
#' @param parameterPattern Pattern used to match groups of parameters.
#' @param parameters Custom vector of aggregation parameters to view.
#' @param nrow Number of rows in the faceted plot.
#' @param autoRange Logical specifying whether to scale the y axis separately
#' for each plot or to use a common y axis.
#' 
#' @description A plotting function that uses ggplot2 to display a plot of each 
#' output category from the pat_aggregateOutlierCounts() function. Created to 
#' have a quick look at all the stats to help identify necessary quality control
#' methods for PurpleAir Timeseries \emph{aggregationStats} objects.
#' 
#' @examples 
#' \donttest{
#' aggregationStats <- pat_aggregateOutlierCounts(example_pat_failure_A)
#' PurpleAirQC_aggregationPlot(aggregationStats, 
#'                             parameterPattern = "humidity_m|temperature_m", 
#'                             nrow = 2)
#' }

PurpleAirQC_aggregationPlot <- function(
  aggregationStats = NULL, 
  parameterPattern = NULL,
  parameters = NULL,
  nrow = 5,
  autoRange = TRUE
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(aggregationStats)
  
  if ( !is.null(parameters) ) {
    unknownParameters <- setdiff(parameters, names(aggregationStats))
    if ( length(unknownParameters) > 0 ) {
      parameterString <- paste0(unknownParameters, collapse = ", ")
      err_msg <- paste0("Parameters not found in aggregationStats:\n\t",
                        parameterString)
      stop(err_msg)
    }
  }
  
  # ----- Determine parameters to plot -----------------------------------------
  
  if ( is.null(parameters) ) {
    
    # Start by sorting names alphabetically but put pm25_~ at the end
    parameters <- sort(names(aggregationStats))
    parameters <- parameters[!parameters %in% c("pm25_df", "pm25_p", "pm25_t")]
    parameters <- append(parameters, c("pm25_df", "pm25_p", "pm25_t"))
    
    # Subset if requested
    if ( !is.null(parameterPattern) ) {
      parameters <- stringr::str_subset(parameters, parameterPattern)
    }
    
  }
  
  # Otherwise just use the incoming parameters
  
  # Make sure 'datetime' is included, but only once
  parameters <- unique(c("datetime", parameters))

  tidyData <- 
    aggregationStats[,parameters] %>%
    tidyr::gather("parameter", "value", -.data$datetime)
  
  # ----- Create plot ----------------------------------------------------------
  
  # Y axis
  if ( autoRange ) {
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
    ggplot(tidyData, aes_(x = ~datetime, y = ~value)) +
    geom_line() +
    labs(title="Aggregation Statistics") +
    facet_wrap(facets, nrow = nrow, scales = scales ) 
  
  # ----- Return ---------------------------------------------------------------
  
  return(gg)
  
}

