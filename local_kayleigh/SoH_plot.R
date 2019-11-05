#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains
#' 
#' @title State of Health Plot
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param FUNs Vector of function names. All the passed in functions must output
#' tibbles with the same number of rows
#' 
#' @description This function combines the output of the State of Health (SoH) 
#' function arguments into a single tibble. 
#' 
#' 
#' @examples  
#' SoH <- 
#'   example_pat_failure_B %>%
#'   pat_dailyStateOfHealth() 
#' head(SoH)
#' #timeseriesTbl_multiplot(tbl, ylim = c(0,100))
#' 

SoH_plot <- function(
  SoH_tbl = NULL,
  pattern = NULL, 
  parameter = NULL,
) {
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(SoH_tbl)
  
  # Check for existence of "datetime"
  if ( !"datetime" %in% names(SoH_tbl) || (!"POSIXct" %in% class(SoH_tbl$datetime)) )
    stop("Parameter 'tbl' must have a column named 'datetime' of class 'POSIXct'.")
  
  # Check for existence of parameters
  if ( !is.null(parameters) ) {
    unknownParameters <- setdiff(parameters, names(SoH_tbl))
    if ( length(unknownParameters) > 0 ) {
      parameterString <- paste0(unknownParameters, collapse = ", ")
      err_msg <- paste0("Parameters not found in tbl:\n\t",
                        parameterString)
      stop(err_msg)
    }
  }
  
  # ----- Determine parameters to plot -----------------------------------------
  if ( is.null(parameters) ) {
    parameters <- sort(names(SoH_tbl))
  }
  
  # Subset if requested
  if ( !is.null(parameterPattern) ) {
    parameters <- stringr::str_subset(parameters, parameterPattern)
  }
  
  # Make sure 'datetime' is included, but only once
  parameters <- unique(c("datetime", parameters))
  
}  





