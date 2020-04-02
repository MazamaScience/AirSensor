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


pat_SoHIndexAsCSV <- function(
  pat = NULL,
  path = NULL
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
  
  index <-
    index %>%
    dplyr::mutate(label = station_name) %>%
    dplyr::select(.data$datetime, .data$SoH_index_bin, .data$label) 
  
  index <- index[,c(3,1,2)]
  
  if ( is.null(path) ) {
    path <- getwd()
    write.csv(index, file = paste(path,'/pat_SoHIndex.csv', sep = ''))
  }
  
  if ( !is.null(path) ) {
    if (stringr::str_sub(path, -1) == "/") {
      write.csv(index, paste(path,'pat_SoHIndex.csv',sep = ''))
    }
    else {
      write.csv(index, paste(path,'/pat_SoHIndex.csv',sep = ''))
    }
  }
  
  
  
  
  
  # ----- Return -------------------------------------------------------------
  #return(gg)
  
}



