#' @export
#' 
#' @title Join PurpleAir time series data for a single sensor
#' 
#' @param ... Any number of valid PurpleAir Time series \emph{pat} objects.
#' 
#' @return A PurpleAir Time series \emph{pat} object.
#' 
#' @description Create a merged timeseries using of any number of \emph{pat} 
#' objects for a single sensor. If \emph{pat} objects are non-contiguous, the 
#' resulting \emph{pat} will have gaps.
#' 
#' @note An error is generated if the incoming \emph{pat} objects have 
#' non-identical metadata.
#' 
#' @examples 
#' \dontrun{
#' pat <- example_pat
#' 
#' jul01_15 <- 
#'   pat %>%
#'   pat_filterDate(20180701, 20180715)
#' 
#' aug09_22 <- 
#'   pat %>% 
#'   pat_filterDate(20180809, 20180822)
#'   
#' x <- pat_join(jul01_15, aug09_22)
#' 
#' pat_multiplot(x, plottype = "pm25")
#' }
#' 

pat_join <- function(
  ...
) {

  # ----- Validate parameters --------------------------------------------------
  
  
  # ----- Join (concaenate) timeseries -----------------------------------------
  
  # Accept any number of pat objects
  patList <- list(...)  
  
  suppressWarnings({ # checks class type to see if already a list
    
    # TODO:  refactor this logic
    if( class(patList[[1]]) != c("pa_timeseries", "list") ) {
      patList <- patList[[1]]
    }
    
  })
  
  # Append to empty lists 
  dataList <- list()
  metaList <- list()
  
  for( i in seq_along(patList) ) {
    
    # Check parameters
    if( !pat_isPat(patList[[i]]) )
      stop("argument contains a non-'pat' object")
    if( pat_isEmpty(patList[[i]]) )
      stop("argument contains an empty 'pat' object")
    
    dataList[[i]] <- patList[[i]]$data
    metaList[[i]] <- patList[[i]]$meta
  
  }
  
  # Check that meta matches
  if( length(unique(metaList)) != 1 )
    stop("`pat` objects must be of the same monitor")
  
  meta <- patList[[1]]$meta
  data <- do.call(rbind, dataList) # duplicates removed below
  
  # ----- Create the Purple Air Timeseries (pat) object ------------------------
  
  pat <- list(meta = meta, data = data)
  class(pat) <- c("pa_timeseries", class(pat))
  
  # ----- Return ---------------------------------------------------------------
  
  # Remove any duplicate data records
  pat <- pat_distinct(pat)
  
  return(invisible(pat))
  
}
