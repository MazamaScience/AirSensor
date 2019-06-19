#' @export
#' 
#' @title Join PurpleAir time series data
#' 
#' @param ... Any number of valid \code{pat} objects
#' 
#' @return \code{pat} time series object
#' 
#' @description Create a union of any number of \code{pat} objects along a 
#' non-continuous datetime axis. 
#' 
#' @examples 
#' \dontrun{
#' jul01_15 <- 
#'   pat %>%
#'   pat_filterDate(20180701, 20180715)
#' 
#' aug09_22 <- 
#'   pat %>% 
#'   pat_filterDate(20180809, 20180822)
#'   
#' x <- pat_join(jul01_15, aug09_22)
#' }
#' 

pat_join <- function(
  ...
) {

  logger.debug("----- pat_join() -----")
  
  # Accept any number of pat objects
  patList <- list(...)  
  
  suppressWarnings({ # checks class type to see if already a list
    
  if( class(patList[[1]]) != c("pa_timeseries", "list") ) {
    patList <- patList[[1]]
  }
    
  })
  
  # Append to empty lists 
  dataList <- list()
  metaList <- list()
  
  for( i in 1:length(patList) ) {
    
    # Check parameters
    if( !pat_isPat(patList[[i]]) )
      stop("argument contains a non-`pat` object")
    if( pat_isEmpty(patList[[i]]) )
      stop("argument contains an empty `pat` object")
    
    dataList[[i]] <- patList[[i]]$data
    metaList[[i]] <- patList[[i]]$meta
  
  }
  
  # Check that meta matches
  if( length(unique(metaList)) != 1 )
    stop("`pat` objects must be of the same monitor")
  
  meta <- patList[[1]]$meta
  data <- do.call(rbind, dataList)
  
  # ----- Create the Purple Air Timeseries (pat) object ------------------------
  
  pat <- list(meta = meta, data = data)
  class(pat) <- c("pa_timeseries", class(pat))
  
  return(invisible(pat))
  
}
