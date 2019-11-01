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
  
  # NOTE:  If the fist element is NOT a "pa_timeseries", assume we are being 
  # NOTE:  handed a list of pat objects rather than separate pat objects.
  suppressWarnings({
    if( !"pa_timeseries" %in% class(patList[[1]]) ) {
      patList <- patList[[1]]
    }
  })
  
  # Initialize empty lists 
  dataList <- list()
  metaList <- list()
  
  for( i in seq_along(patList) ) {
    
    # Check parameters
    if( !pat_isPat(patList[[i]]) )
      stop("arguments contain a non-'pat' object")
    if( pat_isEmpty(patList[[i]]) )
      stop("arguments contain an empty 'pat' object")
    
    metaList[[i]] <- patList[[i]]$meta
    dataList[[i]] <- patList[[i]]$data
  
  }
  
  # TODO:  We have a basic problem with the pwfsl_closest~ variables.
  # TODO:  These can change whan a new, temprary monitor gets installed.
  # TODO:  We don't want to have two separate metadata records for a single 
  # TODO:  Sensor as the metadata is supposed to be location-specific and
  # TODO:  not time-dependent. Unfortunately, the location of the nearest
  # TODO:  PWFSL monitor is time-dependent and any choice we make will break
  # TODO:  things like pat_externalFit() for those periods when a temporary
  # TODO:  monitor is closer than a permanent monitor.
  # TODO:
  # TODO:  Ideally, enhanceSynopticData() would have some concept of
  # TODO:  "permanent" monitors but this is far beyond what is currently
  # TODO:  supported.
  
  # Deal with pwfsl_closest~ issue by always using the most recent value
  meta_last <- metaList[[length(metaList)]]
  for ( i in seq_along(metaList) ) {
    metaList[[i]]$pwfsl_closestDistance <- meta_last$pwfsl_closestDistance
    metaList[[i]]$pwfsl_closestMonitorID <- meta_last$pwfsl_closestMonitorID
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
