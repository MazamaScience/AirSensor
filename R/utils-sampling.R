#' @keywords internal
#' @title General table row sampling
#' 
#' @description This is a wrapper around sample() to make it easy to select 
#' random rows from a table. Supports both integer (sampleSize) and fractional 
#' (sampleFraction) N row sampling. For reproducible debugging, set a hash seed.
#' 
#' @param data Dataframe to be sampled
#' @param sampleSize a non-negative integer giving the number of rows to choose.
#' @param sampleFraction the fraction of rows to choose.
#' \code{smapleSize} is uised)
#' @param setSeed an integer that sets random numbver generation. Can be used to 
#' reproduce sampling.
#' 
#' @return A data.frame
#' 

.sample <- function(
  data,
  sampleSize = NULL,
  sampleFraction = 1, 
  setSeed = NULL
) { 
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(data)
  
  if ( !is.null(setSeed) ) set.seed(setSeed)
  
  if ( !is.null(sampleSize) ) {
    
    if ( sampleSize <= nrow(data) ) {
      sz <- sampleSize
    } else {
      sz <- nrow(data)
    }
    
  } else  if ( !is.null(sampleFraction) &&
               sampleFraction <= 1 && 
               sampleFraction > 0  ) { 
    
    sz <- nrow(data) * sampleFraction
    
  } else {
    
    stop("Invalid sampleSize or sampleFraction.")
    
  }
  
  subset <- 
    data[
      base::sample(
        x = nrow(data), 
        size = sz, 
        replace = FALSE,
        prob = NULL
      ),]
  
  return(subset)
  
}
