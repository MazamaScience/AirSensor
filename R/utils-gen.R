#' @export
#' 
#' @title General table row sampling
#' 
#' @description This is a wrapper around sample() to make it easy to select 
#' random rows from a table. Supports both integer (sampleSize) and fractional 
#' (sampleFraction) N row sampling. For reproducible debugging, set a hash seed.
#' 
#' @param data
#' @param sampleSize
#' @param sampleFraction
#' @param weight
#' @param setSeed 
#' 
sample <- 
  function(
    data,
    sampleSize = NULL,
    sampleFraction = NULL, 
    weight = NULL,
    setSeed = NULL
    ) { 
    
    if (!is.null(setSeed) )( set.seed(setSeed) ) 
    
    if ( !is.null(sampleSize) && 
         sampleSize <= nrow(data) 
    ) { 
      
      sz <- sampleSize
      
    } 
    
    if ( !is.null(sampleFraction) &&
         sampleFraction <= 1 && 
         sampleFraction > 0 
    ) { 
      
      sz <- nrow(data) * sampleFraction
      
    }
    
    # ----- Fastest possible way to sample -------------------------------------
    subset <- 
      data[
        .Internal(
          sample(
            x = nrow(data), 
            size = sz, 
            replace = FALSE, 
            prob = weight
          )
        ),]
    
    return(subset)

}