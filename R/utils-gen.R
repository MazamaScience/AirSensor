
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

.sample <- 
  function(
    data,
    sampleSize = NULL,
    sampleFraction = 1, 
    setSeed = NULL
  ) { 
    
    if ( !is.null(setSeed) ) set.seed(setSeed)
    
    if ( !is.null(sampleSize) && 
         sampleSize <= nrow(data)  ) { 
      
      sz <- sampleSize
      
    } else  if ( !is.null(sampleFraction) &&
                 sampleFraction <= 1 && 
                 sampleFraction > 0  ) { 
      
      sz <- nrow(data) * sampleFraction
      
    } else {
      
      stop("Invlaid sampleSize or sampleFraction.")
      
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
