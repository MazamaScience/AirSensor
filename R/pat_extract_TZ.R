pat_extract_TZ <- function(pat) {
  # This function extracts the timezone from a pat object.
  if (!(pat_isPat(pat))) {
   stop("pat must be a PA Timeseries object.") 
  }
  if (pat_isEmpty(pat)) {
    stop("pat Meta must have data.")
  }
  
  timezone <- pat_extractMeta(pat)[["timezone"]]
  
  return(timezone)
}

