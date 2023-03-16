pat_agg_00 <- function(pat, FUN, ...) {
  
  # Convert to eXtensible Time Series (xts) data.frame
  # Separate only useful data for calculation (i.e. only numeric)
  df <- xts::xts(pat$data[2:8], order.by = pat$data$datetime, unique = TRUE, tzone = 'UTC')
  
  # Split the xts into list of hourly binned data.frames 
  df_hr <- xts::split.xts(df, f = 'hours', drop = FALSE)
  
  # Align the binned data frames to its rounded next hour period (60 * 60 sec)
  # Get the first index of aligned time for future use.
  datetime <- as.numeric(
    lapply(
      X = df_hr, 
      FUN = function(x) zoo::index(xts::align.time(x, 60*60))[1]
    )
  )
  # Convert saved datetime vector back to posix* from int
  class(datetime) <- c("POSIXct", "POSIXt")
  attr(datetime, 'tzone') <- 'UTC'
  
  # Map each binned hourly data.frame to the user defined anonymous 
  # function f applied via apply to each vector in the mapped data.frame
  mapped <- Map( 
    df_hr, 
    f = function(x, f = FUN) {
      apply(
        X = x, 
        MARGIN = 2, 
        FUN = function(x_, ...) { f(x_, ...) }
      )
    }
  )
  
  # Add the rbind mapped data back to the pat object to preserve flexibility 
  # and consistency among the package. 
  pat$data <- data.frame(
    'datetime' = datetime, 
    do.call(rbind, mapped), 
    'datetime_A' = datetime, 
    'datetime_B' = datetime
  )
  
  # Return 
  return(pat)
}

# --- Tests -----

###setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
setArchiveBaseUrl("https://airsensor.aqmd.gov/PurpleAir/v1")
pas <- pas_load()
pat <- pat_load(label = 'SCSC_33', pas = pas)

str(pat$data)

counts_pat <- pat_agg_00(pat, function(x) length(na.omit(x)))
max_pat <- pat_agg_00(pat, function(x) max(x, na.rm = TRUE))
avg_pat <- pat_agg_00(pat, function(x) mean(x, na.rm = TRUE)) # identical to pat_agg_00(pat, mean)
MAD_pat <- pat_agg_00(pat, function(x) mad(x, na.rm = TRUE))


microbenchmark::microbenchmark(pat_aggregate(pat), pat_agg_00(pat, function(x) mean(x, na.rm = T)), times = 10)
