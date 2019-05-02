# We should probably have our own time averaging function rather than
# openair::timeAverage() which doesn't do what we want and is bloated with
# lots of functionality we don't care about.

# required libraries
library(AirSensor)
library(lubridate)


pat <- AirSensor::example_pat
pat_failure <- AirSensor::example_pat_failure

unit <- "10 min"

starttime <- range(pat$data$datetime)[1]
endtime <- range(pat$data$datetime)[2]s

floorStart <- lubridate::floor_date(starttime, unit = unit)
ceilingEnd <- lubridate::ceiling_date(endtime, unit = unit)

newTimeAxis <- seq.POSIXt(floorStart, ceilingEnd, by = unit)

# TODO:  Create a new, empty tibble with the apropriate columns?

binCount <- length(newTimeAxis - 1)

recordList <- list()

for ( i in seq_len(binCount) ) {
  
  # Extract data within this time range
  tbl <-
    pat %>%
    pat_extractData() %>%
    dplyr::filter(.data$datetime >= newTimeAxis[i]) %>%
    dplyr::filter(.data$datetime < newTimeAxis[i+1])

  if ( nrow(tbl) == 0 ) {
    
    recordList[[i]] <- 
      NULL # TODO:  Create a single row tibble with all NA
    
  } else {
    
    # TODO:  At some point we should support a 'data.thresh' argument like openair::timeAverage()
    
    # TODO:  Put a tibble in recordList with entries for each of the following

    pm25_A_mean <- mean(tbl$pm25_A, na.rm = TRUE)
    pm25_A_sd <- sd(tbl$pm25_A, na.rm = TRUE)
    pm25_A_count <- length(!is.na(tbl$pm25_A))
    # TODO:  other variables for pm25_B, temperature, humidity
    
    # TODO:  Read up on "two-sample t-test" (Khan Academy video is good) and
    # TODO:  make sure we are doing this properly. These also want to be in the
    # TODO:  record.
    tTest <- t.test(tbl$pm25_A, tbl$pm25_B)
    pm25_t <- tTest$statistic
    pm25_df <- tTest$parameter
    pm25_p <- tTest$p.value
    
  }
  
  averagedData <- dplyr::bind_rows(recordList)
  
  return(averagedData)
  
}
