# Create a set of PAT monthly archive data files 

# required libraries
library(AirSensor)
library(stringr)

# Configurable variables
communityRegion <- "Nipomo"
date <- "2019-04-01"

# Get today's PAS object for the specified community
pas <- 
  pas_load() %>%
  pas_filter(communityRegion == !!communityRegion)

# Find all the labels 
labels <-
  pas %>%
  pas_filter(DEVICE_LOCATIONTYPE == "outside") %>%
  pull(label)

# Create and save a PAT object for each label

startdate <-
  lubridate::ymd(date) %>%
  lubridate::floor_date(unit = "month")

enddate <- 
  lubridate::ymd(date) %>%
  lubridate::ceiling_date(unit = "month") - lubridate::dminutes(1)

datestamp <- strftime(startdate, "%Y%m%d")

for ( label in labels ) {
  
  print(paste0("Working on ", label, "..."))
  
  result <- try({
    pat <- pat_load(pas, label,
                    startdate = startdate,
                    enddate = enddate)
  }, silent = TRUE)
  
  if ( "try-error" %in% class(result) ) {
    err_msg <- geterrmessage()
    if ( stringr::str_detect(err_msg, "No data") ) {
      print("No data for the requested time period.")
      print("TODO:  Generate an empty PAT object.")
    } else {
      stop(err_msg)
    }
  }
  
  filename <- paste0("pat_", label, "_", datestamp, ".rda")
  save(pat, file = filename)
  
}

