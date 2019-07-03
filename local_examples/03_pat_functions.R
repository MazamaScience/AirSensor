# Working with "pat" objects

# required libraries
library(AirSensor)
library(stringr)             # for string matching

# Load this morning's synoptic data
pas <- pas_load()

# Filter for SCAQMD sensors
scaqmd <-
  pas %>%
  pas_filter(str_detect(label, "^[Ss][Cc].._.."))

# Have a look
pas_leaflet(scaqmd)

# Let's get time series data for a sensor in Nipomo

pat <- pat_load("SCNP_14", "2019-03-01", "2019-04-15")

# Quick look at raw data to check for spurious correlations
pat_scatterplot(pat)

# Humidity values over 100% are clearly wrong so remove out-of-spec records
pat <- pat_qc(pat)

# Look at raw data again with better y-axis scaling
pat_scatterplot(pat)

# Now for a closer look at the timeseries data
pat_multiplot(pat)

# And an A/B comparison
pat_multiplot(pat, plottype = "pm25_over")

# Let's look for outliers
pat_outliers(pat, showPlot = TRUE)

# Replace them with the window median value
pat <- pat_outliers(pat, replace = TRUE, showPlot = FALSE)

# One more A/B comparison
pat_multiplot(pat, plottype = "pm25_over")

# Looks good but lets get some statistics
model <- pat_internalFit(pat, showPlot = TRUE)

# Very nice. What about model diagnostics?
plot(model)

# Looks good. Let's use the interactive plot on a week's worth of data
pat %>%
  pat_filterDate("2019-03-08","2019-03-14") %>%
  pat_dygraph("pm25")






