# A failing sensor

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

# Let's get time series data for a failing sensor in Nipomo
pat <- pat_load("SCNP_20", "2019-04-01", "2019-04-18")

# Quick look at raw data to check for spurious correlations
pat_scatterplot(pat)

# Humidity values over 100% are clearly wrong so apply out-of-spec QC
pat <- pat_qc(pat)

# Look at raw data again with better y-axis scaling
pat_scatterplot(pat)

# Now for a closer look at the timeseries data
pat_multiplot(pat)

# And an A/B comparison
pat_multiplot(pat, plottype = "pm25_over")

# Channel A has lots of high values. Let's look for outliers
pat_outliers(pat, showPlot = TRUE)

# Outlier detection with default settings doesn't catch them all.

# Things look bad so lets get some statistics to see how bad
model <- pat_internalFit(pat, showPlot = TRUE)

# By coincidence, the slope is 0.99 even though R squared is 0.427

# The slope line loos like it can't possibly be correct unless theare are many
# more points on the diagonal than there are shooting upward. We can identify
# this situation by using the graphical parameters "alpha" and "xylim"
model <- pat_internalFit(pat, alpha = 0.05, xylim = c(0,50))

# The relatively few high times when A is much higher than B are balanced out
# by the relatively many times when A is slightly lower than B.



