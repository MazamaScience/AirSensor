# Kincade fire, Sonoma County Airport, Oct 25-29, 2019
#

# Load useful packages
library(dplyr)             # tibble manipulation
library(skimr)             # quickly summarize tibbles
library(worldmet)          # airport weather data
library(openair)           # pollution plots
library(PWFSLSmoke)        # monitoring data and plots
library(AirMonitorPlots)   # monitoring advanced plots (ggplot2)

# ----- PWFSLSmoke for monitoring data -----------------------------------------

# NOTE:  PWFSLSmoke data is all in UTC timezone

# All monitors in California
ca_mon <- 
  monitor_loadLatest() %>%
  monitor_subset(stateCodes = "CA")

# Use leaflet map to identify the temporary monitor at Sonoma County Airport
monitor_leaflet(ca_mon)

stsID <- "lon_.122.806_lat_38.511_arb2.1042"

# NOTE:  monitor_load(start, end) is slow unless you specify monitorID.
# NOTE:  Now that we have it, we can ask for specific dates.

sts_mon <- 
  monitor_load(20191025, 20191030, monitorIDs = stsID)

# Advanced timeseries plot
AirMonitorPlots::monitor_ggTimeseries(sts_mon)

# Get times associated with first and last non-NA pm25 values
startIndex <- min(which(!is.na(sts_mon$data[,2])))
endIndex <- max(which(!is.na(sts_mon$data[,2])))
startTime <- sts_mon$data$datetime[startIndex]
endTime <- sts_mon$data$datetime[endIndex]

# ----- worldmet for airport weather data --------------------------------------

# NOTE:  worldmet data is all in GMT timezone

# Get metadata dataframe and interactive map at the same time
ca_worldmet <- getMeta(country="US", state="CA", plot = TRUE)

# Use leaflet map to identify the met station at Sonoma County Airport
ksts <- dplyr::filter(ca_worldmet, code == "724957-23213")

# Quick look:
print(ksts)

# Get the hourly data for 2019
sts_met <- importNOAA(code <- ksts$code, year = 2019)

# Quick look
skimr::skim(sts_met)

# NOTE:  'date' column is POSIXct -- good for work with openair package

# Trim things down to where we have monitor data
sts_met <- 
  sts_met %>%
  dplyr::filter(date >= startTime & date <= endTime)

# Quick look again
skimr::skim(sts_met)

# ----- openair for pollution plots --------------------------------------------

# NOTE:  openair requires a tibble with a 'date' column and a pollution parameter.
# NOTE:  We are going to add the pm25 data from sts_mon to the sts_met tibble
# NOTE:  and then pass this to openair::pollutionRose()

# 1) Get pm25_tbl with proper names
pm25_tbl <- sts_mon$data
names(pm25_tbl) <- c("date", "pm25")

# 2) Join this with sts_met
tbl <- dplyr::left_join(sts_met, pm25_tbl, by = "date")

# 3) Review it
View(tbl)

# This is sometimes needed to reset the "plot device"
dev.off()

# 4) Create the pollutionRose plot
title <- sprintf(
  "%s -- %s to %s",
  ksts$STATION,
  strftime(startTime, "%b %d"),
  strftime(endTime, "%b %d, %Y")
)
openair::pollutionRose(tbl, pollutant = "pm25", main = title)
