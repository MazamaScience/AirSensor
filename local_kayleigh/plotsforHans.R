library(AirSensor)
library(MazamaCoreUtils)
library(MazamaSpatialUtils)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
pas <- pas_load()

pat <- pat_createNew(pas,"Rippon", startdate = 20191010, enddate = 20191129,
                     baseURL = "https://api.thingspeak.com/channels/")

# line plots for many SoH metrics
pat_dailySoHPlot(pat)

# daily SoH index and raw data plot
pat_dailySoHIndexPlot(pat)














