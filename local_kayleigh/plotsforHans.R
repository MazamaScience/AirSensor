library(AirSensor)
library(MazamaCoreUtils)
library(MazamaSpatialUtils)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
pas <- pas_load()

# Jon liked this example pat because it changes in quality half-way through the date range
pat <- pat_createNew(pas,"Rippon", startdate = 20191010, enddate = 20191129,
                     baseURL = "https://api.thingspeak.com/channels/")

# 1) line plots for many SoH metrics ---------------------------------------------
pat_dailySoHPlot(pat)

# 2) daily SoH index and raw data plot -------------------------------------------
pat_dailySoHIndexPlot(pat)

# 3) state of health histogram ---------------------------------------------------

# load the .rda file containing all the soh's for CA
soh_CA <- get(load("/Users/kayleigh/Projects/AirSensor/local_kayleigh/soh_CA.Rda"))

# plot historgram
gg <- ggplot(soh_CA, aes(SoH_index)) +
  geom_histogram(data=subset(soh_CA, SoH_index_bin == '0'), 
                 fill = "firebrick", color = "firebrick", alpha = 0.6, bins = 40) +
  geom_histogram(data=subset(soh_CA, SoH_index_bin == '1'), 
                 fill = "goldenrod1", color = "goldenrod1", alpha = 0.6, bins = 40) +
  geom_histogram(data=subset(soh_CA, SoH_index_bin == '2'), 
                 fill = "mediumseagreen", color = "mediumseagreen", alpha = 0.6, bins = 40) +
  xlab("SoH Index") +
  labs(title = "State of health histogram of California PurpleAir sensors")

gg














