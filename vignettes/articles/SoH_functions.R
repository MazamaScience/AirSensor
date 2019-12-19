## ----setup, include=FALSE, message=FALSE---------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5)
library(AirSensor)
library(dplyr)
library(knitr)
library(rmarkdown)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
pas <- pas_load(archival = TRUE)
hum_color <- "forestgreen"
pm25_A_color <- "goldenrod3"
pm25_B_color <- "deepskyblue4"
temp_color <- "plum4"
colors <- c(pm25_A_color, pm25_B_color, hum_color, temp_color)

## ---- pat_muliplots fig-margin, fig.margin = FALSE-----------------------

good_pat <- pat_createNew(pas, "SCAH_29", 
                          startdate = "2019-05-27", 
                          enddate = "2019-06-22", 
                          timezone = "America/Los_Angeles")

pat_multiplot(good_pat, sampleSize = 1e6)

bad_pat <- pat_createNew(pas, "SCTV_16", 
                         startdate = "2019-05-27", 
                         enddate = "2019-06-22", 
                         timezone = "America/Los_Angeles")

pat_multiplot(bad_pat,  sampleSize = 1e6)

## ---- calculate_percent_valid, layout = "l-body-outset"------------------
good_PctReport <- PurpleAirSoH_dailyPctReporting(good_pat)
bad_PctReport <- PurpleAirSoH_dailyPctReporting(bad_pat)

## ---- percent_reporting_plot, echo=TRUE, message=FALSE, warning=FALSE----

# gg setup
good_tidy <- good_PctReport %>%
  gather(key ="variable", value = "value", -datetime) 

bad_tidy <- bad_PctReport %>%
  gather(key ="variable", value = "value", -datetime)

# ggplots using tidy data
gg_good <- ggplot(good_tidy) +
  geom_point(aes(x = datetime, y = value, color = variable), alpha = 0.8)+
  facet_wrap(vars(variable), scales = "free_y", nrow = 4) +
  scale_color_manual(values = c("black","red", "blue", "black")) +
  ylab("percent") +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150), 
                     limits = c(0, 150)) +
  theme(legend.position = "none") +
  ggtitle("GOOD sensor")

print(gg_good)

gg_bad <- ggplot(bad_tidy) +
  geom_point(aes(x = datetime, y = value, color = variable), alpha = 0.8)+
  facet_wrap(vars(variable), scales = "free_y", nrow = 4) +
  scale_color_manual(values = c("black","red", "blue", "black")) +
  ylab("percent") +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150), 
                     limits = c(0, 150)) +
  theme(legend.position = "none") +
  ggtitle("BAD sensor")

print(gg_bad)

## ---- calculate_percent_valid, PurpleAirSoH_dailyPctValid----------------
good_PctValid <- PurpleAirSoH_dailyPctValid(good_pat)
bad_PctValid <- PurpleAirSoH_dailyPctValid(bad_pat)

## ---- percent_valid_plot, echo=TRUE, message=FALSE, warning=FALSE--------

# gg setup
good_tidy <- good_PctValid %>%
  gather(key ="variable", value = "value", -datetime) 

bad_tidy <- bad_PctValid %>%
  gather(key ="variable", value = "value", -datetime) 

# ggplots using tidy data
gg_good <- ggplot(good_tidy) +
  geom_point(aes(x = datetime, y = value, color = variable), alpha = 0.8)+
  facet_wrap(vars(variable), scales = "free_y", nrow = 4) +
  scale_color_manual(values = c("black","red", "blue", "black")) +
  ylab("percent") +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150), 
                     limits = c(0, 150)) +
  theme(legend.position = "none") +
  ggtitle("GOOD sensor")

print(gg_good)

gg_bad <- ggplot(bad_tidy) +
  geom_point(aes(x = datetime, y = value, color = variable), alpha = 0.8)+
  facet_wrap(vars(variable), scales = "free_y", nrow = 4) +
  scale_color_manual(values = c("black","red", "blue", "black")) +
  ylab("percent") +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150), 
                     limits = c(0, 150)) +
  theme(legend.position = "none") +
  ggtitle("BAD sensor")

print(gg_bad)

## ---- SoH_plot, echo=TRUE, message=FALSE, warning=FALSE------------------

pat_dailySoHPlot(good_pat)

pat_dailySoHPlot(bad_pat)

## ---- pat_dailySoHIndexPlot, echo=TRUE, message=FALSE, warning=FALSE-----

pat_dailySoHIndexPlot(good_pat)

pat_dailySoHIndexPlot(bad_pat)

