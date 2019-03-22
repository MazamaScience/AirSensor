## ----setup, include=FALSE, message=FALSE---------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5)

## ----pas_objects, message=FALSE------------------------------------------
# loading MazamaPurpleAir 
library(MazamaPurpleAir)
# and MazamaSpatialUtils
library(MazamaSpatialUtils)

# Load package data
data("example_pas")

# Code to download new data
# pas <- pas_load()

## ----examining_pas-------------------------------------------------------
class(example_pas)
dim(example_pas)

## ----column_names--------------------------------------------------------
colnames(example_pas)

## ----subsetting----------------------------------------------------------
# subsetting using piping and filter() from magrittr and dplyr, respecively
WestCoast_pas <- example_pas %>%
  filter(stateCode %in% c('OR', 'WA', 'CA'))
# And plotting to check what the result is 
pas_leaflet(WestCoast_pas)

## ----histogram-----------------------------------------------------------
hist(WestCoast_pas$pwfsl_closestDistance, n=50)

## ----subsetting-2--------------------------------------------------------
# subset 
WestCoast_pas <- WestCoast_pas %>%
  filter(pwfsl_closestDistance <= 16009)
# and plot 
pas_leaflet(WestCoast_pas)

