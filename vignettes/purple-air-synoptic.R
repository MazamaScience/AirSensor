## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5)

## ----pas_objects---------------------------------------------------------
# loading MazamaPurpleAir 
library(MazamaPurpleAir)
# and MazamaSpatialUtils
library(MazamaSpatialUtils)

# Loading package data
data("pas")

# Code to download new data
# pas <- pas_load()

## ----examining_pas-------------------------------------------------------
class(pas)
dim(pas)

## ----column_names--------------------------------------------------------
colnames(pas)

## ----subsetting----------------------------------------------------------
# subsetting using piping and filter() from magrittr and dplyr, respecively
WestCoastPas <- pas %>%
  filter(stateCode %in% c('OR', 'WA', 'CA'))
# And plotting to check what the result is 
pas_leaflet(WestCoastPas)

## ----histogram-----------------------------------------------------------
hist(WestCoastPas$pwfsl_closestDistance)

## ----subsetting-2--------------------------------------------------------
# subset 
WestCoastPas <- WestCoastPas %>%
  filter(pwfsl_closestDistance <= 1609)
# and plot 
pas_leaflet(WestCoastPas)

