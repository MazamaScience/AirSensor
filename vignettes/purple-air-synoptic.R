## ----setup, include=FALSE, message=FALSE---------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5)

## ----pas_objects, message=FALSE------------------------------------------
# loading MazamaPurpleAir 
library(MazamaPurpleAir)
# and MazamaSpatialUtils
library(MazamaSpatialUtils)

# Loading package data
data("pas_load_sample")

# Code to download new data
# pas <- pas_load()

## ----examining_pas-------------------------------------------------------
class(pas_load_sample)
dim(pas_load_sample)

## ----column_names--------------------------------------------------------
colnames(pas_load_sample)

## ----subsetting----------------------------------------------------------
# subsetting using piping and filter() from magrittr and dplyr, respecively
WestCoastPas <- pas_load_sample %>%
  filter(stateCode %in% c('OR', 'WA', 'CA'))
# And plotting to check what the result is 
pas_leaflet(WestCoastPas)

## ----histogram-----------------------------------------------------------
hist(WestCoastPas$pwfsl_closestDistance)

## ----subsetting-2--------------------------------------------------------
# subset 
WestCoastPas <- WestCoastPas %>%
  filter(pwfsl_closestDistance <= 16009)
# and plot 
pas_leaflet(WestCoastPas)

