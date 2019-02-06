## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5)

## ----loading-packages-and-data, message=FALSE, warnings='suppress'-------
# loading required package
library(MazamaPurpleAir)
library(MazamaSpatialUtils)

# using pre-loaded data
data("pas_raw")

# code to download new data 
# pas_raw <- downloadParseSynopticData()

## ----names---------------------------------------------------------------
names(pas_raw)

## ----names-enhanced------------------------------------------------------
initializeMazamaSpatialUtils()
pas_enhanced <- enhanceSynopticData(pas_raw)
names(pas_enhanced)

## ----interactive-mapping-------------------------------------------------
pas_leaflet(pas_enhanced)

## ----subsetting----------------------------------------------------------
# subsetting using piping and filter() from magrittr and dplyr, respecively
WestCoastPas <- pas_enhanced %>%
  filter(stateCode %in% c('OR', 'WA', 'CA'))
# And plotting to check what the result is 
pas_leaflet(WestCoastPas)

## ----histogram-----------------------------------------------------------
hist(WestCoastPas$pwfsl_closestDistance)

