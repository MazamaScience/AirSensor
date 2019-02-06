## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5)

## ----loading-packages-and-data, message=FALSE, warnings='suppress'-------
# loading required package
library(MazamaPurpleAir)

# using pre-loaded data
data("pas_raw")

# code to download new data 
# pas_raw <- downloadParseSynopticData()

## ----names---------------------------------------------------------------
names(pas_raw)

