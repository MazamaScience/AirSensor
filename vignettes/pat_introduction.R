## ----setup, include=FALSE, message=FALSE---------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5)
library(MazamaCoreUtils)
library(MazamaPurpleAir)
library(dplyr)
library(ggplot2)
# NOTE: Use example PAS and PAT data for vignettes to avoid long wait-times
data("example_pas")
data("example_pat")
pas <- example_pas
pat <- example_pat

## ---- eval = FALSE-------------------------------------------------------
#  library(MazamaPurpleAir)
#  library(MazamaCoreUtils)
#  pas <- pas_load()
#  pat <- pat_load(pas, "Seattle", startdate = 20180701, enddate = 20180901)

## ------------------------------------------------------------------------

data("example_pat")

df <- tibble(datetime = example_pat$data$datetime, pm25 = example_pat$data$pm25_A)
ggdf <-  ggplot(data = df,  aes(x = datetime, y = pm25 )) + geom_line() 

ggdf

