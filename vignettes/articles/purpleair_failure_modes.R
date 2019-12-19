## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(fig.width=7, fig.height=5)
library(AirSensor)
setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
pas <- pas_load(archival = TRUE)

## ----channel_a_noise_1, echo=FALSE, message=FALSE, warning=FALSE---------
pat_a_noisy_1 <- example_pat_failure_A
pat_internalFit(pat_a_noisy_1)

## ----channel_a_noise_2, echo=FALSE---------------------------------------
pat_a_noisy_2 <- pat_createNew(pas, 
                               label = "SCAP_14", 
                               startdate = 20190701, 
                               enddate = 20190708,
                               timezone = "America/Los_Angeles")
pat_multiplot(pat_a_noisy_2, plottype = "pm25_over")

## ----channel_a_jump, echo=FALSE------------------------------------------
pat_a_jump <- example_pat_failure_B
pat_multiplot(pat_a_jump, plottype = "pm25_over")

## ----channel_a_humidity_multi, echo=FALSE, warning=FALSE-----------------
pat_a_humidity <- pat_createNew(pas, "BikeSGV - West Pasadena", 
                                startdate = "2019-04-16", 
                                enddate = "2019-04-24",
                                timezone = "America/Los_Angeles")
pat_multiplot(pat_a_humidity)

## ---- channel_a_humidity_scatter, echo=FALSE, warning=FALSE--------------
pat_scatterplot(pat_a_humidity)

## ---- echo=FALSE, warning=FALSE------------------------------------------
pat <- pat_createNew(pas, "SCEM_05", 
                     startdate = 20190701, 
                     enddate = 20190710,
                     timezone = "America/Los_Angeles")
pat_multiplot(pat)

## ------------------------------------------------------------------------
plot(pat$data$datetime, pat$data$pm25_A, 
     ylim = c(3325, 3340), 
     pch = 15, cex = 0.6, col=adjustcolor("black", 0.2),
     xlab = "2019", ylab = "PM2.5 A")

temp <- table(as.vector(pat$data$pm25_A))
print(paste0("Mode value: ", names(temp)[temp == max(temp)]))

## ----channel_b_zero, echo=FALSE------------------------------------------
pat_b_zero <- pat_createNew(pas, "SCAP_46", 
                            startdate = "2019-07-01", 
                            enddate = "2019-07-08",
                            timezone = "America/Los_Angeles")
pat_multiplot(pat_b_zero, sampleSize = NULL)

simple <- dplyr::select(pat_b_zero$data, datetime, pm25_A, pm25_B)
head(simple)

