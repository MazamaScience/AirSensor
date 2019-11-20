library(AirSensor)
library(MazamaCoreUtils)
library(MazamaSpatialUtils)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)
library(purrr)
library(skimr)


setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
pas <- pas_load()

setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
pas <- pas_load(archival = TRUE)

## bad sensors to test ------------
# "Friends of Calwa": >200 unit offset between A and B channels in November, bad slope, intercept
# "Redlands_11 P1": really sparse data, percent reporting almost completely 0 accross the board
# "Tumminaro Hangar": a couple drop outs, A/B offset (~100), poor A/B r^2, poor slope

## medium sensors to test -------------
# "North Cascades National Park - Marblemount": marginal offset between A/B, few dips in reporting, really good A/B r^2
# "City of Emmett": a bit noisier A than B, wonky r^2, everything else looks great
# "Globeville": flatlined temperature, few drops in r^2

## good sensors to test --------------
# "Fulcrum Environmental Consulting": small drop out in slope, int, r^2, but perfect aside from that
# "GRIZZLY WAY": small blips in pct_DC but perfect aside from that

pat <- pat_createNew(pas,"Globeville", startdate = 20191001, enddate = 20191118,
                     baseURL = "https://api.thingspeak.com/channels/")

pat_multiplot(pat)
pat_multiplot(pat, plottype = "pm25_over")
#pat_dailySoHPlot(pat)
soh <- pat_dailySoH(emmett_pat)
pat_dailySoHPlot_change_Vars(emmett_pat)

pattern <- c("_statistic"| 
             "_value"| 
             "pm25_A_pm25_B_slope"|
             "pm25_A_pm25_B_intercept"|
             "pm25_A_pm25_B_rsquared")

soh <- pat_dailySoH(calwa_pat)


timeseriesTbl_multiplot(soh, parameters = c("pm25_A_pctReporting", "pm25_B_pctReporting", "pm25_A_pm25_B_t_statistic", 
                          "pm25_A_pm25_B_p_value", 
                         "pm25_A_pm25_B_slope",
                          "pm25_A_pm25_B_intercept",
                          "pm25_A_pm25_B_rsquared"), ncol = 1, autoRange = TRUE)


