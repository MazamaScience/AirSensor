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


#Forest Service data
setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
pas <- pas_load()

setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
pas <- pas_load(archival = TRUE)

## bad sensors to test ------------
# "Friends of Calwa": >200 unit offset between A and B channels in November, bad slope, intercept
### variable missing complete  n mean   sd    p0 p25  p50 p75 p100     hist
### calwa_soh$pm25_A_pm25_B_rsquared       0       39 39 0.54 0.31 0.013 0.3 0.45 0.9 0.98 ▂▅▅▅▂▂▂▇

# "Redlands_11 P1": really sparse data, percent reporting almost completely 0 accross the board
# "Tumminaro Hangar": a couple drop outs, A/B offset (~100), poor A/B r^2, poor slope
### variable missing complete  n  mean    sd      p0    p25   p50   p75 p100     hist
### tumm_soh$pm25_A_pm25_B_rsquared       2       46 48 0.041 0.051 1.8e-06 0.0032 0.027 0.054 0.27 ▇▃▁▂▁▁▁▁

# "Rippon": great example of a sensor that is doing well until it starts to fail.
### variable missing complete  n mean   sd      p0   p25  p50  p75 p100     hist
### rippon_soh$pm25_A_pm25_B_rsquared       0       40 40 0.51 0.45 0.00012 0.034 0.53 0.96    1 ▇▁▁▁▁▁▁▇
# "Clean Air Carolina Calypso": 

## medium sensors to test -------------
# "North Cascades National Park - Marblemount": marginal offset between A/B, few dips in reporting, really good A/B r^2
# "City of Emmett": a bit noisier A than B, wonky r^2, everything else looks great
# "Globeville": flatlined temperature, few drops in r^2
# "Charleston, Illinois": pretty good
# "Weatherby": pretty good 


## good sensors to test --------------
# "Fulcrum Environmental Consulting": small drop out in slope, int, r^2, but perfect aside from that
# "BWS2 N Fairview": The most perfect one yet
# "GRIZZLY WAY": small blips in pct_DC but perfect aside from that
# " Nelson Trailhead": - also very good 
# "Cherry Hill"

pat <- pat_createNew(pas,"Rippon", startdate = 20191001, enddate = 20191118,
                     baseURL = "https://api.thingspeak.com/channels/")

pat_multiplot(pat)
pat_multiplot(pat, plottype = "pm25_over")
#pat_dailySoHPlot(pat)
soh <- pat_dailySoH(fairview_pat)
pat_dailySoHPlot_change_Vars(pat)
pat_dailySoHPlot_SEPARATE_Vars(pat)


pat$data$datetime <- lubridate::with_tz(pat$data$datetime, tzone = pat$meta$timezone)



#index <- pat_dailySoHIndex_00(pat)
station_name <- pat$meta$label
#cols <- brewer.pal(3, "RdYlGn")
colors <- factor(index$SoH_index_bin)
plot_offset <- 0.05*(max(pat$data$pm25_A, pat$data$pm25_B, na.rm = TRUE))
gg <- ggplot(pat$data) +
  geom_point(aes(pat$data$datetime, pat$data$pm25_A), color= "red",pch = 16, cex = 0.5, alpha = 0.5) +
  geom_point(aes(pat$data$datetime, pat$data$pm25_B), color= "blue", pch = 16, cex = 0.5, alpha = 0.5) +
  geom_point(data = index, aes(index$datetime, (index$SoH_index_bin*0)-plot_offset, color = colors), pch = 15, cex =5) +
  scale_color_manual(values=c("0" = "firebrick", "1" = "gold", "2" = "green4")) +
  labs(title = paste0("SoH Index - ", station_name)) 

gg


min_plot <- plot_ly(calwa_soh) %>%
  add_trace(x = ~datetime, y = ~pm25_A_pm25_B_rsquared, 
            marker = list(opacity = 0.5, color = "blue")) 

ggplot(soh_emmett) +
  geom_point(aes(datetime, pm25_A_pm25_B_p_value), color= "red") +
  scale_y_continuous(trans = "log")

# --------- time testings -----------------------------------------------------

setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
pas <- pas_load(archival = TRUE)
pat <- pat_createNew(pas,"GRIZZLY WAY", startdate = 20191001, enddate = 20191118,
                     baseURL = "https://api.thingspeak.com/channels/")

timezone <- pat$meta$timezone
grizzly_pat <- pat

# filter using base for a 24 hour day 11/05/2019 at 5 am to 11/06/2019 at 5 am
start_hour <- parseDatetime(2019110505, timezone = timezone)
end_hour <- parseDatetime(2019110605, timezone = timezone)

small_pat_hour <- pat

data <- 
  small_pat_hour$data %>%
  filter(.data$datetime >= start_hour) %>%
  filter(.data$datetime < end_hour)

small_pat_hour$data <- data
timerange_utc <- range(small_pat_hour$data$datetime)
timerange_mt <- lubridate::with_tz(timerange_utc, tzone = timezone)

report_hour <- PurpleAirSoH_dailyPctReporting(small_pat_hour) # SHOULD contain TWO entries

# filter using base for a 24 hour day 11/05/2019 at 12 am to 11/06/2019 at 12 am

start_day <- parseDatetime(20191105, timezone = timezone)
end_day <- parseDatetime(20191106, timezone = timezone)

small_pat_day <- pat

data <- 
  small_pat_day$data %>%
  filter(.data$datetime >= start_day) %>%
  filter(.data$datetime < end_day)

small_pat_day$data <- data

report_day <- PurpleAirSoH_dailyPctReporting(small_pat_day) # SHOULD contain ONE entry


# filter using base for two full days plus taper on either end 11/05/2019 at 6 am to 11/08/2019 at 8 am

start_days_hours <- parseDatetime(2019110506, timezone = timezone)
end_days_hours <- parseDatetime(2019110808, timezone = timezone)

small_pat_days_hours <- pat

data <- 
  small_pat_days_hours$data %>%
  filter(.data$datetime >= start_days_hours) %>%
  filter(.data$datetime < end_days_hours)

small_pat_days_hours$data <- data

report_days_hours <- PurpleAirSoH_dailyPctReporting(small_pat_days_hours) # SHOULD contain FOUR entries


# ------------------------------------------------------------------------------
# loop through all of california stations and load pat for one day.
setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
pas <- pas_load()
pas <- pas_filter(pas, stateCode == "CA", DEVICE_LOCATIONTYPE == "outside")
test_pas <- pas_filter(pas,stringr::str_detect(label, "B") )
labels <- unique(pas_getLabels(pas))

soh_all <- data.frame()
system.time(
  for (sensor in labels) {
    
    print(sensor)
    result <- try({
      pat <- pat_load(label = sensor, startdate = 20191130, enddate = 20191201, timezone = "America/Los_Angeles")
    }, silent = TRUE)
    
    if ( ! "try-error" %in% class(result) ) {
      result <- try({
        soh <- pat_dailySoHIndex(pat) %>%
          dplyr::mutate(label = sensor)
        
        soh_all <-
          soh_all %>%
          dplyr::bind_rows(soh)
      }, silent = TRUE)
    }
    
  })

hist(soh_all$SoH_index, n = 80, main = "State of health of California PurpleAir sensors", xlab = "SoH Index", col = "#a128cd")


gg <- ggplot(soh_all, aes(SoH_index)) +
  geom_histogram(data=subset(soh_all, SoH_index_bin == '0'), 
                fill = "firebrick", color = "firebrick", alpha = 0.6, bins = 40) +
  geom_histogram(data=subset(soh_all, SoH_index_bin == '1'), 
                 fill = "goldenrod1", color = "goldenrod1", alpha = 0.6, bins = 40) +
  geom_histogram(data=subset(soh_all, SoH_index_bin == '2'), 
                 fill = "mediumseagreen", color = "mediumseagreen", alpha = 0.6, bins = 40) +
  xlab("SoH Index") +
  labs(title = "State of health histogram of California PurpleAir sensors")

gg

# df <- data.frame(
#   x = c(0, 0.2, 0.8, 1.0),
#   y = c(-15, -15, -15, -15)
# )
# 
# gg <- ggplot(soh_all) +
#   geom_histogram(aes(SoH_index), bins = 30, fill = "#a128cd", color = "#a128cd", alpha = 0.5 ) +
#   xlab("SoH Index") +
#   labs(title = "State of health of California PurpleAir sensors") +
#   geom_rect(data = df, aes(xmin = x[1], xmax = x[2], ymin = y[1], ymax = y[1] - 20), color = "firebrick", fill = "firebrick") +
#   geom_rect(data = df, aes(xmin = x[2], xmax = x[3], ymin = y[1], ymax = y[1] - 20), color = "goldenrod1", fill ="goldenrod1" ) +
#   geom_rect(data = df, aes(xmin = x[3], xmax = x[4], ymin = y[1], ymax = y[1] - 20), color = "mediumseagreen", fill = "mediumseagreen")
# 
# 
# gg





