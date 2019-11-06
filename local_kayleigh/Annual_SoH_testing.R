library(AirSensor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)
library(purrr)
library(plotly)
library(gridExtra)
library(skimr)

# ---- Series of pats required for testing all the SoH() functions -----------

ex_pat <- example_pat

# pat for testing pctDC
setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
pas <- pas_load(archival = TRUE)
pat_b_zero <- pat_createNew(pas, "SCAP_46", startdate = "2019-07-01", enddate = "2019-07-08", timezone = "America/Los_Angeles")

# new pat for testing pctReporting
pat_new <- pat_createNew(pas, label = "#SFAQ16", startdate = "20190601", enddate = "20190822")

# ----- Whole year from USFS -------------------------------------------------

setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
pas <- pas_load()
pas %>% pas_filter(stateCode == "WA") %>% pas_leaflet()

pat <- pat_load("MV Clean Air Ambassador @ Willowbrook Farm", startdate = 20190101, enddate = 20191023) #summer gap
pat <- pat_load("Spokane South Hill #1", startdate = 20190101, enddate = 20191023) #super clean station
pat %>% pat_multiplot()

SoH <- pat_dailyStateOfHealth(pat)

# ----- Plotting ------------------------------------------------------------
plot(pat$data$datetime, pat$data$pm25_A, pch='.', 
     xlim = MazamaCoreUtils::timeRange(starttime = "20190801", endtime = "20191101", timezone = "UTC") ,
     ylim = c(0, 200))
plot(pat$data$datetime, pat$data$pm25_A, pch='.', ylim = c(0, 200))
points(SoH$datetime, SoH$pm25_A_pctDC, col = "red")
points(SoH$datetime, SoH$pm25_A_pctReporting, col = "blue")
points(SoH$datetime, SoH$pm25_A_pctValid, col = "green")



points(tbl$datetime, tbl$humidity_count, pch = '.', col = 'red')

# ----- Experiment with ggplot -------------------------------------------------

pat_sub <- pat_sample(pat, keepOutliers = TRUE)
d <- pat_sub$data

gg1 <- ggplot(d) +
  geom_point(aes(datetime, pm25_A), alpha = 0.5, color = "darkred") +
  geom_point(aes(datetime, pm25_B), alpha = 0.3, color = "darkblue") +
  labs(y = "ug/m3", x = " ") +
  ggtitle("pm25")

gg1 + theme(legend.title = element_text(size=12, color = "firebrick"), 
           legend.text = element_text(size=10),
           legend.key=element_rect(fill='springgreen'))

gg2 <- ggplot(SoH) +
  geom_point(aes(datetime, pm25_A_pctReporting), alpha = 0.5, color = "darkred") +
  geom_point(aes(datetime, pm25_B_pctReporting), alpha = 0.3, color = "darkblue") + 
  labs(y = "percent") +
  labs(x = " ")

gg3 <- ggplot(SoH) +
  geom_line(aes(datetime, pm25_A_pm25_B_cor), alpha = 0.6, color = "darkred") +
  geom_line(aes(datetime, pm25_A_pm25_B_slope), alpha = 0.5, color = "darkblue") +
  labs(y = " ") + 
  labs(x = "date")

gg4 <- ggplot(d) +
  geom_point(aes(datetime, temperature), alpha = 0.5, color = "darkgreen") +
  geom_point(aes(datetime, humidity), alpha = 0.3, color = "darkorange") +
  labs(y = " ") +
  labs(x = " ")

gg5 <- ggplot(SoH) +
  geom_point(aes(datetime, temperature_pctReporting), alpha = 0.5, color = "darkgreen") +
  geom_point(aes(datetime, humidity_pctReporting), alpha = 0.3, color = "darkorange") +
  labs(y = " ") +
  labs(x = " ")

gg6 <- ggplot(SoH) +
  geom_line(aes(datetime, temperature_humidity_cor), alpha = 0.6, color = "darkgreen") +
  geom_line(aes(datetime, pm25_A_humidity_cor), alpha = 0.5, color = "darkorange") +
  labs(y = " ") +
  labs(x = "date")

gg <- multi_ggplot(gg1, gg2, gg3, gg4, gg5, gg6, cols = 2) +
  labs( title = "Sensor State of Health")
# gg <- grid.arrange(gg1, gg4, gg2, gg5, gg3, gg6, ncol=2) +
#   labs( title = "Sensor State of Health")

# --------- Flat-lined plot ---------------------------------------------------

# wanted plots: all _pctReporting, A and B _pctDC, (?) all pctValid (?), 
# A/B cor, temp/humd cor, A/B intercept and slope = 10 (plus valid if want)

SoH_sub_normalized <- dplyr::select(SoH, contains("_pctReporting"), contains("pm25_A_pm25_B"),
                         "temperature_humidity_cor", "pm25_A_pctDC", "pm25_B_pctDC",
                         "datetime") %>%
  dplyr::mutate(pm25_A_pctReporting = pm25_A_pctReporting/max(pm25_A_pctReporting, na.rm = TRUE)) %>%
  dplyr::mutate(pm25_B_pctReporting = pm25_B_pctReporting/max(pm25_B_pctReporting, na.rm = TRUE)) %>%
  dplyr::mutate(temperature_pctReporting = temperature_pctReporting/max(temperature_pctReporting, na.rm = TRUE)) %>%
  dplyr::mutate(humidity_pctReporting = humidity_pctReporting/max(humidity_pctReporting, na.rm = TRUE)) %>%
  dplyr::mutate(pm25_A_pm25_B_cor = pm25_A_pm25_B_cor/max(abs(pm25_A_pm25_B_cor), na.rm = TRUE)) %>%
  dplyr::mutate(pm25_A_pm25_B_slope = pm25_A_pm25_B_slope/max(abs(pm25_A_pm25_B_slope), na.rm = TRUE)) %>%
  dplyr::mutate(pm25_A_pm25_B_intercept = pm25_A_pm25_B_intercept/max(abs(pm25_A_pm25_B_intercept), na.rm = TRUE)) %>%
  dplyr::mutate(temperature_humidity_cor = temperature_humidity_cor/max(abs(temperature_humidity_cor), na.rm = TRUE)) %>%
  dplyr::mutate(pm25_A_pctDC = 100-pm25_A_pctDC) %>%
  dplyr::mutate(pm25_B_pctDC = 100-pm25_B_pctDC) %>%
  dplyr::mutate(pm25_A_pctDC = pm25_A_pctDC/max(pm25_A_pctDC, na.rm = TRUE)) %>%
  dplyr::mutate(pm25_B_pctDC = pm25_B_pctDC/max(pm25_B_pctDC, na.rm = TRUE)) %>%
  tidyr::gather(variable, value, -datetime) %>%
  transform(id = as.integer(factor(variable)))

labels <- c("humidity %reporting", "pm25 A %DC", "pm25 A %reporting", "pm25 A/B correlation",
            "pm25 A/B intercept", "pm25 A/B slope", "pm25 B %DC", "pm25 B %reporting",
            "temp/humidity correlation", "temp %reporting")


gg <- ggplot(SoH_sub_normalized, aes(datetime, value)) +
  geom_line()+
  facet_wrap(vars(variable), nrow = 10, strip.position = c("top")) +
  scale_y_continuous(breaks = seq(-1, 1, by=1), limits=c(-1,1)) 
  #theme(panel.background = element_rect(fill = "white"))
  # theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(color = "lightgray"))
        #element_text(hjust = 0.25)
gg

 








# ----- Experiment with Plotly -------------------------------------------------


pat_sub <- pat_sample(pat, keepOutliers = TRUE)
d <- pat_sub$data

top_y_axis <-  list(showgrid = T , zeroline = T , nticks = 5 , showline = F ,
                    title = "ug/m3" , mirror = "all" )
middle_y_axis <-  list(showgrid = T , zeroline = T , nticks = 5 , showline = F ,
                       title = "Percent" , mirror = "all" )
bottom_y_axis <- list(showgrid = T , zeroline = T , nticks = 6 , showline = F ,
                       mirror = "all" )
 
p1 <- plot_ly(d) %>%
  add_trace(x = ~datetime, y = ~pm25_A, name = "pm25_A", 
              marker = list(opacity = 0.5, color = "darkred")) %>%
  add_trace(x = ~datetime, y = ~pm25_B, name = "pm25_B",
            marker = list(opacity = 0.3, color = "darkblue"))

p2 <- plot_ly(SoH) %>%
  add_trace(x = ~datetime, y = ~pm25_A_pctReporting, name = "pm25_A %Reporting",
            marker = list(opacity = 0.5, color = "darkred")) %>%
  add_trace( x = ~datetime, y = ~pm25_B_pctReporting, name = "pm25_B %Reporting",
            marker = list(opacity = 0.3, color = "darkblue")) 

p3 <- plot_ly(SoH) %>%
  add_lines(x = ~datetime, y = ~pm25_A_pm25_B_cor, name = "pm25A and pm25B correlation",
            line = list(color = "darkred"), opacity = 0.5) %>%
  add_lines(x = ~datetime, y = ~pm25_A_pm25_B_slope, name = "pm25A and pm25B lm slope",
            line = list(color = "darkblue"), opacity = 0.3)

p4 <- plot_ly(d) %>%
  add_trace(x = ~datetime, y = ~temperature, name = "temperature", 
            marker = list(opacity = 0.3, color = "darkgreen")) %>%
  add_trace(x = ~datetime, y = ~humidity, name = "humidity",
            marker = list(opacity = 0.5, color = "darkorange"))

p5 <- plot_ly(SoH) %>%
  add_trace(x = ~datetime, y = ~temperature_pctReporting, name = "temperature %Reporting",
            marker = list(opacity = 0.3, color = "darkgreen")) %>%
  add_trace( x = ~datetime, y = ~humidity_pctReporting, name = "humidity %Reporting",
             marker = list(opacity = 0.5, color = "darkorange")) 

p6 <- plot_ly(SoH) %>%
  add_lines(x = ~datetime, y = ~temperature_humidity_cor, name = "temperature and humidity correlation",
            line = list(color = "darkgreen"), opacity = 0.3) %>%
  add_lines(x = ~datetime, y = ~pm25_A_humidity_cor, name = "pm25A and humidity correlation",
            line = list(color = "darkorange"), opacity = 0.7) 


s1 <- subplot(p1, p2, p3, nrows = 3, shareX = TRUE) 
  
s2 <- subplot(p4, p5, p6, nrows = 3, shareX = TRUE) 

subplot(s1, s2) %>%
  plotly::layout(title = "Sensor State of Health", showlegend = FALSE, yaxis = bottom_y_axis, 
                 yaxis2 = middle_y_axis, yaxis3 = top_y_axis)






# ---- test
p1 <- ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point()

p1 + facet_grid(
  vs + am ~ gear,
  labeller = labeller(vs = label_both, am = label_value)
)











