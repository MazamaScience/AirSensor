
library(PWFSLSmoke)
monitor_loadAnnual(2019) %>%
  monitor_subset(stateCodes='WA', tlim=c(20190101,20191130)) %>%
  monitor_dailyStatistic() %>%
  monitor_timeseriesPlot(style = 'gnats', ylim=c(0,300), xpd=NA)
addAQIStackedBar()
addAQILines()
title("WA 2019 -- Federal Monitor Daily Average PM2.5")
aqi <- AQI
