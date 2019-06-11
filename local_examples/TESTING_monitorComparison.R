# Can we compare sensor data with PWFSL monitor data?

# required libraries
library(PWFSLSmoke)
library(AirSensor)
library(dplyr)

# Get April data for two Nipomo sensors
scnp_08 <- get(load("pat_SCNP_08_20190401.rda"))
scnp_14 <- get(load("pat_SCNP_14_20190401.rda"))

# Get April data for two Nipomo monitors

ca_april <-
  get(load("daily_monitor.RData")) %>%
  monitor_subset(stateCodes = "CA") %>%
  monitor_subset(tlim = c(2019040100,2019043023))

Nipomo_Mesa <- monitor_subset(ca_april, monitorIDs = "060792007_01")
Nipomo_Mesa2 <- monitor_subset(ca_april, monitorIDs = "060792004_01")

# > names(scnp_08$data)
# [1] "datetime"    "pm25_A"      "pm25_B"      "temperature" "humidity"   
# [6] "uptime"      "adc0"        "rssi"        "datetime_A"  "datetime_B" 
# > names(Nipomo_Mesa$data)
# [1] "datetime"     "060792007_01"
# > names(scnp_08$meta)
# [1] "ID"                             "label"                         
# [3] "sensorType"                     "DEVICE_LOCATIONTYPE"           
# [5] "THINGSPEAK_PRIMARY_ID"          "THINGSPEAK_PRIMARY_ID_READ_KEY"
# [7] "longitude"                      "latitude"                      
# [9] "countryCode"                    "stateCode"                     
# [11] "timezone"                       "pwfsl_closestDistance"         
# [13] "pwfsl_closestMonitorID"        
# > names(Nipomo_Mesa$meta)
# [1] "monitorID"             "longitude"             "latitude"             
# [4] "elevation"             "timezone"              "countryCode"          
# [7] "stateCode"             "siteName"              "agencyName"           
# [10] "countyName"            "msaName"               "monitorType"          
# [13] "siteID"                "instrumentID"          "aqsID"                
# [16] "pwfslID"               "pwfslDataIngestSource" "telemetryAggregator"  
# [19] "telemetryUnitID"      

# > sort(unique(diff(scnp_08$data$datetime)))     # seconds
# [1]     0    60   120   180   240   300   360   420   480   540   600   660   720
# [14]   780   840   900  1080  1200  1260  1920  2040  2160  2220  2580  2940  3000
# [27]  3060  3900  4260  4380  8940 13800 19620
# > sort(unique(diff(Nipomo_Mesa$data$datetime))) # hours
# [1] 1

# ast_08 <- createASTimeseriesObject(scnp_08, period = "1 hour")
# ast_14 <- createASTimeseriesObject(scnp_14, period = "1 hour")

# > names(ast_08$meta)
# [1] "ID"          "label"       "longitude"   "latitude"    "countryCode"
# [6] "stateCode"
# > names(ast_08$data)
# [1] "date"           "pm25"           "humidity"       "temperature"   
# [5] "pm25_sd"        "humidity_sd"    "temperature_sd" "pm25_ct"       
# [9] "humidity_ct"    "temperature_ct" "pm25_qc"        "humidity_qc"   
# [13] "temperature_qc"
# > sort(unique(diff(ast_08$data$date)))
# [1] 1
ast_08 <- pat_createASTimeseries(scnp_08, period = "1 hour")
ast_14 <- pat_createASTimeseries(scnp_14, period = "1 hour")

layout(matrix(seq(3)))
ast_08$data %>% select(datetime, humidity) %>% plot()
ast_08$data %>% select(datetime, humidity_ct) %>% plot()
ast_08$data %>% select(datetime, humidity_sd) %>% plot()

ast_08$data %>% select(datetime, temperature) %>% plot()
ast_08$data %>% select(datetime, temperature_ct) %>% plot()
ast_08$data %>% select(datetime, temperature_sd) %>% plot()

ast_08$data %>% select(datetime, pm25) %>% plot()
ast_08$data %>% select(datetime, pm25_ct) %>% plot()
ast_08$data %>% select(datetime, pm25_sd) %>% plot()

layout(1)



monitor_timeseriesPlot(Nipomo_Mesa, type = 'l')

points(scnp_08$data$datetime, scnp_08$data$pm25_A, 
       pch=15, cex = 1, col = adjustcolor('red', 0.02))

lines(ast_08$data$date, ast_08$data$pm25, col = 'white')

# lines(ast_14$data$date, ast_14$data$pm25, col = 'blue')



