# Create annual sensor files:

library(AirSensor)
setArchiveBaseUrl("https://airsensor.aqmd.gov/PurpleAir/v1")

# ----- 2017 -------------------------------------------------------------------

airsensor <-
  sensor_loadMonth("scaqmd", 201710 ) %>%
  sensor_join( sensor_loadMonth("scaqmd", 201711) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 201712) )

# PWFSLSmoke::monitor_timeseriesPlot(airsensor, style = 'gnats')

save(list = "airsensor", file = "airsensor_scaqmd_2017.rda")

# ----- 2018 -------------------------------------------------------------------

first_half <-
  sensor_loadMonth("scaqmd", 201801 ) %>%
  sensor_join( sensor_loadMonth("scaqmd", 201802) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 201803) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 201804) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 201805) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 201806) )

second_half <- 
  sensor_loadMonth("scaqmd", 201807) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 201808) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 201809) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 201810) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 201811) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 201812) )

airsensor <- sensor_join(first_half, second_half)

# PWFSLSmoke::monitor_timeseriesPlot(airsensor, style = 'gnats')

save(list = "airsensor", file = "airsensor_scaqmd_2018.rda")

# ----- 2019 -------------------------------------------------------------------

first_half <-
  sensor_loadMonth("scaqmd", 201901 ) %>%
  sensor_join( sensor_loadMonth("scaqmd", 201902) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 201903) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 201904) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 201905) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 201906) )

second_half <- 
  sensor_loadMonth("scaqmd", 201907) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 201908) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 201909) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 201910) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 201911) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 201912) )

airsensor <- sensor_join(first_half, second_half)

# PWFSLSmoke::monitor_timeseriesPlot(airsensor, style = 'gnats')

save(list = "airsensor", file = "airsensor_scaqmd_2019.rda")

# ----- 2020 -------------------------------------------------------------------

first_half <-
  sensor_loadMonth("scaqmd", 202001 ) %>%
  sensor_join( sensor_loadMonth("scaqmd", 202002) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 202003) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 202004) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 202005) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 202006) )

second_half <- 
  sensor_loadMonth("scaqmd", 202007) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 202008) ) %>%   
  sensor_join( sensor_loadMonth("scaqmd", 202009) ) #%>%   
  #sensor_join( sensor_loadMonth("scaqmd", 202010) ) %>%   
  #sensor_join( sensor_loadMonth("scaqmd", 202011) ) %>%   
  #sensor_join( sensor_loadMonth("scaqmd", 202012) )

airsensor <- sensor_join(first_half, second_half)

# PWFSLSmoke::monitor_timeseriesPlot(airsensor, style = 'gnats')

save(list = "airsensor", file = "airsensor_scaqmd_2020.rda")

