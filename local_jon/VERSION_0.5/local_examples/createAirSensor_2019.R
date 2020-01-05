# Required libraries
library(PWFSLSmoke)
library(AirSensor)

setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")

# ------ Create annual airsensor object ----------------------------------------

jan <- sensor_loadMonth("scaqmd", 201901)
feb <- sensor_loadMonth("scaqmd", 201902)
mar <- sensor_loadMonth("scaqmd", 201903)
apr <- sensor_loadMonth("scaqmd", 201904)
may <- sensor_loadMonth("scaqmd", 201905)
jun <- sensor_loadMonth("scaqmd", 201906)
jul <- sensor_loadMonth("scaqmd", 201907)
aug <- sensor_loadMonth("scaqmd", 201908)
sep <- sensor_loadMonth("scaqmd", 201909)
oct <- sensor_loadMonth("scaqmd", 201910)

monitorIDs <- union(jan$meta$monitorID, feb$meta$monitorID)
airsensor <- PWFSLSmoke::monitor_join(jan, feb, monitorIDs = monitorIDs)

monitorIDs <- union(airsensor$meta$monitorID, mar$meta$monitorID)
airsensor <- PWFSLSmoke::monitor_join(airsensor, mar, monitorIDs = monitorIDs)

monitorIDs <- union(airsensor$meta$monitorID, apr$meta$monitorID)
airsensor <- PWFSLSmoke::monitor_join(airsensor, apr, monitorIDs = monitorIDs)

monitorIDs <- union(airsensor$meta$monitorID, may$meta$monitorID)
airsensor <- PWFSLSmoke::monitor_join(airsensor, may, monitorIDs = monitorIDs)

monitorIDs <- union(airsensor$meta$monitorID, jun$meta$monitorID)
airsensor <- PWFSLSmoke::monitor_join(airsensor, jun, monitorIDs = monitorIDs)

monitorIDs <- union(airsensor$meta$monitorID, jul$meta$monitorID)
airsensor <- PWFSLSmoke::monitor_join(airsensor, jul, monitorIDs = monitorIDs)

monitorIDs <- union(airsensor$meta$monitorID, aug$meta$monitorID)
airsensor <- PWFSLSmoke::monitor_join(airsensor, aug, monitorIDs = monitorIDs)

monitorIDs <- union(airsensor$meta$monitorID, sep$meta$monitorID)
airsensor <- PWFSLSmoke::monitor_join(airsensor, sep, monitorIDs = monitorIDs)

monitorIDs <- union(airsensor$meta$monitorID, oct$meta$monitorID)
airsensor <- PWFSLSmoke::monitor_join(airsensor, oct, monitorIDs = monitorIDs)

save(list = "airsensor", file = "airsensor_scaqmd_2019.rda")

