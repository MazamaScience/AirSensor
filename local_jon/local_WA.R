library(PWFSLSmoke)
library(AirSensor)

setArchiveBaseDir("/Users/jonathan/Projects/PWFSL/2020/sensor-data-ingest-v1/output")

pas <- pas_load()

pat <- pat_createNew(pas, label = "Ballard")
pat_multiplot(pat)

as <- pat_createAirSensor(pat)
PWFSLSmoke::monitor_timeseriesPlot(as, shadedNight = TRUE)


