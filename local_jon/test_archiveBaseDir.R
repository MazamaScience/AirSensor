#
# Work with a data archived created on the desktop machine
#

library(PWFSLSmoke)
library(AirSensor)

setArchiveBaseDir("/Users/jonathan/Projects/PWFSL/2020/sensor-data-ingest-v1/output")

pas <- pas_load()

pat <- pat_createNew(label = "Ballard", pas = pas)
pat_multiplot(pat)

sensor <- pat_createAirSensor(pat)
PWFSLSmoke::monitor_timeseriesPlot(sensor, shadedNight = TRUE)


