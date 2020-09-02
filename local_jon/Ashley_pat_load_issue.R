library(dplyr)
library(stringr)

library(AirSensor)
setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1/")

# Create a 'pas' object for all current and historical SCAQMD sensors
pas_old <- pas_load(datestamp = "20191001", archival = TRUE)
pas_latest <- pas_load(archival = TRUE) 

# Handle any improper typing
pas_old$deviceID <- as.character(pas_old$deviceID)
pas_latest$deviceID <- as.character(pas_latest$deviceID)

# NOTE:  Determine distinct records by using both the channel-specific ID
# NOTE:  and the locationID. If the sensor moves, we will get a new
# NOTE:  locationID and keep those records. Otherwise we retain the A- and
# NOTE:  B-channels but remove any subsequent records from pat_latest that
# NOTE:  have the same sensor in the same location.

pas <- 
  dplyr::bind_rows(pas_old, pas_latest) %>%
  dplyr::distinct(THINGSPEAK_PRIMARY_ID, locationID, .keep_all = TRUE) %>%
  pas_filter(stringr::str_detect(label, "^[Ss][Cc][Ss][Bb]"))
















# Create a 'pas' object for all current and historical SCSB sensors
pas1 <- pas_load(datestamp = "20191001", archival = TRUE)
pas2 <- pas_load(archival = TRUE)
pas_all1 <- pas_filter(pas = pas1, stringr::str_detect(label, "^[Ss][Cc][Ss][Bb]"));
pas_all2 <- pas_filter(pas = pas2, stringr::str_detect(label, "^[Ss][Cc][Ss][Bb]"));
pas_all <- rbind(pas_all1, pas_all2)
pas_all <- distinct(pas_all, label, .keep_all = TRUE)

# Get labels for unique sensors
labels <-
  pas_all %>%
  dplyr::filter(is.na(parentID)) %>%
  dplyr::pull(label)

label <- labels[1]

id <- pas_getDeviceDeploymentIDs(pas_all, label)

pat1 <- pat_load(
  pas = pas_all, 
  label = labels[1],
  startdate = 20171001, 
  enddate = 20180101, 
  timezone = "America/Los_Angeles"
)



sens <- pas_all$label; sens_a <- sens[which(str_detect(sens, " B") == FALSE)]

i = 1; label <- sens_a[i]; pat <- NULL; id <- pas_getDeviceDeploymentIDs(pas_all, label)
pat <- pat_load(id = id, pas = pas_all, label = label, startdate = 20171001, enddate = 20200801, timezone = "America/Los_Angeles")
i = 2; label <- sens_a[i]; pat <- NULL; id <- pas_getDeviceDeploymentIDs(pas_all, label)
pat <- pat_load(id = id, pas = pas_all, label = label, startdate = 20171001, enddate = 20200801, timezone = "America/Los_Angeles")
i = 5; label <- sens_a[i]; pat <- NULL; id <- pas_getDeviceDeploymentIDs(pas_all, label)
pat <- pat_load(id = id, pas = pas_all, label = label, startdate = 20171001, enddate = 20200801, timezone = "America/Los_Angeles")

i = 1; label <- sens_a[i]; pat <- NULL; id <- pas_getDeviceDeploymentIDs(pas_all, label)
pat <- pat_createNew(id = id, pas = pas_all, label = label, startdate = 20171001, enddate = 20200801, timezone = "America/Los_Angeles")
i = 5; label <- sens_a[i]; pat <- NULL; id <- pas_getDeviceDeploymentIDs(pas_all, label)
pat <- pat_createNew(id = id, pas = pas_all, label = label, startdate = 20171001, enddate = 20200801, timezone = "America/Los_Angeles")