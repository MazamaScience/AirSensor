# Filtering a "pas" object

# required libraries
library(AirSensor)
library(stringr)             # for string matching

# Load this morning's synoptic data
pas <- pas_load()

# Filter for sensors in California
ca <-
  pas %>%
  filter(stateCode == 'CA')

# Look for labels of monitors near Seal Beach
pas_leaflet(ca)

# They all start with "SCSB" so let's filter for that label string
scsb <- 
  ca %>%
  pas_filter(str_detect(label, "^SCSB_"))

# Have a look
pas_leaflet(scsb, maptype = "satellite")

# Oops. They are wandering away from Seal Beach
# Let's remove the wanderers by filtering against particular labels
wanderers <- c("SCSB_20", "SCSB_40", "SCSB_35")
scsb <- 
  scsb %>%
  pas_filter(!label %in% wanderers)

pas_leaflet(scsb, maptype = "satellite")

# Or use Seal Beach coordinates to find sensors in a rectangle
n <- 33.78
s <- 33.75
w <- -118.10
e <- -118.07

scsb <-
  ca %>%
  pas_filterArea(w, e, s, n)

pas_leaflet(scsb)

# Let's use a regular expression to find all SCAQMD sensors
scaqmd <-
  ca %>% pas_filter(str_detect(label, "^[Ss][Cc].._"))

pas_leaflet(scaqmd)

# Now for some static maps.

pas_staticMap(scaqmd)

pas_staticMap(scsb, palette = "AQI", size = 4, zoomAdjust = 2)



