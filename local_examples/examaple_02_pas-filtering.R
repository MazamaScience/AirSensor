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
sb_north <- 33.78
sb_south <- 33.75
sb_west <- -118.10
sb_east <- -118.07

scsb <-
  ca %>%
  pas_filter(longitude > sb_west & longitude < sb_east) %>%
  pas_filter(latitude > sb_south & latitude < sb_north)

pas_leaflet(scsb)

# Let's use a regular expression to find all SCAQMD sensors
scaqmd <-
  ca %>% pas_filter(str_detect(label, "^[Ss][Cc].._"))

pas_leaflet(scaqmd)

# Now for some static maps.

pas_esriMap(scaqmd)

pas_esriMap(scsb)



