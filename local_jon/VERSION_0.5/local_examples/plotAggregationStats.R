library(AirSensor)
library(dplyr)
library(tidyr)
library(ggplot2)

setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")

# Get pas for Texas
pas <- pas_load()
pas_tx <- pas %>% pas_filter(stateCode == "TX")

# Get the pat of interest
pat <- pat_createNew(pas_tx, label = "Outside 3")

# Create hourly aggregation statistics
agg <- pat_aggregateOutlierCounts(pat)

# ---- Plot aggregation statistics ---------------------------------------------

# Convert from "wide" to "long" (aka "tidy")
data_long <- agg %>%
  gather(param, value, -datetime)

# Create plot
gg <- ggplot(data_long, aes(x = datetime, y = value)) +
  geom_line() +
  labs(title="Aggregation Statistics") +
  facet_wrap(~param)

# Look at the plot
print(gg)
