library(AirSensor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)

setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")

# Get pas for Louisiana
pas <- pas_load()
pas_la <- pas %>% pas_filter(stateCode == "LA")

# Get the pat of interest
pat_la <- pat_createNew(pas_la, label = "Island Park")

# Check out the data ---
# pat_multiplot(pat_la)
# pat_dygraph(pat_la)
# pat_scatterplot(pat_la)

# Create hourly aggregation statistics
agg_out <- pat_aggregateOutlierCounts(pat_la)
# agg <- pat_aggregate(pat_la)

# Sorting names for plot: -> currently runs and works, just doesnt't transfer to
# ggplot properly
a <- sort(names(agg_out))
a <- a[!a %in% c("pm25_df", "pm25_p", "pm25_t")]
a <- append(a, c("pm25_df", "pm25_p", "pm25_t"))

# a <- sort(names(agg_out)) %>%
#   str_remove_all(.,  c("pm25_df|pm25_p|pm25_t")) %>%
#   append(c("pm25_df", "pm25_p", "pm25_t"))


# Convert from "wide" to "long" (aka "tidy")
data_long <- agg_out %>%
  gather(param, value, -datetime)  

param <- factor(data_long$param, levels = a)

ggplot(data_long, aes(x = datetime, y = value)) +
  geom_line() +
  labs(title="Aggregation Statistics") +
  #facet_wrap(~param, scales = "free_y")
  facet_wrap(param, nrow = 5, scales = "free_y" )








