# Experimenting with QC of raw pm25 data

# required libraries
library(AirSensor)
library(dplyr)

# Configurable parameters
avg.time = "10 min"
raw_color = "gray80"
raw_shape = 15
raw_size = 0.5
avg_color = "black"
avg_shape = 15
avg_size = 0.75
avg_lwd = 1

# Load this morning's synoptic data
pas <- pas_load()

# Filter for sensors in California
ca <-
  pas %>%
  filter(stateCode == 'CA')

# Load timeseries data from Nipomo
scnp_14 <- pat_load(pas, "SCNP_14")

# Merge the A and B channel data
df_A <- 
  scnp_14$data %>% 
  select(datetime_A, pm25_A) %>%
  filter(!is.na(pm25_A)) %>%
  rename(datetime = datetime_A, pm25 = pm25_A)

df_B <- 
  scnp_14$data %>% 
  select(datetime_B, pm25_B) %>%
  filter(!is.na(pm25_B)) %>%
  rename(datetime = datetime_B, pm25 = pm25_B)

df <- 
  bind_rows(df_A, df_B) %>%
  arrange(datetime)

# Change the name to "date" for use with openair::timeAverage

df_5minMean <-
  df %>%
  rename(date = datetime) %>%
  openair::timeAverage(avg.time = avg.time, 
                       data.thresh = 0,
                       statistic = "mean")

df_5minSd <-
  df %>%
  rename(date = datetime) %>%
  openair::timeAverage(avg.time = avg.time, 
                       data.thresh = 0,
                       statistic = "sd")

layout(matrix(seq(3)))

plot(df, pch=raw_shape, col=raw_color, cex=raw_size)
points(df_5minMean, pch=avg_shape, col=avg_color, cex=avg_size)
title(paste0("A & B raw data and ",avg.time," mean"))

plot(df_5minMean, type='l', col=avg_color, lwd=avg_lwd)
title(paste0(avg.time," min mean timeseries"))

plot(df_5minSd, pch=avg_shape, col=avg_color, cex=avg_size,
     ylab = "std dev.")
title(paste0(avg.time," min mean standard deviation"))


