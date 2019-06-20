# required libraries
library(AirSensor)

# Always apply out-of-spec QC
pat <- 
  example_pat %>% 
  pat_filterDate(20180701, 20180708) %>% 
  pat_qc()

pat_multiplot(pat, sampleSize = NULL)

# Create aggregation statistics
agg <- pat_aggregate(pat, period = "1 hour")

# Set up default colors
colors <- c(rgb(0.9, 0.25, 0.2), rgb(0.2, 0.25, 0.9))

# A/B means plot
df <-
  agg %>%
  dplyr::select(datetime, pm25_A_mean, pm25_B_mean) %>%
  tidyr::gather("channel", "value", -datetime)

gg1 <- 
  ggplot(df, aes(datetime, value, color = channel)) +
  geom_point() +
  scale_color_manual(values=colors) +
  scale_y_continuous() + 
  theme(legend.position = "none") +
  ggtitle("A/B means")

# p-value plot
df <-
  agg %>%
  dplyr::select(datetime, pm25_p)

gg2 <- 
  ggplot(df, aes(datetime, pm25_p)) +
  geom_point() +
  scale_y_log10() + 
  ggtitle("p value")

multi_ggplot(gg1, gg2)

# A-B mean difference
df <-
  agg %>%
  dplyr::select(datetime, pm25_A_mean, pm25_B_mean) %>%
  dplyr::mutate(mean_diff = abs(pm25_A_mean - pm25_B_mean))

gg3 <- 
  ggplot(df, aes(datetime, mean_diff)) +
  geom_point() +
  scale_y_continuous() + 
  ggtitle("A/B mean difference")

# A-B min count
df <-
  agg %>%
  dplyr::mutate(min_count = min(pm25_A_count, pm25_B_count, na.rm = TRUE))

gg4 <- 
  ggplot(df, aes(datetime, min_count)) +
  geom_point() +
  scale_y_continuous() + 
  ggtitle("A/B minimum count")





