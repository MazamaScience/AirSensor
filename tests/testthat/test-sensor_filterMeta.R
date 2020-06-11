context("test-sensor_filterMeta")

test_that("meta and data still agree", {
  data("example_sensor")

  filtered <-
    example_sensor %>%
    sensor_filterMeta(.data$monitorID == 'Probably not a monitorID label')

  expect_length(filtered$meta$monitorID, 0)
})
