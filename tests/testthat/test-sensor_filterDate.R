context('test-sensor_filterDate')
data('example_sensor')
test_that(
  'Filter Date works', 
  expect_equivalent(
    sensor_filterDate(
      example_sensor, 
      startdate = 20180815, 
      enddate = 20180816, 
      timezone = 'UTC'
    )$data$datetime, 
    sensor_filter(
      example_sensor, 
      .data$datetime >= lubridate::ymd_h(2018081500), 
      .data$datetime < lubridate::ymd_h(2018081600)
    )$data$datetime
  )
)
