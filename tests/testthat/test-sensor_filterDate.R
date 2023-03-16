context('test-sensor_filterDate')
data('example_sensor')
test_that(
  'Filter Date works', 
  expect_equivalent(
    sensor_filterDate(
      example_sensor, 
      startdate = 20220702, 
      enddate = 20220705, 
      timezone = 'UTC'
    )$data$datetime, 
    sensor_filter(
      example_sensor, 
      .data$datetime >= lubridate::ymd_h(2022070200, tz = 'UTC'), 
      .data$datetime < lubridate::ymd_h(2022070500, tz = 'UTC')
    )$data$datetime
  )
)
