context('test-pat_createAirSensor')
data('example_pat', 'example_sensor')

test_that(
  'converts to airsensor, ws_monitor objects', 
  expect_s3_class(
    pat_createAirSensor(example_pat), 
    c("airsensor", "ws_monitor")
  )
)

test_that(
  'hourly datetime axis is continious',
  expect_true(
    any(as.logical(diff(pat_createAirSensor(example_pat)$data$datetime)))
  )
)
 
test_that(
  'FUN parameter check', 
  expect_error(
    pat_createAirSensor(example_pat, FUN = 1)
  )
)

test_that(
  'parameter parameter check', 
  expect_error(
    pat_createAirSensor(example_pat, parameter = 'DNE')
  )
)