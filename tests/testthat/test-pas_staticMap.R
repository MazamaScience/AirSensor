context('test-pas_staticMap')

test_that(
  'static map input validation works', 
  {
    expect_error(pas_staticMap())
    expect_error(pas_staticMap(AirSensor::example_pas, parameter = NULL))
  }
)