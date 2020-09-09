context("test-pas_filter")

test_that("input validation works", {
  data("example_pas")
  
  expect_error(pas_filter(example_pas, "pm25_current > 70.0"))
  expect_error(pas_filter(pm25_current > 70.0, example_pas))
})

test_that("pa_synoptic class returned", { 
  expect_true(pas_isPas(pas_filter(example_pas, stateCode == "CA")))
})
