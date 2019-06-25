context("test-pas_filter")

test_that("input validation works", {
  data("example_pas")
  
  expect_error(pas_filter(NULL))
  expect_error(pas_filter(1:10))
  #expect_error(pas_filter(example_pas, param="PM2.5"))
  #expect_error(pas_filter(example_pas, outsideOnly = 'outside'))
  #expect_error(pas_filter(example_pas, radius=NA))
})

test_that("pa_synoptic class returned", { 
  expect_true(pas_isPas(pas_filter(pas_filter(example_pas, stateCode == "CA"))))
})
