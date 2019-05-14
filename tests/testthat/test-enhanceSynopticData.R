context("test-enhanceSynopticData")

test_that("input validation works", {
  # Load package internal, test 'pas' object
  data("example_pas_raw")
  
  # Catch errors first
  expect_error(enhanceSynopticData(c(1:4)))
  expect_error(enhanceSynopticData(example_pas_raw,
                                   countryCodes = c(1:4)))
  expect_error(enhanceSynopticData(example_pas_raw, 
                                   countryCodes = c('US','MX','XX')))
  expect_error(enhanceSynopticData(example_pas_raw, 
                                   includePWFSL = "X"))
  
})
