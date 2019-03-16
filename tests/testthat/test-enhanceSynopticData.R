context("test-enhanceSynopticData")

test_that("input validation works", {
  # Load package internal, test 'pas' object
  data("example_raw_pas")
  
  # Catch errors first
  expect_error(enhanceSynopticData(c(1:4)))
  expect_error(enhanceSynopticData(example_raw_pas,
                                   countryCodes = c(1:4)))
  expect_error(enhanceSynopticData(example_raw_pas, 
                                   countryCodes = c('US','MX','XX')))
  expect_error(enhanceSynopticData(example_raw_pas, 
                                   includePWFSL = "X"))
  
})
