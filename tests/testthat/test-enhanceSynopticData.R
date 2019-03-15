context("test-enhanceSynopticData")

test_that("input validation works", {
  # Load package internal, test 'pas' object
  data("example_pas")
  
  # Catch errors first
  expect_error(enhanceSynopticData(c(1:4)))
  expect_error(enhanceSynopticData(pas_load_sample,
                                   countryCodes = c(1:4)))
  expect_error(enhanceSynopticData(pas_load_sample, 
                                   countryCodes = c('US','MX','XX')))
  expect_error(enhanceSynopticData(pas_load_sample, 
                                   includePWFSL = "X"))
})
