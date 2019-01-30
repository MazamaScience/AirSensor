context("test-enhanceSynopticData")

test_that("input validation works", {
  # Load package internal, test 'pas' object
  data("pas_Jan25")
  
  # Catch errors first
  expect_error(enhanceSynopticData(c(1:4)))
  expect_error(enhanceSynopticData(pas,
                                   countryCodes = c(1:4)))
  expect_error(enhanceSynopticData(pas, 
                                   countryCodes = c('US','MX','XX')))
  expect_error(enhanceSynopticData(pas, 
                                   includePWFSL = "X"))
})
