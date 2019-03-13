context("test-pas_load")

test_that("input validation works", {
  expect_error(pas_load_sample(countryCodes = c(1:4)))
  expect_error(pas_load_sample(countryCodes = c('US','MX','XX')))
  expect_error(pas_load_sample(includePWFSL = "X"))
  expect_error(pas_load_sample(lookbackDays = 0))
})
