context("test-pas_load")

test_that("input validation works", {
  expect_error(pas_load(countryCodes = c(1:4)))
  expect_error(pas_load(countryCodes = c('US','MX','XX')))
  expect_error(pas_load(includePWFSL = "X"))
  expect_error(pas_load(lookbackDays = 0))
})
