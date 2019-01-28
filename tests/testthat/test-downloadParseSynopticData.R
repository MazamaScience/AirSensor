context("test-downloadParseSynopticData")

test_that("input validation check for 'www.purpleair.com' in URL works", {
  expect_error(downloadParseSynopticData("DUMMY"))
})
