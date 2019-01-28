context("test-enhanceSynopticData")

test_that("input validation works", {
  nondf <- c(1,2,3,4)
  expect_error(enhanceSynopticData(nondf))
})
