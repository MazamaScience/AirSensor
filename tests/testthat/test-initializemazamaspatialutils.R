context("test-initializemazamaspatialutils")

# Want to test 
# 1) Correct errors with faulty input - is this already tested at the 
# MazamaSpatialUtils level?


# 2) An uneccessary test?
test_that("MazamaSpatialUtils package is loaded", {
  expect_true("MazamaSpatialUtils" %in% .packages())
})