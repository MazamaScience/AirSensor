context("test-initializeMazamaSpatialUtils")


test_that("A spatial data directory exists", {
  expect_true(dir.exists("~/Data/Spatial")) 
})

test_that("Spatial Data is loaded", {
  initializeMazamaSpatialUtils()
  expect_true(exists("NaturalEarthAdm1"))
})

