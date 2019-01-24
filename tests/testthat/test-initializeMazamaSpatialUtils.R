context("test-initializeMazamaSpatialUtils")

test_that("packages are loaded", {
  expect_true('MazamaSpatialUtils' %in% .packages())
  expect_true('MazamaCoreUtils' %in% .packages())
})

test_that("A spatial data directory exists", {
  expect_true(dir.exists("~/Data/Spatial")) 
})

test_that("Spatial Data is loaded", {
  initializeMazamaSpatialUtils()
  expect_true(exists("NaturalEarthAdm1"))
  })

