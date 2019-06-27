context("test-pas_filterArea")

test_that("input validation works", {
  data("example_pas")
  
  expect_error(pas_filterArea(NULL))
  expect_error(pas_filterArea(1:10))
  #expect_error(pas_filterArea(example_pas, w = 1:10))
  
  expect_true(!pas_isEmpty(pas_filterArea(example_pas)))
})

test_that("pa_synoptic class returned", { 
  expect_true(pas_isPas(pas_filterArea(pas_filter(example_pas, stateCode == "CA"))))
})

test_that("bound order doesn't matter", { 
  areaOrder1 <- pas_filterArea(example_pas, w = -118.10, e = -118.07, s = 33.75, n = 33.78)
  lonRange1 <- range(areaOrder1$longitude)
  latRange1 <-range(areaOrder1$latitude)
  
  areaOrder2 <- pas_filterArea(example_pas, n = 33.78, e = -118.07, s = 33.75, w = -118.10)
  lonRange2 <- range(areaOrder2$longitude)
  latRange2 <-range(areaOrder2$latitude)
  
  expect_true(lonRange1 == lonRange2 && latRange1 == latRange2)
})
