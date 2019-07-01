context("test-pas_filterArea")

test_that("input validation works", {
  data("example_pas")
  
  expect_error(pas_filterArea(NULL))
  expect_error(pas_filterArea(1:10))
})

test_that("pa_synoptic class returned", { 
  expect_true(pas_isPas(pas_filterArea(pas_filter(example_pas, stateCode == "CA"))))
})

test_that("bound order doesn't matter", { 
  areaOrder1 <- pas_filterArea(example_pas, w = -118.10, e = -118.07, 
                               s = 33.75, n = 33.78)
  lonRange1 <- range(areaOrder1$longitude)
  latRange1 <-range(areaOrder1$latitude)
  
  areaOrder2 <- pas_filterArea(example_pas, n = 33.78, e = -118.07, 
                               s = 33.75, w = -118.10)
  lonRange2 <- range(areaOrder2$longitude)
  latRange2 <-range(areaOrder2$latitude)
  
  expect_true(lonRange1 == lonRange2 && latRange1 == latRange2)
})

test_that("results_are_consistent", {
  # San Fransisco
  pas_sanfran <- pas_filterArea(example_pas, 
                                w = -122.518155, e = -122.374895,
                                s = 37.739002, n = 37.810491)
  expect_equal(nrow(pas_sanfran), 195)
  
  # Fresno
  pas_fresno <- pas_filterArea(example_pas,
                               w = -119.894617, e = -119.659692,
                               s = 36.681235, n = 36.866996)
  expect_equal(nrow(pas_fresno), 64)
  
  # Redding
  pas_redding <- pas_filterArea(example_pas,
                                w = -122.430983, e = -122.300330,
                                s = 40.523458, n = 40.629298) 
  expect_equal(nrow(pas_redding), 28)
})