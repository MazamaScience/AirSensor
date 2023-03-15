context("test-pas_filterArea")

test_that("input validation works", {
  expect_error(pas_filterArea(NULL))
  expect_error(pas_filterArea(1:10))
})

test_that("pa_synoptic class returned", { 
  expect_true(pas_isPas(pas_filterArea(pas_filter(example_pas, stateCode == "CA"))))
})

test_that("bound order doesn't matter", { 
  areaOrder1 <- pas_filterArea(example_pas, 
                               w = -118.10, e = -118.07, 
                               s = 33.75, n = 33.78)
  lonRange1 <- range(areaOrder1$longitude)
  latRange1 <- range(areaOrder1$latitude)
  
  areaOrder2 <- pas_filterArea(example_pas, 
                               n = 33.78, e = -118.07, 
                               s = 33.75, w = -118.10)
  lonRange2 <- range(areaOrder2$longitude)
  latRange2 <- range(areaOrder2$latitude)
  
  expect_true(all(lonRange1 == lonRange2) && all(latRange1 == latRange2))
})

test_that("results_are_consistent", {
  # South Coast Seal Beach
  pas_scsb <-
    example_pas %>%
    pas_filterArea(
      w = -118.10,
      e = -118.07,
      s = 33.75,
      n = 33.78
    )
  expect_equal(nrow(pas_scsb), 20)
})