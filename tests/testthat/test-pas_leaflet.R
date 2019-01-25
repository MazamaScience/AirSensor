context("test-pas_leaflet")

# skip on cran - requires a pa_synoptic object
test_that("input validation works", {
  a <- 10
  expect_error(pas_leaflet(a))
})
