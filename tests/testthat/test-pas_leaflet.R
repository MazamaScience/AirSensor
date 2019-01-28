context("test-pas_leaflet")

test_that("input validation works", {
  a <- 10
  expect_error(pas_leaflet(a))
  data(pas_Jan25)
  expect_error(pas_leaflet(pas, param="PM2.5"))
  expect_error(pas_leaflet(pas, outsideOnly = 'outside'))
  expect_error(pas_leaflet(pas, radius=NA))
})
