context("test-pas_leaflet")

test_that("input validation works", {
  a <- 10
  expect_error(pas_leaflet(a))
  data(pas_load_sample)
  expect_error(pas_leaflet(pas_load_sample, param="PM2.5"))
  expect_error(pas_leaflet(pas_load_sample, outsideOnly = 'outside'))
  expect_error(pas_leaflet(pas_load_sample, radius=NA))
})
