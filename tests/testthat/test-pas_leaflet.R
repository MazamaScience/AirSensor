context("test-pas_leaflet")

test_that("input validation works", {
  data("example_pas")
  expect_error(pas_leaflet(1:10))
  expect_error(pas_leaflet(example_pas, param="PM2.5"))
  expect_error(pas_leaflet(example_pas, outsideOnly = 'outside'))
  expect_error(pas_leaflet(example_pas, radius=NA))
})
test_that("leaflet class returned", { 
  expect_true(is(pas_leaflet(example_pas), "leaflet"))
  })
