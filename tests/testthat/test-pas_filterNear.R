context("test-pas_filterNear")

test_that("filtering is consistent", {
  # San Fransisco
  pas_sanfran <- pas_filterNear(example_pas,
                                latitude = 37.774429, longitude = -122.419463,
                                radius = "10 km")
  expect_equal(nrow(pas_sanfran), 364)

  # Fresno
  pas_fresno <- pas_filterNear(example_pas,
                               latitude = 36.734764, longitude = -119.786450,
                               radius = "15 km")
  expect_equal(nrow(pas_fresno), 64)

  # Redding
  pas_redding <- pas_filterNear(example_pas,
                                latitude = 40.585569, longitude = -122.391938,
                                radius = "10 km")
  expect_equal(nrow(pas_redding), 38)
})
