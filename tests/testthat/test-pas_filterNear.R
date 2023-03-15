context("test-pas_filterNear")

test_that("filtering is consistent", {
  pas_diamond_bar <-
    example_pas %>%
    pas_filterNear(
      longitude = -117.820833,
      latitude = 34.001667,
      radius = "20 km"
    )
  expect_equal(nrow(pas_diamond_bar), 5)
})
