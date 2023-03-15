context("test-pas_getColumn")

# Data used in all examples
data("example_pas")

test_that("input validation works", {
  expect_error(pas_getColumn(NULL))
  expect_error(pas_getColumn(1:10))
})

test_that("filtering works", { 
  # Many DEVICE_LOCATIONTYPE values are NA and don't show up as TRUE or FALSE
  expect_equal(length(pas_getColumn(example_pas, "ID", isOutside = TRUE)), 71)
  expect_equal(length(pas_getColumn(example_pas, "ID", isOutside = FALSE)), 0)
})

test_that("proper column is extracted", { 
  # ID
  expect_equal(
    example_pas %>%
      pas_filter(communityRegion == "El Monte") %>%
      pas_getColumn("ID"),
    c("2452", "2496", "2504", "2693", "2713")
  )
  # pm25
  expect_equal(
    example_pas %>%
      pas_filter(communityRegion == "El Monte") %>%
      pas_getColumn("pm25"),
    c(2.1, 1.2, 2.4, 3.8, 2.1)
  )
})

test_that("pas_getIDs() and pas_getLabels() work", { 
  # pas_getIDs()
  expect_equal(
    example_pas %>%
      pas_filter(communityRegion == "El Monte") %>%
      pas_getIDs(),
    c("2452", "2496", "2504", "2693", "2713")
  )
  # pas_getLabels()
  expect_equal(
    example_pas %>%
      pas_filter(communityRegion == "El Monte") %>%
      pas_getLabels(),
    c("SCEM_05", "SCEM_07", "SCEM_03", "SCEM_04", "scem_06")
  )
})

