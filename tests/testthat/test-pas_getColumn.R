context("test-pas_getColumn")

# Data used in all examples
data("example_pas")

test_that("input validation works", {
  expect_error(pas_getColumn(NULL))
  expect_error(pas_getColumn(1:10))
})

test_that("filtering works", { 
  # Many DEVICE_LOCATIONTYPE values are NA and don't show up as TRUE or FALSE
  expect_equal(length(pas_getColumn(example_pas, "ID", isOutside = TRUE)), 4159)
  expect_equal(length(pas_getColumn(example_pas, "ID", isOutside = FALSE)), 901)
})

test_that("proper column is extracted", { 
  # ID
  expect_equal(
    example_pas %>%
      pas_filter(stateCode == "SD") %>%
      pas_getColumn("ID"),
    c("3097", "26059", "2506" , "8376", "5656")
  )
  # Label
  expect_equal(
    example_pas %>%
      pas_filter(stateCode == "SD") %>%
      pas_getColumn("label"),
    c(
      "Cat Ranch",
      "Garfield School",
      "Golden Valley",
      "MandMnorth40",
      "West Rapid City"
    )
  )
  # pm25
  expect_equal(
    example_pas %>%
      pas_filter(stateCode == "SD") %>%
      pas_getColumn("pm25"),
    c(0.04, 1.30, 0.00, 8.59, 1.99)
  )
})

test_that("pas_getIDs() and pas_getLabels() work", { 
  # pas_getIDs()
  expect_equal(
    example_pas %>%
      pas_filter(stateCode == "SD") %>%
      pas_getIDs(),
    c("3097", "26059", "2506" , "8376", "5656")
  )
  # pas_getLabels()
  expect_equal(
    example_pas %>%
      pas_filter(stateCode == "SD") %>%
      pas_getLabels(),
    c(
      "Cat Ranch",
      "Garfield School",
      "Golden Valley",
      "MandMnorth40",
      "West Rapid City"
    )
  )
})

