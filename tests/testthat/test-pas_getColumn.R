context("test-pas_getColumn")

# Data used in all examples
data("example_pas")

test_that("input validation works", {
  expect_error(pas_getColumn(NULL))
  expect_error(pas_getColumn(1:10))
})

test_that("filtering works", { 
  # Many DEVICE_LOCATIONTYPE values are NA and don't show up as TRUE or FALSE
  expect_equal(length(pas_getColumn(example_pas, "ID", isOutside = TRUE)), 6176)
  expect_equal(length(pas_getColumn(example_pas, "ID", isOutside = FALSE)), 2070)
})

test_that("proper column is extracted", { 
  # ID
  expect_equal(
    example_pas %>%
      pas_filter(stateCode == "SD") %>%
      pas_getColumn("ID"),
    c("3097", "2506" , "8376", "49685", "5656")
  )
  # Label
  expect_equal(
    example_pas %>%
      pas_filter(stateCode == "SD") %>%
      pas_getColumn("label"),
    c(
      "Cat Ranch",
      "Golden Valley",
      "MandMnorth40",
      "Northern Lights",
      "West Rapid City"
    )
  )
  # pm25
  expect_equal(
    example_pas %>%
      pas_filter(stateCode == "SD") %>%
      pas_getColumn("pm25"),
    c(4.53, 30.97, 7.71, 22.85, 18.21)
  )
})

test_that("pas_getIDs() and pas_getLabels() work", { 
  # pas_getIDs()
  expect_equal(
    example_pas %>%
      pas_filter(stateCode == "SD") %>%
      pas_getIDs(),
    c("3097", "2506" , "8376", "49685", "5656")
  )
  # pas_getLabels()
  expect_equal(
    example_pas %>%
      pas_filter(stateCode == "SD") %>%
      pas_getLabels(),
    c(
      "Cat Ranch",
      "Golden Valley",
      "MandMnorth40",
      "Northern Lights",
      "West Rapid City"
    )
  )
})

