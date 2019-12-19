context("test-pas_getColumn")

# Data used in all examples
data("example_pas")

test_that("input validation works", {
  expect_error(pas_getColumn(NULL))
  expect_error(pas_getColumn(1:10))
})

test_that("filtering works", { 
  # Many DEVICE_LOCATIONTYPE values are NA and don't show up as TRUE or FALSE
  expect_equal(length(pas_getColumn(example_pas, "ID", isOutside = TRUE)), 3337)
  expect_equal(length(pas_getColumn(example_pas, "ID", isOutside = FALSE)), 534)
  # State counts when example_pas was created
  expect_equal(length(pas_getColumn(example_pas, "ID", states = c("WA", "OR"))), 312)
  expect_equal(length(pas_getColumn(example_pas, "ID", states = c("CA"))), 1645)
})

test_that("proper column is extracted", { 
  # ID
  expect_equal(
    pas_getColumn(example_pas, "ID", states = "MT"),
    c("21317", "2342", "13975", "28423", "6726", "2851", "3471")
  )
  # Label
  expect_equal(
    pas_getColumn(example_pas, "label", states = "MT"),
    c("1719 s 10th st w",
      "Birch Grove, Kalispell, MT", 
      "Corvette Dr.",
      "GRIZZLY WAY",
      "G_SwanLake",
      "Helena 2-North",
      "Teakettle Mountain"
      )
  )
  # pm25
  expect_equal(
    pas_getColumn(example_pas, "pm25", states = "MT"),
    c(12.8, 8.54, 6.39, 7.59, 6.12, 6.29, 8.31)
  )
})

test_that("pas_getIDs() and pas_getLabels() work", { 
  # pas_getIDs()
  expect_equal(
    pas_getIDs(example_pas, states = "MT"),
    c("21317", "2342", "13975", "28423", "6726", "2851", "3471")
  )
  # pas_getLabels()
  expect_equal(
    pas_getLabels(example_pas, states = "MT"),
    c("1719 s 10th st w",
      "Birch Grove, Kalispell, MT", 
      "Corvette Dr.",
      "GRIZZLY WAY",
      "G_SwanLake",
      "Helena 2-North",
      "Teakettle Mountain"
    )
  )
})

