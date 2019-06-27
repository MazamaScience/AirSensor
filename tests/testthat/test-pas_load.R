context("loadData")

test_that("baseUrl Input validation works", {
  expect_error(downloadParseSynopticData(baseUrl = "https://www.purpleair.com/DUMMY"))
  expect_error(pas_load(baseUrl = "https://www.purpleair.com/DUMMY"))
  expect_error(pas_loadLatest(baseUrl = "https://www.purpleair.com/DUMMY"))
})

test_that("loads correct class", {
  expect_true(pas_isPas(pas_load()))
  expect_true(pas_isPas(pas_loadLatest()))
})
