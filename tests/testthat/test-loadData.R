context("loadData")

test_that("baseUrl Input validation works", {
  expect_error(downloadParseSynopticData(baseUrl = "https://www.purpleair.com/DUMMY"))
  expect_error(pas_load(baseUrl = "https://www.purpleair.com/DUMMY"))
})

test_that("pwfsl loads correct class", {
  expect_true(is(pwfsl_load(), "ws_monitor"))
  expect_true(is(pwfsl_loadLatest(), "ws_monitor"))
})
