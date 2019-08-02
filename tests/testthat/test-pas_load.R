context("test-pas_load")

test_that("baseUrl Input validation works", {
  expect_error(downloadParseSynopticData(baseUrl = "https://www.purpleair.com/DUMMY"))
  expect_error(pas_createNew(baseUrl = "https://www.purpleair.com/DUMMY"))
  expect_error(pas_load(baseUrl = "https://www.mazamascience.com/DUMMY"))
})

test_that("loads correct class", {
  skip_on_cran()
  skip_on_travis()
  expect_true(pas_isPas(pas_load()))
  # expect_true(pas_isPas(pas_createNew()))
})

# test_that("pwfsl loads correct class", {
#   skip_on_cran()
#   skip_on_travis()
#   expect_true(is(pwfsl_load(), "ws_monitor"))
#   expect_true(is(pwfsl_loadLatest(), "ws_monitor"))
# })
