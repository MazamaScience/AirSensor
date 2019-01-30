context("test-pwfsl_load")

test_that("object is of class 'ws_monitor'", {
  skip_on_cran()
  expect_is(pwfsl_load(), "ws_monitor")
  expect_false(PWFSLSmoke::monitor_isEmpty(pwfsl_load()))
})
