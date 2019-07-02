context("test-pat_qc")

test_that("consistent aggregation results", {
  data("example_pat")
  
  withBad <- pat_qc(example_pat_failure_B, removeOutOfSpec = FALSE)
  withoutBad <- pat_qc(example_pat_failure_B, removeOutOfSpec = TRUE) 
  
  # Should we compare these NA counts against precalculated constants or just 
  # against each other (more NAs when removeOutOfSpec is TRUE than FALSE)?
  withBadNaCount <- sum(is.na(withBad$data$pm25_A))
  withoutBadNaCount <- sum(is.na(withoutBad$data$pm25_A))
  
  expect_true(withoutBadNaCount > withBadNaCount)
})