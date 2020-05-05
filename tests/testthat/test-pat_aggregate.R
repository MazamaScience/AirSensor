context("test-pat_aggregate")

test_that('Alternative aggregation', {
  data("example_pat")
  # Test aggregation settings to test default settings
  expect_true(identical(
    pat_aggregate(example_pat, 
                  FUN = function(x) mean(x,na.rm = TRUE)), 
    pat_aggregate(example_pat, 
                  FUN = function(x) mean(x, na.rm = TRUE), 
                  unit = 'minutes', 
                  count = 60)
  ))
  
})
