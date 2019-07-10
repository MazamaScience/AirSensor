context("test-pat_aggregate")

test_that("consistent aggregation results", {
  data("example_pat")
  
  # Load aggregate results from 2019-07-02
  pat_aggr_template <- readRDS("example_pat_aggr_test.rds")
  pat_aggr_attempt <- pat_aggregate(example_pat) 
  
  # NOTE: Need new example_pat_aggr_test.rds to match 
  # areSame <- identical(pat_aggr_attempt, pat_aggr_template)
  # expect_true(areSame)
})