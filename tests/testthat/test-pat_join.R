# context("test-pat_join")
# 
# test_that("join handles both a list and discrete objects", {
#   pat <- get(data("example_pat"))
#   
#   jul01_15 <-
#     pat %>%
#     pat_filterDate(20180701, 20180715)
# 
#   aug09_22 <-
#     pat %>%
#     pat_filterDate(20180809, 20180822)
# 
#   expect_error( { x <- pat_join(jul01_15, aug09_22) }, NA )
#   expect_error( { x <- pat_join(list(jul01_15, aug09_22)) }, NA )
#   
# })