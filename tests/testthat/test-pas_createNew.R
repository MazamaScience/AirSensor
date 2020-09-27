context('test-pas_createNew')

test_that(
  'baseURL is valid', 
  {
    skip_on_cran()
    skip_on_travis()
    
    expect_false(httr::http_error(as.list(args(pas_createNew))$baseUrl))
  }
)

test_that(
  'input validation works', 
  {
    expect_error(pas_createNew(countryCodes = 1))
    expect_error(pas_createNew(lookbackDays = 0))
  }
)
