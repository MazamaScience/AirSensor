context('test-pas_createNew')

test_that(
  'input validation works', 
  {
    expect_error(pas_createNew(countryCodes = 1))
    expect_error(pas_createNew(lookbackDays = 0))
  }
)
