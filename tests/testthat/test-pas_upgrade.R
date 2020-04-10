context('test-pas_upgrade') 

test_that(
  'input validation works', 
  {
    data('example_pas')
    expect_error(pas_upgrade(pas = NULL))
    expect_error(pas_upgrade(pas = example_pas, verbose = NULL))
  }
)

test_that(
  'invalid columns are removed', 
  {
    data('example_pas')
    test_pas <- example_pas
    test_pas$testCol <- NA
    expect_equal(names(pas_upgrade(test_pas)), names(example_pas))
  }
)

test_that(
  'random missing columns are added', 
  {
    data('example_pas')
    test_pas <- example_pas[,1:runif(1, 1, ncol(example_pas))]
    expect_equal(names(pas_upgrade(test_pas)), names(example_pas))
  }
)