context('test-pas_upgrade') 

initializeMazamaSpatialUtils()

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
    test_pas <- example_pas[,-c(11,13,17,21,23)]
    expect_equal(names(pas_upgrade(test_pas)), names(example_pas))
  }
)