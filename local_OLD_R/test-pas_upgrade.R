context('test-pas_upgrade') 

test_that(
  'input validation works', 
  {
    skip_on_cran()
    skip_on_travis()
    
    initializeMazamaSpatialUtils()
    
    data('example_pas')
    expect_error(pas_upgrade(pas = NULL))
    expect_error(pas_upgrade(pas = example_pas, verbose = NULL))
  }
)

test_that(
  'invalid columns are removed', 
  {
    skip_on_cran()
    skip_on_travis()
    
    initializeMazamaSpatialUtils()
    
    data('example_pas')
    test_pas <- example_pas
    test_pas$testCol <- NA
    expect_equal(names(pas_upgrade(test_pas)), names(example_pas))
  }
)

test_that(
  'random missing columns are added', 
  {
    skip_on_cran()
    skip_on_travis()
    
    initializeMazamaSpatialUtils()
    
    data('example_pas')
    test_pas <- example_pas[,-c(11,13,17,21,23)]
    expect_equal(names(pas_upgrade(test_pas)), names(example_pas))
  }
)