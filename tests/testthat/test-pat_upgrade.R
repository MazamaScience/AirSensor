context('test-pat_upgrade') 


test_that(
  'input validation works', 
  {
    skip_on_cran()
    skip_on_travis()
    
    initializeMazamaSpatialUtils()
    
    data('example_pat')
    expect_error(pat_upgrade(pat = NULL))
    expect_error(pat_upgrade(pat = example_pat, verbose = NULL))
  }
)

test_that(
  'invalid columns are removed', 
  {
    skip_on_cran()
    skip_on_travis()
    
    initializeMazamaSpatialUtils()
    
    data('example_pat')
    test_pat <- example_pat
    test_pat$data$testCol <- NA
    expect_equal(names(pat_upgrade(test_pat)), names(example_pat))
  }
)

test_that(
  'random missing columns are added', 
  {
    skip_on_cran()
    skip_on_travis()
    
    initializeMazamaSpatialUtils()
    
    data('example_pat')
    test_pat <- example_pat
    test_pat$data <- example_pat$data[,-sample(length(example_pat$data), 2)]
    expect_equal(names(pat_upgrade(test_pat)$data), names(example_pat$data))
  }
)