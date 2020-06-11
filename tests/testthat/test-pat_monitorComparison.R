context('test-pat_monitorComparison')
data('example_pat', 'example_sensor')

test_that(
  'Plot return classes are correct', 
  expect_s3_class(
    pat_monitorComparison(example_pat, distanceCutoff = 0),
    c('gg', 'ggplot')
  )
)
