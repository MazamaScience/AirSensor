context('test-pat_scatterPlotMatrix')
data('example_pat', 'example_sensor')

test_that(
  'is ggplot', 
  expect_s3_class(
    pat_scatterPlotMatrix(example_pat),
    c('gg', 'ggplot')
  )
)
