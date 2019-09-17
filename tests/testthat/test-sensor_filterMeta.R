context("test-sensor_filterMeta")

test_that("meta and data still agree", {
  data("example_sensor_scaqmd")
  sensor <- example_sensor_scaqmd
  
  filtered <- 
    example_sensor_scaqmd %>%
    sensor_filterMeta(stringr::str_detect(monitorID, "^SCUV_"))
  
  # 'data' has columns 'datetime' and then meta$monitorID
  expect_true(all(filtered$meta$monitorID == names(filtered$data)[-1]))
})
