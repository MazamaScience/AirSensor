# # NOTE:  The range of the example_pat is:
# #
# # > range(example_pat$data$datetime)
# # [1] "2022-07-01 00:04:43 UTC" "2022-07-07 23:49:33 UTC"
# 
# 
# test_that("character date formats work", {
# 
#   # IF start and end are specified as numeric or character:
#   #   assume the timezone of the sensor
# 
#   start_PDT <- ISOdatetime(2022, 07, 02, 07, 00, 00, tz = "UTC")
#   end_PDT <- ISOdatetime(2022, 07, 03, 06, 59, 00, tz = "UTC")
# 
#   expect_identical(
#     c(start_PDT, end_PDT),
#     range( pat_filterDate(example_pat, 20220702, 20220703)$data$datetime )
#   )
# 
#   expect_identical(
#     c(start_PDT, end_PDT),
#     range( pat_filterDate(example_pat, "20220702", "20220703")$data$datetime )
#   )
# 
#   expect_identical(
#     c(start_PDT, end_PDT),
#     range( pat_filterDate(example_pat, "2022-07-02", 20220703)$data$datetime )
#   )
# 
#   expect_identical(
#     c(start_PDT, end_PDT),
#     range( pat_filterDate(example_pat, "2022-07-02 00", 20220703)$data$datetime )
#   )
# 
#   expect_identical(
#     c(start_PDT, end_PDT),
#     range( pat_filterDate(example_pat, "2022-07-02 00:00:00", 20220703)$data$datetime )
#   )
# 
# })
# 
# test_that("character date formats work with timezones", {
# 
#   # IF start and end are specified as numeric or character
#   # AND a timezone is specified:
#   #   use the timezone to interpret the dates
# 
#   # UTC times
#   start_HST <- ISOdatetime(2022, 07, 02, 10, 00, 00, tz = "UTC")
#   end_HST <- ISOdatetime(2022, 07, 03, 09, 59, 00, tz = "UTC")
# 
#   start_PDT <- ISOdatetime(2022, 07, 02, 07, 00, 00, tz = "UTC")
#   end_PDT <- ISOdatetime(2022, 07, 03, 06, 59, 00, tz = "UTC")
# 
#   start_MDT <- ISOdatetime(2022, 07, 02, 06, 00, 00, tz = "UTC")
#   end_MDT <- ISOdatetime(2022, 07, 03, 05, 59, 00, tz = "UTC")
# 
#   start_EDT <- ISOdatetime(2022, 07, 02, 04, 00, 00, tz = "UTC")
#   end_EDT <- ISOdatetime(2022, 07, 03, 03, 59, 00, tz = "UTC")
# 
#   start_UTC <- ISOdatetime(2022, 07, 02, 00, 00, 00, tz = "UTC")
#   end_UTC <- ISOdatetime(2022, 07, 02, 23, 59, 00, tz = "UTC")
# 
#   expect_identical(
#     c(start_PDT, end_PDT),
#     range( pat_filterDate(example_pat, 20220702, 20220703)$data$datetime )
#   )
# 
#   expect_identical(
#     c(start_HST, end_HST),
#     range( pat_filterDate(example_pat, 20220702, 20220703, timezone = "US/Hawaii")$data$datetime )
#   )
# 
#   expect_identical(
#     c(start_PDT, end_PDT),
#     range( pat_filterDate(example_pat, 20220702, 20220703, timezone = "America/Los_Angeles")$data$datetime )
#   )
# 
#   expect_identical(
#     c(start_MDT, end_MDT),
#     range( pat_filterDate(example_pat, 20220702, 20220703, timezone = "America/Denver")$data$datetime )
#   )
# 
#   expect_identical(
#     c(start_EDT, end_EDT),
#     range( pat_filterDate(example_pat, 20220702, 20220703, timezone = "America/New_York")$data$datetime )
#   )
# 
#   expect_identical(
#     c(start_UTC, end_UTC),
#     range( pat_filterDate(example_pat, 20220702, 20220703, timezone = "UTC")$data$datetime )
#   )
# 
# })
# 
# test_that("POSIXct date formats work", {
# 
#   # Assume that anyone passing in a POSIXct object knows what they are doing.
# 
#   # IF start and end are specified as POSIXct
#   #   accept them at face value, ignoring the sensor timezone
# 
#   # UTC times
#   start_HST <- ISOdatetime(2022, 07, 02, 10, 00, 00, tz = "UTC")
#   end_HST <- ISOdatetime(2022, 07, 03, 09, 59, 00, tz = "UTC")
# 
#   start_PDT <- ISOdatetime(2022, 07, 02, 07, 00, 00, tz = "UTC")
#   end_PDT <- ISOdatetime(2022, 07, 03, 06, 59, 00, tz = "UTC")
# 
#   start_MDT <- ISOdatetime(2022, 07, 02, 06, 00, 00, tz = "UTC")
#   end_MDT <- ISOdatetime(2022, 07, 03, 05, 59, 00, tz = "UTC")
# 
#   start_EDT <- ISOdatetime(2022, 07, 02, 04, 00, 00, tz = "UTC")
#   end_EDT <- ISOdatetime(2022, 07, 03, 03, 59, 00, tz = "UTC")
# 
#   start_UTC <- ISOdatetime(2022, 07, 02, 00, 00, 00, tz = "UTC")
#   end_UTC <- ISOdatetime(2022, 07, 02, 23, 59, 00, tz = "UTC")
# 
#   # Local times
#   start_Hawaii <- ISOdatetime(2022, 07, 02, 00, 00, 00, tz = "US/Hawaii")
#   end_Hawaii <- ISOdatetime(2022, 07, 02, 23, 59, 00, tz = "US/Hawaii")
# 
#   start_LA <- ISOdatetime(2022, 07, 02, 00, 00, 00, tz = "America/Los_Angeles")
#   end_LA <- ISOdatetime(2022, 07, 02, 23, 59, 00, tz = "America/Los_Angeles")
# 
#   start_Denver <- ISOdatetime(2022, 07, 02, 00, 00, 00, tz = "America/Denver")
#   end_Denver <- ISOdatetime(2022, 07, 02, 23, 59, 00, tz = "America/Denver")
# 
#   start_NYC <- ISOdatetime(2022, 07, 02, 00, 00, 00, tz = "America/New_York")
#   end_NYC <- ISOdatetime(2022, 07, 02, 23, 59, 00, tz = "America/New_York")
# 
#   expect_identical(
#     c(start_HST, end_HST),
#     range( pat_filterDate(example_pat, start_Hawaii, end_Hawaii)$data$datetime )
#   )
# 
#   expect_identical(
#     c(start_PDT, end_PDT),
#     range( pat_filterDate(example_pat, start_LA, end_LA)$data$datetime )
#   )
# 
#   expect_identical(
#     c(start_MDT, end_MDT),
#     range( pat_filterDate(example_pat, start_Denver, end_Denver)$data$datetime )
#   )
# 
#   expect_identical(
#     c(start_EDT, end_EDT),
#     range( pat_filterDate(example_pat, start_NYC, end_NYC)$data$datetime )
#   )
# 
#   expect_identical(
#     c(start_UTC, end_UTC),
#     range( pat_filterDate(example_pat, start_UTC, end_UTC)$data$datetime )
#   )
# 
# })
# 
# test_that("POSIXct date formats work with timezones", {
# 
#   # Assume that anyone passing in a POSIXct object knows what they are doing.
# 
#   # IF start and end are specified as POSIXct
#   # AND a timezone is specified:
#   #   accept them at face value, ignoring the incoming timezone
# 
#   # UTC times
#   start_HST <- ISOdatetime(2022, 07, 02, 10, 00, 00, tz = "UTC")
#   end_HST <- ISOdatetime(2022, 07, 03, 09, 59, 00, tz = "UTC")
# 
#   start_PDT <- ISOdatetime(2022, 07, 02, 07, 00, 00, tz = "UTC")
#   end_PDT <- ISOdatetime(2022, 07, 03, 06, 59, 00, tz = "UTC")
# 
#   start_MDT <- ISOdatetime(2022, 07, 02, 06, 00, 00, tz = "UTC")
#   end_MDT <- ISOdatetime(2022, 07, 03, 05, 59, 00, tz = "UTC")
# 
#   start_EDT <- ISOdatetime(2022, 07, 02, 04, 00, 00, tz = "UTC")
#   end_EDT <- ISOdatetime(2022, 07, 03, 03, 59, 00, tz = "UTC")
# 
#   start_UTC <- ISOdatetime(2022, 07, 02, 00, 00, 00, tz = "UTC")
#   end_UTC <- ISOdatetime(2022, 07, 02, 23, 59, 00, tz = "UTC")
# 
#   # Local times
#   start_Hawaii <- ISOdatetime(2022, 07, 02, 00, 00, 00, tz = "US/Hawaii")
#   end_Hawaii <- ISOdatetime(2022, 07, 02, 23, 59, 00, tz = "US/Hawaii")
# 
#   start_LA <- ISOdatetime(2022, 07, 02, 00, 00, 00, tz = "America/Los_Angeles")
#   end_LA <- ISOdatetime(2022, 07, 02, 23, 59, 00, tz = "America/Los_Angeles")
# 
#   start_Denver <- ISOdatetime(2022, 07, 02, 00, 00, 00, tz = "America/Denver")
#   end_Denver <- ISOdatetime(2022, 07, 02, 23, 59, 00, tz = "America/Denver")
# 
#   start_NYC <- ISOdatetime(2022, 07, 02, 00, 00, 00, tz = "America/New_York")
#   end_NYC <- ISOdatetime(2022, 07, 02, 23, 59, 00, tz = "America/New_York")
# 
#   expect_identical(
#     c(start_HST, end_HST),
#     range( pat_filterDate(example_pat, start_Hawaii, end_Hawaii, timezone = "US/Hawaii")$data$datetime )
#   )
# 
#   expect_identical(
#     c(start_PDT, end_PDT),
#     range( pat_filterDate(example_pat, start_LA, end_LA, timezone = "America/Los_Angeles")$data$datetime )
#   )
# 
#   expect_identical(
#     c(start_MDT, end_MDT),
#     range( pat_filterDate(example_pat, start_Denver, end_Denver, timezone = "America/Denver")$data$datetime )
#   )
# 
#   expect_identical(
#     c(start_EDT, end_EDT),
#     range( pat_filterDate(example_pat, start_NYC, end_NYC, timezone = "America/New_York")$data$datetime )
#   )
# 
#   expect_identical(
#     c(start_UTC, end_UTC),
#     range( pat_filterDate(example_pat, start_UTC, end_UTC, timezone = "UTC")$data$datetime )
#   )
# 
# })
# 
# # # test_that("character formats w/o timezone get complete days in the sensor timezone", {
# # #   
# # #   # TODO:  Flesh this out
# # #   
# # # })
# # # 
# # # test_that("character formats w/ timezone get complete days in the specified timezone", {
# # #   
# # #   # TODO:  Flesh this out
# # #   
# # # })
# # # 
# # # test_that("POSIXct formats always get complete days in the POSIXct timezone", {
# # #   
# # #   # Assume that anyone passing in a POSIXct object knows what they are doing.
# # #   
# # #   # TODO:  Flesh this out
# # #   
# # # })
# 
