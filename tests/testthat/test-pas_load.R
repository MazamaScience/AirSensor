context("test-pas_load")

test_that("baseDir input validation works", {
  expect_error(pas_load(baseDir = "/var/DUMMY"))
})

test_that("baseUrl input validation works", {
  expect_error(downloadParseSynopticData(baseUrl = "https://www.purpleair.com/DUMMY"))
  expect_error(pas_createNew(baseUrl = "https://www.purpleair.com/DUMMY"))
})

# test_that("loads correct class", {
#   skip_on_cran()
#   skip_on_travis()
#   setArchiveBaseUrl("https://airsensor.aqmd.gov/PurpleAir/v1")
#   expect_true(pas_isPas(pas_load()))
#   # expect_true(pas_isPas(pas_createNew()))
# })

test_that("bad datestamps are rejected", {
  futureStamp <- 
  { lubridate::now(tzone = "UTC") + lubridate::ddays(10) } %>%
    strftime("%Y%m%d", tz = "UTC")
  
  expect_error(pas_load(datestamp = "abc"))
  expect_error(pas_load(datestamp = 20190101))
  expect_error(pas_load(datestamp = "20190101"))
  expect_error(pas_load(datestamp = futureStamp))
})

test_that("advice is returned when archiveBaseDir is in use", {
  setArchiveBaseDir(tempdir())
  expect_error({pas <- pas_load()},
               ".*removeArchiveBaseDir().*")
  # Cleanup
  removeArchiveBaseDir()
})