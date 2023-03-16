#' @encoding UTF-8
#' @title Example Purple Air Synoptic dataset
#' @format A tibble with 16584 rows and 44 columns of data.
#' @description The \code{example_pas} dataset provides a quickly loadable
#' version of a \emph{pa_synoptic} object for practicing and code examples.
#' This dataset was generated on 2020-09-15 by running:
#'
#' \preformatted{
#' library(AirSensor)
#' 
#' initializeMazamaSpatialUtils()
#' 
#' example_pas <- 
#'   pas_createNew(
#'     api_key = MY_API_READ_KEY,
#'     countryCodes = "US",
#'     stateCodes = "CA",
#'     show_only = SCAQMD_SENSOR_INDICES,
#'     lookbackDays = 1,
#'     location_type = 0
#'   )
#' 
#' save(example_pas, file = "data/example_pas.rda")
#' }
"example_pas"

#' @encoding UTF-8
#' @title Example PurpleAir Timeseries dataset
#' @format An S3 object composed of "meta" and "data" data.
#' @description The \code{example_pat} dataset provides a quickly loadable version of
#' a \emph{pa_timeseries} object for practicing and code examples.
#' This dataset was was generated on 2023-03-15 by running:
#'
#' \preformatted{
#' library(AirSensor)
#' 
#' initializeMazamaSpatialUtils()
#' 
#' example_pat <-
#'   pat_createNew(
#'     api_key = MY_API_READ_KEY,
#'     pas = example_pas,
#'     sensor_index = "3515",
#'     startdate = "2022-07-01",
#'     enddate = "2022-07-08",
#'     timezone = "UTC",
#'     verbose = TRUE
#'   )
#' 
#' save(example_pat, file = "data/example_pat.rda")
#' }
"example_pat"

#' @encoding UTF-8
#' @title Example AirSensor Timeseries dataset
#' @format An S3 object composed of "meta" and "data" data.
#' @description The \code{example_sensor} dataset provides a quickly loadable version of
#' an \emph{airsensor} object for practicing and code examples.
#' This dataset was was generated on 2020-09-15 by running:
#'
#' \preformatted{
#' library(AirSensor)
#' 
#' initializeMazamaSpatialUtils()
#' 
#' example_sensor <- pat_createNew(
#'   label = "SCAN_14",
#'   pas = example_pas,
#'   startdate = "2018-08-14",
#'   enddate = "2018-09-07"
#' ) %>%
#' pat_createAirSensor(parameter = 'pm25', FUN = AirSensor::PurpleAirQC_hourly_AB_01)
#' 
#' save(example_sensor, file = "data/example_sensor.rda")
#' }
"example_sensor"

#' @encoding UTF-8
#' @title Example AirSensor Timeseries dataset
#' @format An S3 object composed of "meta" and "data" data.
#' @description The \code{example_sensor_scaqmd} dataset provides a quickly
#' loadable version of a multi-sensor \emph{airsensor} object for practicing and
#' code examples. This dataset was was generated on 2020-09-15 by running:
#'
#' \preformatted{
#' library(AirSensor)
#' 
#' setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
#' 
#' example_sensor_scaqmd <-
#'   sensor_load("scaqmd", startdate = 20190701, enddate = 20190708)
#' 
#' save(example_sensor_scaqmd, file = "data/example_sensor_scaqmd.rda")
#' }
"example_sensor_scaqmd"
