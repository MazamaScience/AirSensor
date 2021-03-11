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
#' example_pas <- pas_createNew(countryCodes = "US")
#' 
#' save(example_pas, file = "data/example_pas.rda")
#' }
#'
#' @seealso example_pas_raw
#' @source https://www.purpleair.com/json?all=true
"example_pas"

#' @encoding UTF-8
#' @title Example raw Purple Air Synoptic dataset
#' @format A tibble with 1233 rows and 32 columns of data.
#' @description The \code{example_pas_raw} dataset provides a quickly loadable
#' version of raw Purple Air synoptic data JSON for practicing and code
#' examples. This dataset contains data for sensors in Washington and Oregon
#' and was generated on 2020-09-15 by running:
#' 
#' \preformatted{
#' library(AirSensor)
#' 
#' initializeMazamaSpatialUtils()
#' 
#' example_pas_raw <- 
#'   pas_downloadParseRawData() %>%
#'   dplyr::filter(Lon > -125.0 & Lon < -117.0 & Lat > 42.0 & Lat < 49.0)
#'   
#' save(example_pas_raw, file = "data/example_pas_raw.rda")
#' }
#' 
#' This dataset can be converted into a standard \emph{pas} dataset with:
#' 
#' \preformatted{
#' pas <- pas_enhanceData(example_pas_raw)
#' }
#' 
#' @seealso example_pas
#' @source https://www.purpleair.com/json?all=true
"example_pas_raw"

#' @encoding UTF-8
#' @title Example PurpleAir Timeseries dataset
#' @format An S3 object composed of "meta" and "data" data.
#' @description The \code{example_pat} dataset provides a quickly loadable version of
#' a \emph{pa_timeseries} object for practicing and code examples.
#' This dataset was was generated on 2020-09-15 by running:
#'
#' \preformatted{
#' library(AirSensor)
#' 
#' initializeMazamaSpatialUtils()
#' 
#' example_pat <- pat_createNew(
#'   id = "ebcb53584e44bb6f_3218",
#'   pas = example_pas,
#'   startdate = "2018-08-01",
#'   enddate = "2018-08-28",
#'   verbose = TRUE
#' )
#' 
#' save(example_pat, file = "data/example_pat.rda")
#' }
#' @seealso example_pat_failure_A
#' @seealso example_pat_failure_B
"example_pat"

#' @encoding UTF-8
#' @title Example PurpleAir Timeseries dataset exhibiting moderate errors
#' @format An S3 object composed of "meta" and "data" data.
#' @description The \code{example_pat_failure_A} dataset provides a quickly loadable
#' version of a \emph{pa_timeseries} object for practicing and code examples.
#' This dataset was was generated on 2020-09-15 by running:
#'
#' \preformatted{
#' library(AirSensor)
#' 
#' initializeMazamaSpatialUtils()
#' 
#' example_pat_failure_A <- pat_createNew(
#'   label = "SCNP_20",
#'   pas = example_pas,
#'   startdate = "2019-04-01",
#'   enddate = "2019-04-18",
#'   verbose = "TRUE"
#' )
#' 
#' save(example_pat_failure_A, file = "data/example_pat_failure_A.rda")
#' }
#' @seealso example_pat
#' @seealso example_pat_failure_B
"example_pat_failure_A"

#' @encoding UTF-8
#' @title Example PurpleAir Timeseries dataset exhibiting severe errors
#' @format An S3 object composed of "meta" and "data" data.
#' @description The \code{example_pat_failure_B} dataset provides a quickly loadable
#' version of a \emph{pa_timeseries} object for practicing and code examples.
#' This dataset was was generated on 2020-09-15 by running:
#'
#' \preformatted{
#' library(AirSensor)
#' 
#' initializeMazamaSpatialUtils()
#' 
#' example_pat_failure_B <- pat_createNew(
#'   label = "SCTV_16",
#'   pas = example_pas,
#'   startdate = "2019-06-01",
#'   enddate = "2019-06-18",
#'   verbose = TRUE
#' )
#' 
#' save(example_pat_failure_B, file = "data/example_pat_failure_B.rda")
#' }
#' @seealso example_pat
#' @seealso example_pat_failure_A
"example_pat_failure_B"

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
