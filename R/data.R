#' @title Example Purple Air Synoptic dataset
#' @format A tibble with 7113 rows and 35 columns of data.
#' @description The \code{example_pas} dataset provides a quickly loadable
#' version of a \emph{pa_synoptic} object for practicing and code examples.
#' This dataset was generatedon 2019-07-06 by running:
#' 
#' \preformatted{
#'   initializeMazamaSpatialUtils()
#'   example_pas <- pas_createNew()
#' }
#' 
#' @seealso example_pas_raw
#' @source https://www.purpleair.com/json
"example_pas"


#' @title Example raw Purple Air Synoptic dataset
#' @format A tibble with 12657 rows and 32 columns of data.
#' @description The \code{example_pas_raw} dataset provides a quickly loadable
#' version of raw Purple Air synoptic data JSON for practicing and code
#' examples. This dataset was was generated on 2019-07-06 by running:
#' 
#' \preformatted{
#'   example_pas_raw <- downloadParseSynopticData()
#' }
#' 
#' This dataset can be converted into a standard \emph{pas} dataset with:
#' 
#' \preformatted{
#'   pas <- enhanceSynopticData(example_pas_raw)
#' }
#' 
#' @seealso example_pas
#' @source https://www.purpleair.com/json
"example_pas_raw"


#' @title Example Purple Air Timeseries dataset
#' @format An S3 object composed of "meta" and "data" data.
#' @description The \code{example_pat} dataset provides a quickly loadable version of
#' a \emph{pa_timeseries} object for practicing and code examples.
#' This dataset was was generated on 2019-07-06 by running:
#' 
#' \preformatted{
#'   pas <- pas_load()
#'   example_pat <- pat_createNew(pas, label = "Seattle",
#'                                startdate = "2018-07-01",
#'                                enddate = "2018-09-01")
#' }
#' @seealso example_pat_failure_A
#' @seealso example_pat_failure_B
"example_pat"

 
#' @title Example Purple Air Timeseries dataset exhibiting moderate errors
#' @format An S3 object composed of "meta" and "data" data.
#' @description The example_pat_failure_A dataset provides a quickly loadable
#' version of a \emph{pa_timeseries} object for practicing and code examples. 
#' This dataset was was generated on 2019-07-06 by running:
#' 
#' \preformatted{
#'   pas <- pas_load()
#'   example_pat_failure_A <- 
#'     pat_createNew(pas, label = "SCNP_20",
#'                   startdate = "2019-04-01",
#'                   enddate = "2019-04-18")
#' }
#' @seealso example_pat
#' @seealso example_pat_failure_B
"example_pat_failure_A"


#' @title Example Purple Air Timeseries dataset exhibiting severe errors
#' @format An S3 object composed of "meta" and "data" data.
#' @description The \code{example_pat_failure_B} dataset provides a quickly loadable
#' version of a \emph{pa_timeseries} object for practicing and code examples. 
#' This dataset was was generated on 2019-07-06 by running:
#' 
#' \preformatted{
#'   pas <- pas_load()
#'   example_pat_failure_B <- 
#'     pat_createNew(pas, label = "SCTV_16",
#'                   startdate = "2019-06-01",
#'                   enddate = "2019-06-18")
#' }
#' @seealso example_pat
#' @seealso example_pat_failure_A
"example_pat_failure_B"


#' @title Example AirSensor Timeseries dataset
#' @format An S3 object composed of "meta" and "data" data.
#' @description The \code{example_sensor} dataset provides a quickly loadable version of
#' an \emph{airsensor} object for practicing and code examples.
#' This dataset was was generated on 2019-07-06 by running:
#' 
#' \preformatted{
#'   pas <- pas_load()
#'   example_sensor <- 
#'     pat_createNew(pas, label = "SCAN_14",
#'                   startdate = "2018-08-14",
#'                   enddate = "2018-09-07") %>%
#'     pat_createAirSensor(period = "1 hour")
#' }
"example_sensor"


#' @title Example AirSensor Timeseries dataset
#' @format An S3 object composed of "meta" and "data" data.
#' @description The \code{example_sensor_scaqmd} dataset provides a quickly 
#' loadable version of a multi-sensor \emph{airsensor} object for practicing and 
#' code examples. This dataset was was generated on 2019-07-10 by running:
#' 
#' \preformatted{
#'   example_sensor_scaqmd <- 
#'     sensor_load("scaqmd", startdate = 20190701, enddate = 20190708)
#' }
"example_sensor_scaqmd"
