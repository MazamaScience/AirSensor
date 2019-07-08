#' @docType package
#' @name AirSensor
#' @title Data access and analysis functions for Purple Air sensor data
#' @description This package contains code to access current synoptic data from
#' Purple Air as well as time series data for individual sensors from Thing
#' Speak.
#'
#' Functions for downloading and enhancing sensor data return one of two types
#' of object:
#' \itemize{
#' \item{\code{pas} -- PurpleAirSynoptic dataframe of uniformly named properties}
#' \item{\code{pat} -- PurpleAirTimeseries lost of dataframes containg
#' sensor metadata and data}
#' }
#'
#' Analysis and visualization functions provide basic functionality for working
#' with Purple Air sensor data and comparing it with national monitoring data
#' retrieved with the \pkg{PWFSLSmoke} package.
NULL

#' @docType data
#' @keywords datasets
#' @name example_pas
#' @title Example Purple Air Synoptic dataset
#' @format A tibble with 7113 rows and 35 columns of data.
#' @description The \code{example_pas} dataset provides a quickly loadable
#' version of a \emph{pa_synoptic} object for practicing and code examples
#' This dataset was generatedon 2019-07-06 by running:
#' 
#' \preformatted{
#'   initializeMazamaSpatialUtils()
#'   example_pas <- pas_loadLatest()
#' }
#' 
#' @seealso example_pas_raw
#' @source https://www.purpleair.com/json
NULL

#' @docType data
#' @keywords datasets
#' @name example_pas_raw
#' @title Example raw Purple Air Synoptic dataset
#' @format A tibble with 12657 rows and 32 columns of data.
#' @description The \code{example_pas_raw} dataset provides a quickly loadable
#' version of raw Purple Air synoptic data JSON for practicing and code
#' examples This dataset was was generated on 2019-07-06 by running:
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
NULL

#' @docType data
#' @keywords datasets
#' @name example_pat
#' @title Example Purple Air Timeseries dataset
#' @format An S3 object composed of "meta" and "data" data.
#' @description The example_pat dataset provides a quickly loadable version of
#' a \emph{pa_timeseries} object for practicing and code examples.
#' This dataset was was generated on 2019-07-06 by running:
#' 
#' \preformatted{
#'   pas <- pas_load()
#'   example_pat <- pat_loadLatest(pas, name = "Seattle",
#'                                 startdate = "2018-07-01",
#'                                 enddate = "2018-09-01")
#' }
#' @seealso example_pat_failure_A
#' @seealso example_pat_failure_B
NULL

#' @docType data
#' @keywords datasets
#' @name example_pat_failure_A
#' @title Example Purple Air Timeseries dataset exhibiting moderate errors
#' @format An S3 object composed of "meta" and "data" data.
#' @description The example_pat_failure_A dataset provides a quickly loadable
#' version of a \emph{pa_timeseries} object for practicing and code examples. 
#' This dataset was was generated on 2019-07-06 by running:
#' 
#' \preformatted{
#'   pas <- pas_load()
#'   example_pat_failure_A <- 
#'     pat_loadLatest(pas, name = "SCNP_20",
#'                    startdate = "2019-04-01",
#'                    enddate = "2019-04-18")
#' }
#' @seealso example_pat
#' @seealso example_pat_failure_B
NULL

#' @docType data
#' @keywords datasets
#' @name example_pat_failure_B
#' @title Example Purple Air Timeseries dataset exhibiting severe errors
#' @format An S3 object composed of "meta" and "data" data.
#' @description The example_pat_failure_B dataset provides a quickly loadable
#' version of a \emph{pa_timeseries} object for practicing and code examples. 
#' This dataset was was generated on 2019-07-06 by running:
#' 
#' \preformatted{
#'   pas <- pas_load()
#'   example_pat_failure_B <- 
#'     pat_loadLatest(pas, name = "SCTV_16",
#'                    startdate = "2019-06-01",
#'                    enddate = "2019-06-18")
#' }
#' @seealso example_pat
#' @seealso example_pat_failure_A
NULL

#' @docType data
#' @keywords datasets
#' @name example_sensor
#' @title Example AirSensor Timeseries dataset
#' @format An S3 object composed of "meta" and "data" data.
#' @description The example_as dataset provides a quickly loadable version of
#' an \emph{airsensor} object for practicing and code examples.
#' This dataset was was generated on 2019-07-06 by running:
#' 
#' \preformatted{
#'   pas <- pas_load()
#'   example_sensor <- 
#'     pat_loadLatest(pas, name = "SCAN_14",
#'                    startdate = "2018-08-14",
#'                    enddate = "2018-09-07") %>%
#'     pat_createAirSensor(period = "1 hour")
#' }
NULL