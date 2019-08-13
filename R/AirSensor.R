# ----- Package Data -----------------------------------------------------------

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
#'   example_pas <- pas_createNew()
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
#'     pat_createNew(pas, label = "SCNP_20",
#'                   startdate = "2019-04-01",
#'                   enddate = "2019-04-18")
#' }
#' @seealso example_pat
#' @seealso example_pat_failure_B
NULL

#' @docType data
#' @keywords datasets
#' @name example_pat_failure_B
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
NULL

#' @docType data
#' @keywords datasets
#' @name example_sensor
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
NULL

#' @docType data
#' @keywords datasets
#' @name example_sensor_scaqmd
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
NULL

# ----- Internal Package State -------------------------------------------------

airsensorEnv <- new.env(parent = emptyenv())
airsensorEnv$archiveBaseUrl <- NULL

#' @docType data
#' @keywords environment
#' @name ArchiveBaseUrl
#' @title Base URL for pre-generated data
#' @format URL string.
#' @description This package maintains an internal URL which users can set
#' using \code{setArchiveBaseUrl()}. Package functions that load pre-generated
#' data files use this. These functions include:
#' 
#' \itemize{
#' \item{\code{pas_load()}}
#' \item{\code{pat_load()}}
#' \item{\code{pat_loadLatest()}}
#' \item{\code{pat_loadMonth()}}
#' \item{\code{sensor_load()}}
#' \item{\code{sensor_loadLatest()}}
#' \item{\code{sensor_loadMonth()}}
#' }
#' 
#' @seealso getArchiveBaseUrl
#' @seealso setArchiveBaseUrl
NULL

#' @keywords environment
#' @export
#' @import sp
#' @title Get data archive base URL
#' @description Returns the package base URL pointing to an archive of
#' pre-generated data files.
#' @return URL string.
#' @seealso archiveBaseUrl
#' @seealso setArchiveBaseUrl
getArchiveBaseUrl <- function() {
  if (is.null(airsensorEnv$archiveBaseUrl)) {
    stop('No URL. Please set a base URL with setArchiveBaseUrl("BASE_URL").', call.=FALSE)
  } else {
    return(airsensorEnv$archiveBaseUrl)    
  }
}

#' @keywords environment
#' @export
#' @title Set data archive base URL
#' @param archiveBaseUrl Base URL pointing to an archive of pre-generated data files.
#' @description Sets the package base URL pointing to an archive of
#' pre-generated data files.
#' @return Silently returns previous value of base URL.
#' @seealso ArchiveBaseUrl
#' @seealso getArchiveBaseUrl
setArchiveBaseUrl <- function(archiveBaseUrl) {
  old <- airsensorEnv$archiveBaseUrl
  airsensorEnv$archiveBaseUrl <- stringr::str_remove(archiveBaseUrl, "/$")
  return(invisible(old))
}

#' @keywords environment
#' @keywords internal
#' @export
#' @title Remove data archive base URL
#' @description Resets the data archive base URL to NULL. Used for internal 
#' testing. 
#' @return Silently returns previous value of the base URL.
#' @seealso ArchiveBaseUrl
#' @seealso getArchiveBaseUrl
#' @seealso setArchiveBaseUrl
removeArchiveBaseUrl <- function() {
  old <- airsensorEnv$archiveBaseUrl
  airsensorEnv$archiveBaseUrl <- NULL
}

