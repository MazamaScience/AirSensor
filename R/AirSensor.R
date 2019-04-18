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
#' @description The sample_pas dataset provides a quickly loadable version of
#' a \emph{pa_synoptic} object for practicing and code development. This
#' dataset was generated by running \code{pas_load()} on 2019-03-14.
#' @seealso example_raw_pas
#' @source https://www.purpleair.com/json
NULL

#' @docType data
#' @keywords datasets
#' @name example_raw_pas
#' @title Example raw Purple Air Synoptic dataset
#' @format A tibble with 12657 rows and 32 columns of data.
#' @description The sample_pas dataset provides a quickly loadable version of
#' raw Purple Air syoptic data for practicing and code development. This
#' was generated by running \code{pas_load()} on 2019-03-14.
#' 
#' This dataset can be converted into a standard \emph{pas} dataset with:
#' 
#' \code{pas <- enhanceSynopticData(example_raw_pas)}
#' @seealso example_pas
#' @source https://www.purpleair.com/json
NULL

#' @docType data
#' @keywords datasets
#' @name example_pat
#' @title Example Purple Air Timeseries dataset
#' @format An S3 object composed of "meta" and "data" data.
#' @description The sample_pas dataset provides a quickly loadable version of
#' a \emph{pa_synoptic} object for practicing and code development. The dataset 
#' was generated by running \code{pat_load()} on 2019-03-22 focused on a monitor
#' in central Seattle over a two-month interval, 2018-07-01 to 2018-09-01.
#' @seealso createPATimeseriesObject
#' @source https://www.purpleair.com/json
NULL