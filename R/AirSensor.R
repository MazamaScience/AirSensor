#' @docType package
#' @name AirSensor
#' @title Data access and analysis functions for PurpleAir sensor data
#' @description This package contains code to access current synoptic data from
#' Purple Air as well as time series data for individual sensors from Thing
#' Speak.
#'
#' Functions for downloading and enhancing sensor data return one of two types
#' of object:
#' \itemize{
#' \item{\code{pas} -- PurpleAirSynoptic dataframe of uniformly named properties}
#' \item{\code{pat} -- PurpleAirTimeseries list of dataframes containing
#' sensor metadata and data}
#' }
#'
#' Analysis and visualization functions provide basic functionality for working
#' with PurpleAir sensor data and comparing it with national monitoring data
#' retrieved with the \pkg{PWFSLSmoke} package.
NULL

# ----- Package Data -----------------------------------------------------------

# Relocated to R/data.R

# ----- Internal Package State -------------------------------------------------

airsensorEnv <- new.env(parent = emptyenv())
airsensorEnv$archiveBaseDir <- NULL
airsensorEnv$archiveBaseUrl <- NULL

#' @docType data
#' @keywords environment
#' @name ArchiveBaseDir
#' @title Base directory for pre-generated data
#' @format Directory string.
#' @description If an archive of pre-generated data files is available locally,
#' users can set the location of this directory with\code{setArchiveBaseDir()}.
#' Otherwise, users must specify an external source of pre-generated datafiles 
#' with \code{setArchiveBaseUrl()}.
#' 
#' To avoid internet latency, specification of BASE_DIR will always take 
#' precedence over specification of BASE_URL.
#' 
#' Package functions that load pre-generated data files will load data from this
#' directory. These functions include:
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
#' @seealso getArchiveBaseDir
#' @seealso setArchiveBaseDir
#' @seealso setArchiveBaseUrl
NULL

#' @keywords environment
#' @export
#' @title Get data archive base directory
#' @description Returns the package base directory pointing to an archive of
#' pre-generated data files.
#' @return directory string.
#' @seealso archiveBaseDir
#' @seealso setArchiveBaseDir
getArchiveBaseDir <- function() {
  # NULL is an acceptable value
  return(airsensorEnv$archiveBaseDir)    
}

#' @keywords environment
#' @export
#' @title Set data archive base directory
#' @param archiveBaseDir Base directory pointing to an archive of pre-generated 
#' data files.
#' @description Sets the package base directory pointing to an archive of
#' pre-generated data files.
#' 
#' @return Silently returns previous value of base directory.
#' @seealso ArchiveBaseDir
#' @seealso getArchiveBaseDir
setArchiveBaseDir <- function(archiveBaseDir) {
  old <- airsensorEnv$archiveBaseDir
  if ( is.null(archiveBaseDir) ) {
    airsensorEnv$archiveBaseDir <- archiveBaseDir
  } else if ( is.character(archiveBaseDir) ) {
    airsensorEnv$archiveBaseDir <- stringr::str_remove(archiveBaseDir, "/$")
  } else {
    stop("Parameter 'archiveBaseDir' must be either NULL or a directory path.")
  }
  return(invisible(old))
}

#' @keywords environment
#' @keywords internal
#' @export
#' @title Remove data archive base directory
#' @description Resets the data archive base directory to NULL. Used for internal 
#' testing. 
#' @return Silently returns previous value of the base directory.
#' @seealso ArchiveBaseDir
#' @seealso getArchiveBaseDir
#' @seealso setArchiveBaseDir
removeArchiveBaseDir <- function() {
  old <- airsensorEnv$archiveBaseDir
  airsensorEnv$archiveBaseDir <- NULL
  return(invisible(old))
}

#' @docType data
#' @keywords environment
#' @name ArchiveBaseUrl
#' @title Base URL for pre-generated data
#' @format URL string.
#' @description This package maintains an internal archive base URL which users 
#' can set using \code{setArchiveBaseUrl()}. Alternatively, if an archive of 
#' pre-generated data files is availalbe locally, users can set the location of 
#' this directory with\code{setArchiveBaseDir()}.
#' 
#' To avoid internet latency, specification of BASE_DIR will always take 
#' precedence over specification of BASE_URL.

#' Known base URLs include:
#' \itemize{
#' \item{http://data.mazamascience.com/PurpleAir/v1}
#' }
#' 
#' Package functions that load pre-generated data files download data from this
#' URL. These functions include:
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
#' @seealso setArchiveBaseDIR
NULL

#' @keywords environment
#' @export
#' @title Get data archive base URL
#' @description Returns the package base URL pointing to an archive of
#' pre-generated data files.
#' @return URL string.
#' @seealso archiveBaseUrl
#' @seealso setArchiveBaseUrl
getArchiveBaseUrl <- function() {
  # Check for archiveBaseDir
  if ( is.null(airsensorEnv$archiveBaseDir) &&
       is.null(airsensorEnv$archiveBaseUrl) ) {
    stop(
      'No BASE_URL set. Please set one with setArchiveBaseUrl("BASE_URL").',
      "\n\nKnown options include:\n\n",
      "  setArchiveBaseUrl(\"https://airsensor.aqmd.gov/PurpleAir/v1/\")   # SCAQMD sensors\n\n",
      "  setArchiveBaseUrl(\"http://data.mazamascience.com/PurpleAir/v1\") # SCAQMD sensors\n\n"
    )
  }
  return(airsensorEnv$archiveBaseUrl)    
  
}

#' @keywords environment
#' @export
#' @title Set data archive base URL
#' @param archiveBaseUrl Base URL pointing to an archive of pre-generated data files.
#' @description Sets the package base URL pointing to an archive of
#' pre-generated data files.
#' 
#' Known base URLs include:
#' \itemize{
#' \item{http://data.mazamascience.com/PurpleAir/v1} 
#  \item{https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1}
#' }
#' 
#' @return Silently returns previous value of base URL.
#' @seealso ArchiveBaseUrl
#' @seealso getArchiveBaseUrl
setArchiveBaseUrl <- function(archiveBaseUrl) {
  old <- airsensorEnv$archiveBaseUrl
  if ( is.null(archiveBaseUrl) ) {
    airsensorEnv$archiveBaseUrl <- archiveBaseUrl
  } else if ( is.character(archiveBaseUrl) ) {
    airsensorEnv$archiveBaseUrl <- stringr::str_remove(archiveBaseUrl, "/$")
  } else {
    stop("Parameter 'archiveBaseUrl' must be either NULL or a directory path.")
  }
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
  return(invisible(old))
}

