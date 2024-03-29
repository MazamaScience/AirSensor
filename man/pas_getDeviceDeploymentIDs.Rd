% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pas_getDeviceDeploymentIDs.R
\name{pas_getDeviceDeploymentIDs}
\alias{pas_getDeviceDeploymentIDs}
\title{Return timeseries identifiers from filtered PurpleAir Synoptic objects}
\usage{
pas_getDeviceDeploymentIDs(
  pas = NULL,
  pattern = ".*",
  idPattern = ".*",
  isOutside = TRUE
)
}
\arguments{
\item{pas}{PurpleAir Synoptic \emph{pas} object.}

\item{pattern}{Text pattern used to filter station labels.}

\item{idPattern}{Text pattern used to filter \code{deviceDeploymentID}.}

\item{isOutside}{Logical, is the sensor located outside?}
}
\value{
Vector of values.
}
\description{
The incoming \code{pas} object is first filtered based on the 
values of \code{stateCodes}, \code{pattern} and \code{isOutside}.
The values associated with the \code{"deviceDeploymentID"} column are then 
returned.

This function is useful for returning a vector of unique time series
identifiers. These are used in the names of pre-generated \emph{pat} files
found in data archives.
}
\seealso{
\code{\link{pas_getColumn}},  \code{\link{pas_getLabels}}
}
