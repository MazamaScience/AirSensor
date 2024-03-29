% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pas_getLabels.R
\name{pas_getLabels}
\alias{pas_getLabels}
\title{Return labels from filtered PurpleAir Synoptic objects}
\usage{
pas_getLabels(pas = NULL, pattern = ".*", idPattern = ".*", isOutside = TRUE)
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
The values associated with the \code{"label"} column are then returned.
}
\examples{
library(AirSensor)

pas <- example_pas

pas_getLabels(pas = pas) \%>\% head(10)

pas_getLabels(pas = pas, pattern = "back") \%>\% head(10)


}
\seealso{
\code{\link{pas_getColumn}},  \code{\link{pas_getIDs}},  \code{\link{pas_getDeviceDeploymentIDs}}
}
