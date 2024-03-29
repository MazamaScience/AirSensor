% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pas_addUniqueIDs.R
\name{pas_addUniqueIDs}
\alias{pas_addUniqueIDs}
\title{Add Unique Identifiers to PurpleAir Synoptic Data}
\usage{
pas_addUniqueIDs(pas = NULL)
}
\arguments{
\item{pas}{a pa_synoptic dataframe}
}
\value{
A dataframe with generated unique ID columns added.
}
\description{
Generates and adds a unique identification vector to PurpleAir
sensors using the \code{MazamaLocationUtils} package, which creates a unique 
ID based upon coordinate location and device id. 

Adds the following vectors: 
\itemize{
  \item{deviceID --PurpleAir ID}
  \item{locationID -- MazamaLocationUtils generated location ID }
  \item{deviceDeploymentID -- A combination of device and location IDs}
}
}
\note{
This function is only needed to add these IDs to archival \emph{pas}
generated with very early verions of the \pkg{AirSensor} package.
}
\seealso{
\link{pas_enhanceRawData}
}
