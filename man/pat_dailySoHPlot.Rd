% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pat_dailySoHPlot.R
\name{pat_dailySoHPlot}
\alias{pat_dailySoHPlot}
\title{Daily State of Health metric plot}
\usage{
pat_dailySoHPlot(pat = NULL, ncol = 2)
}
\arguments{
\item{pat}{PurpleAir Timeseries \emph{pat} object.}

\item{ncol}{Number of columns in the faceted plot.}
}
\description{
This function plots a subset of the most useful State of Health 
metrics calculated by the \code{pat_dailySoH} function. The function 
runs \code{pat_dailySoH} internally and uses the output to create 
the plot.
}
\examples{
\donttest{
library(AirSensor)

pat_dailySoHPlot(example_pat)
}
}
\seealso{
\link{pat_dailySoH}
}
