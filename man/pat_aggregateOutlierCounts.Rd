% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pat_aggregateOutlierCounts.R
\name{pat_aggregateOutlierCounts}
\alias{pat_aggregateOutlierCounts}
\title{Aggregate data with count of outliers in each bin}
\usage{
pat_aggregateOutlierCounts(
  pat = NULL,
  unit = "minutes",
  count = 60,
  windowSize = 23,
  thresholdMin = 8
)
}
\arguments{
\item{pat}{PurpleAir Timeseries \emph{pat} object.}

\item{unit}{Character string specifying temporal units for binning.}

\item{count}{Number of units per bin.}

\item{windowSize}{the size of the rolling window. Must satisfy windowSize <= count.}

\item{thresholdMin}{the minimum threshold value to detect outliers via hampel filter}
}
\value{
\code{data.frame} A data.frame with flag counts per bin.
}
\description{
Aggregate data with count of outliers in each bin
}
\examples{
\donttest{
library(AirSensor)
library(ggplot2)

df <- 
  pat_aggregateOutlierCounts(example_pat_failure_A)

# Plot the counts 
multi_ggplot(
  # A Channel
  ggplot(df, aes(x = datetime, y = pm25_A_outlierCount)) + geom_point(),
  # B Channel
  ggplot(df, aes(x = datetime, y = pm25_B_outlierCount)) + geom_point(),
  # Humidity
  ggplot(df, aes(x = datetime, y = humidity_outlierCount)) + geom_point(),
  # Temperature 
  ggplot(df, aes(x = datetime, y = temperature_outlierCount)) + geom_point()
)

}

}
\seealso{
pat_aggregateData
}
