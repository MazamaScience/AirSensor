% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\encoding{UTF-8}
\name{example_pas}
\alias{example_pas}
\title{Example Purple Air Synoptic dataset}
\format{
A tibble with 71 rows and 59 columns of data.
}
\usage{
example_pas
}
\description{
The \code{example_pas} dataset provides a quickly loadable
version of a \emph{pa_synoptic} object for practicing and code examples.
This dataset was generated on 2023-03-15 by running:

\preformatted{
library(AirSensor)

initializeMazamaSpatialUtils()

example_pas <- 
  pas_createNew(
    api_key = PURPLE_AIR_API_READ_KEY,
    countryCodes = "US",
    stateCodes = "CA",
    show_only = SCAQMD_SENSOR_INDICES,
    lookbackDays = 1,
    location_type = 0
  )

save(example_pas, file = "data/example_pas.rda")
}
}
\keyword{datasets}
