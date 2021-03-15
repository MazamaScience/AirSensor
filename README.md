[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/AirSensor)](https://cran.r-project.org/package=AirSensor)
[![Downloads](http://cranlogs.r-pkg.org/badges/AirSensor)](https://cran.r-project.org/package=AirSensor)
[![Build Status](https://travis-ci.org/MazamaScience/AirSensor.svg?branch=master)](https://travis-ci.org/MazamaScience/AirSensor)

# AirSensor R Package

`Process and display PM2.5 data from PurpleAir`

## Background

The AirSensor R package is being developed to help air quality analysts, 
scientists and interested members of the public more easily work with air 
quality data from consumer-grade air quality sensors. Initial focus is on PM2.5 
measurements from sensors produced by [PurpleAir](https://www2.purpleair.com).

The package makes it easier to obtain data, perform analyses and create
visualizations. It includes functionality to:

* download and easily work with PM2.5 data from PurpleAir
* visualize raw "engineering-level" data from a PurpleAir sensor
* visualize data quality using built-in analytics and plots
* aggregate raw data onto an hourly axis
* create interactive maps and time series plots
* convert aggregated PurpleAir data into `ws_monitor` objects appropriate for
use with the **PWFSLSmoke** package

## Institutional Support

The initial development of this package was funded by the South Coast Air 
Quality Management District with funds from an EPA STAR grant. The following 
disclaimer applies:

> This package was prepared as part of a project funded through a Science to Achieve Results (STAR) grant award (RD83618401) from the U.S. Environmental Protection Agency to the South Coast Air Quality Management District (South Coast AQMD). The opinions, findings, conclusions, and recommendations are those of the author and do not necessarily represent the views of the U.S. EPA or the South Coast AQMD, nor does mention of trade names or commercial products constitute endorsement or recommendation for use. The U.S. EPA, the South Coast AQMD, their officers, employees, contractors, and subcontractors make no warranty, expressed or implied, and assume no legal liability for the information in this package. The U.S. EPA and South Coast AQMD have not approved or disapproved this package, and neither have passed upon the accuracy or adequacy of the information contained herein.

Additional funding was provided by the US Forest Service in support of the 
Interagency Wildland Fire Air Quality Response Program.

[Mazama Science](https://mazamascience.com) develops and maintains the package 
as part of its ongoing relationships with federal, state and local air quality 
agencies.

## Installation

This package is designed to be used with [R](https://cran.r-project.org) 
(>= 3.5) and [RStudio](https://rstudio.com/) so make sure you have those 
installed first.

The package is available on CRAN or you get the latest development version
from GitHub. To install the latest development version, users will want to 
install the **devtools** package and then type the following at the 
RStudio console:

```
# Note that vignettes require knitr and rmarkdown
install.packages('knitr')
install.packages('rmarkdown')
devtools::install_github("MazamaScience/AirSensor")
```

Any work with spatial data, *e.g.* assigning countries, states and timezones, 
will require installation of required spatial datasets. To get these datasets 
you should type the following at the RStudio console:

```
library(MazamaSpatialUtils)
dir.create('~/Data/Spatial', recursive=TRUE)
setSpatialDataDir('~/Data/Spatial')
installSpatialData("NaturalEarthAdm1")
installSpatialData("USCensusStates"")
installSpatialData("CA_AirBasins_01")
```





