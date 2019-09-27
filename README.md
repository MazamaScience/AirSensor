[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/AirSensor)](https://cran.r-project.org/package=AirSensor)
[![Downloads](http://cranlogs.r-pkg.org/badges/AirSensor)](https://cran.r-project.org/package=AirSensor)
[![Build Status](https://travis-ci.org/MazamaScience/AirSensor.svg?branch=master)](https://travis-ci.org/MazamaScience/AirSensor)

# AirSensor R Package

`Utilities to process and display PM2.5 data from PurpleAir`

## Background


## Installation

This package is designed to be used with [R](https://cran.r-project.org) (>= 3.3) and
[RStudio](https://www.rstudio.com) so make sure you have those installed first.

Users will want to install the **devtools** package to have access to the latest version
of the package from Github.

The following packages should be installed by typing the following at the RStudio console:

```
# Note that vignettes require knitr and rmarkdown
install.packages('knitr')
install.packages('rmarkdown')
install.packages('MazamaSpatialUtils')
devtools::install_github('MazamaScience/AirSensor', build_vignettes=TRUE)
```

Any work with spatial data, *e.g.* assigning countries, states and timezones, will require installation of required
spatial datasets. To get these datasets you should type the following at the RStudio console:

```
library(MazamaSpatialUtils)
dir.create('~/Data/Spatial', recursive=TRUE)
setSpatialDataDir('~/Data/Spatial')
installSpatialData()
```





