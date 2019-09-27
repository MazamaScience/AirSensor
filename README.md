

# AirSensor R Package

`Utilities to process and display PM2.5 data from PurpleAir`


## Background

TODO

## Institutional Support

The initial development of this package was funded by the South Coast Air Quality Managment District with funds from an EPA STAR grant. The following disclaimer applies:

> This package was prepared as part of a project funded through a Science to Achieve Results (STAR) grant award (RD83618401) from the U.S. Environmental Protection Agency to the South Coast Air Quality Management District (South Coast AQMD). The opinions, findings, conclusions, and recommendations are those of the author and do not necessarily represent the views of the U.S. EPA or the South Coast AQMD, nor does mention of trade names or commercial products constitute endorsement or recommendation for use. The U.S. EPA, the South Coast AQMD, their officers, employees, contractors, and subcontractors make no warranty, expressed or implied, and assume no legal liability for the information in this package. The U.S. EPA and South Coast AQMD have not approved or disapproved this package, and neither have passed upon the accuracy or adequacy of the information contained herein.

Additional funding was provided by the US Forest Service in support of the Interagency Wildland Fire Air Quality Response Program.

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
devtools::install_github('MazamaScience/MazamaCoreUtils')
devtools::install_github('MazamaScience/AirSensor')
```

Any work with spatial data, *e.g.* assigning countries, states and timezones, will require installation of required
spatial datasets. To get these datasets you should type the following at the RStudio console:

```
library(MazamaSpatialUtils)
dir.create('~/Data/Spatial', recursive=TRUE)
setSpatialDataDir('~/Data/Spatial')
installSpatialData()
```





