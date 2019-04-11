# Installation on Windows

**_Updated 2019-03-27_**

Tested on an ASUS laptop running Windows 10.0.

## Requirements

### Development version

The following list of software must be installed in order to begin using the 
development version of the **AirSensor** R package.

* git
* git Fork (recommended git client)
* R >= 3.5.3
* RTools (for package compilation)
* RStudio >= 1.1.456
* R package **MazamaSpatialUtils** and associated data
* R package **PWFSLSmoke**
* R package **testthat**
* R package **devtools**

### Released version

The following list of software must be installed in order to begin using the 
publicly released **AirSensor** R package.

* R >= 3.5.3
* RStudio >= 1.1.456
* R package **MazamaSpatialUtils** and associated data
* R package **PWFSLSmoke**
* R package **AirSensor**

## Software Installation Instructions

### git

Git is used to interact with the **AirSensor** code repository on Git Hub.

https://git-scm.com

Download and install using the installer (*currently v2.21.0*)

* Information
* Select Components -- use defaults
* Choose default editor -- choose your favorite
* Adjust your PATH -- "Use Git from Git Bash only"
* HTTPS transport -- "Use the Open SSL library"
* Line ending conversions -- "Checkout Windows-style, commit Unix-style"
* Terminal emulator -- "Use MinTTY"
* Extra options -- use defaults

### Fork

Fork is an easy-to-use client for git. Other possible clients are listed at 
https://git-scm.com/downloads/guis.

https://git-fork.com/windows

Download and install using the installer.

#### User information

* User Name -- this is the name that will be attached to git check-ins
* Email -- this is the email that will be attached to git check-ins
* Default source folder -- we recommend `C:\Users\<YOUR NAME>\Projects`

Fork will take a few moments to initialize. Leave this window open for later
use.

### R

Statistical programming language.

https://cran.r-project.org/bin/windows/base

Download R 3.5.3 for Windows

* Information
* Location -- use defaults
* Components -- use defaults
* Startup options -- "No (accept defaults")
* Additional Tasks -- add shortcuts as desired

### RTools

These compilers are needed if you will be downloading packages from Git Hub
or otherwise building them from source.

https://cran.rstudio.com/bin/windows/RTools

Install the recommended RTools (*currently RTools35*)

* Information
* Destination -- use defaults
* Components -- use defaults
* Additional Tasks -- use defaults

### RStudio

GUI for R. The **AirSensor** package is designed to be run in RStudio.

https://www.rstudio.com/products/rstudio/download

Install RStudio Desktop -- the FREE one

* Install Location -- use defaults
* Start Menu -- use defaults

## R Package Installation Instructions

To install R packages you need to start up RStudio.

### MazamaSpatialUtils

Data and functions for geospatial analysis.

At the R console prompt type:

```
install.packages("MazamaSpatialUtils")
```

After the package has been installed we need to tell it where to keep spatial
data and then install the default spatial datasets. (Note that '~' in the code 
below is interpreted as `C:\Users\<YOUR NAME>\/Documents`.)

```
library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")
installSpatialData()
```

This will download 168 MB of spatial data that is used to enhance monitor and
sensor metadata with spatial fields like `timezone` or `stateCode`.

### PWFSLSmoke

Core functionality for working with AirNow, AIRSIS and WRCC monitoring data.

At the R console prompt type:

```
install.packages("PWFSLSmoke")
```

This will install **PWFSLSmoke** 1.1.3 but we need to get the development version
from GitHub. To do that we will need to install the **devtools** package

At the R console prompt type:

```
install.packages("devtools")
...
devtools::install_github("MazamaScience/PWFSLSmoke")
```

If you look in the lower right pane and click on the "Packages" tab you should
see the PWFSLSmoke package at version 1.1.27 or higher.

### AirSensor

Core functionality for working with Purple Air sensor data.  This package is in 
a private repository and cannot be installed with **devtools**. We first need
to clone the source code using Fork.

#### clone the repository

Open up Fork and choose "File > Clone...".

* Repository Url -- https://github.com/MazamaScience/AirSensor.git
* Parent Folder -- `C:\Users\<YOUR NAME>\Projects`
* Name -- AirSensor

Click on "Clone". This will take a few moments.

#### start a new project in RStudio

Return to RStudio and choose "File > New Project... > Existing Directory" and
navigate to `C:\Users\<YOUR NAME>\Projects`. (We could have kept everything 
in the `Documents` directory but this just feels wrong. We encourage the use
of a `Projects` directory directly underneath your user directory.)

Create a project from this directory. To be sure you are in the right place
type:

```
getwd()
[1] "C:/Users/<YOU NAME>/Projects/AirSensor"
```

(Note that R always uses the unix style '/' and converts to windows style '\\' 
behind the scenes.)

Now we can build the package using RStudio with "Build > Install and Restart".
This should cause the following to be displayed in the R console:

```
Restarting R session...

> library(AirSensor)
Loading required package: dplyr

...
```

Examples are found in the `local_examples/` directory but for now you can try 
the following:

```
library(MazamaSpatialUtils)
library(PWFSLSmoke)
library(AirSensor)

intializeMazamaSpatialUtils()

pas <- get(load("data/example_pas.rda"))
pas %>%
  filter(stateCode == "CA") %>%
  pas_leaflet()
```

#### building documentation

We use the **testthat** R package for unit testing and the **pkgdown** R package 
to build web documentation. As long as the package is private, you will need to 
install these packages to see this documentation.

```
install.packages("testthat")
install.packages("pkgdown")
pkgdown::build_site()
...
```

This will build the website and launch it in your default web browser.

----




