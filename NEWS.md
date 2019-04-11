# AirSensor 0.2.0

* renamed package to *AirSensor*
* renamed `pat_internalData()` to `pat_scatterplot()` with improved functionality
* renamed `pat_outliers()` to `pat_outliers()` with improved functionality
* new utility functions `pat_isPat()`, `pat_isEmpty()`, `pat_extractMeta()`,
`pat_extractData()`

# AirSensor 0.1.9

* added generalized multiplot function `multi_ggplot()`
* include static mapping functionality with `pas_esriMap()`
* added `pat_filterData` 

# AirSensor 0.1.8

* `pat_subdate()` has been renamed to `pat_filterDate()` and defaults to 
the `America/Los_Angeles` timezone
* improved handling of date ranges in `pat_load()` -- all requests are assumed
to be in the sensor's local timezone
* `pas_load()` function now downloads pre-generated `pas` objects
* a new `pas_loadLatest()` function downloads raw synoptic data from Purple
Air and generates a `pas` object
* simplified docker image usage

# AirSensor 0.1.7

* include docker image
* added subdating feature

# AirSensor 0.1.6

* added multiplotting tools
* added outlier detection
* added pat_internalData

# AirSensor 0.1.5

* added PurpleAir timeseries functionality
* updated PurpleAir synoptic vignette

# AirSensor 0.1.4

* improved, more consistent documentation
* renamed example datasets to `example_pas` and `example_raw_pas`

# AirSensor 0.1.3

* added documentation file for package datasets
* changed header of vignette so that it is built properly

# AirSensor 0.1.2

* added parameter validation and testing for all existing functions
* adding `data/` directory with sample `pas` object
* added `vignettes/purple-air-synoptic.Rmd`

# AirSensor 0.1.1

* added parameter validation to pas_leaflet.R
* added test-pas_leaflet.R file

# AirSensor 0.1.0

Initial functions to download and map Purple Air synoptic data.

* downloadParseSynopticData.R -- gets the most recent syoptic data from purpleair.com
* enhanceSynopticData.R -- adds spatial metadata to a synoptic dataset
* initializeMazamaSpatialUtils.R -- convenience function to install Mazama spatial data
* pas_leaflet.R -- creates an interactive map from a synoptic dataset
* pas_load.R -- download/parse/enhance synoptic data
* pwfsl_load.R -- download PWFSL monitoring data

