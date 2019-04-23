<<<<<<< HEAD
# AirSensor 0.2.5 

* added `createASTimeseriesObject()` 
=======
# AirSensor 0.2.5

* changed parameter name from `param` to `parameter` in `pas_leaflet()`
* changed `pat_sample()` outlier detection window size to `n = 23` to match
`pat_outliers()`
>>>>>>> 64ae4a4834fcba879a69387e2ee8ee095f938635

# AirSensor 0.2.4

* uniform parameter validation in all `pat~` functions
* improved defaults for `pat_sample()` function
* minor improvements to `pat~` plot functions

# AirSensor 0.2.3

* `pat_sample()` included to sample `pat` datasets
* `pat_dygraphs()` included to plot JavaScript based "dygraphs"

# AirSensor 0.2.2

* `pat_multiplot()` time axis now in sensor local time
* `pat_multiplot()` has a new `pm25_over` plottype
* much improved `pat_scatterplot()`

# AirSensor 0.2.1

* minor documentation cleanup
* graphical options and other improvements for `pas_esriMap()`
* added `local_examples/example_02_pas-filtering.R`
* added `pas_filterArea()`
* new utility functions `pas_isPas()`, `pas_isEmpty()`

# AirSensor 0.2.0

* renamed package to *AirSensor*
* renamed `pat_internalData()` to `pat_scatterplot()` with improved functionality
* renamed `pat_outliers()` to `pat_outliers()` with improved functionality
* new utility functions `pat_isPat()`, `pat_isEmpty()`, `pat_extractMeta()`,
`pat_extractData()`
* first pass at `pat_internalFit()` function
* added `pas_filter()` to filter toolbox
* fixed binning of `pas_esriMap()`

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

