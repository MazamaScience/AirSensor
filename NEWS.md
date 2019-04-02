# MazamaPurpleAir 0.1.7.0

* include docker image
* added subdating feature

# MazamaPurpleAir 0.1.6.0

* added multiplotting tools
* added outlier detection
* added pat_internalData

# MazamaPurpleAir 0.1.4.1

* added PurpleAir timeseries functionality
* updated PurpleAir synoptic vignette

# MazamaPurpleAir 0.1.4

* improved, more consistent documentation
* renamed example datasets to `example_pas` and `example_raw_pas`

# MazamaPurpleAir 0.1.3

* added documentation file for package datasets
* changed header of vignette so that it is built properly

# MazamaPurpleAir 0.1.2

* added parameter validation and testing for all existing functions
* adding `data/` directory with sample `pas` object
* added `vignettes/purple-air-synoptic.Rmd`

# MazamaPurpleAir 0.1.1

* added parameter validation to pas_leaflet.R
* added test-pas_leaflet.R file

# MazamaPurpleAir 0.1.0

Initial functions to download and map Purple Air synoptic data.

* downloadParseSynopticData.R -- gets the most recent syoptic data from purpleair.com
* enhanceSynopticData.R -- adds spatial metadata to a synoptic dataset
* initializeMazamaSpatialUtils.R -- convenience function to install Mazama spatial data
* pas_leaflet.R -- creates an interactive map from a synoptic dataset
* pas_load.R -- download/parse/enhance synoptic data
* pwfsl_load.R -- download PWFSL monitoring data

