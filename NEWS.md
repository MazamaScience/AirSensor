# AirSensor 0.8.3

* Restored unit tests for `pat_filterDate()`.

# AirSensor 0.8.2

* Removed unnecessary `pat_createNew()` check for spatial data.

# AirSensor 0.8.1

* New `pat_upgrade()` function to bring version 0.6 'pat' objects up to the
version 0.8 data model.
* Improved error messages in `pas_createNew()`.
* Updated `pat_dygraph()` to include support for `parameter = "pressure"`.
* Refactored `pat_aggregateOutlierCounts()`.

# AirSensor 0.8.0

This release completely refactors the way that 'pat' data are downloaded from
ThingSpeak. Additional data columns are added to the standard `pat$data`
dataframe.

* Refactored `pat_downloadParseRawData()` data access times and also reduced the
volume of data downloaded by switching from the `.json` to the `.csv` API from
ThingSpeak. This function now returns a list of five dataframes.
* Renamed and refactored `pat_createPATimeseriesObject()` to work with the new
list of dataframes from `pat_downloadParseRawData()`. New fields `pressure` and
`bsec_iaq` are included in the created `pat` object.
* Including dependency on **httpcode** and **readr** packages.
* Renamed `pat_multiplot()` to `pat_multiPlot()` (capital 'P').
* Renamed `pat_scatterMatrixPlot` to `pat_scatterPlotMatrx()`.

# AirSensor 0.7.6

* `pat_createAirSensor()` 
* `pas_load()` now ignores duplicated warning messages.
* The upper threshold for `pm25` in `pat_qc()` was increased to 2,000 based on 
a 2020-05-14 conversation with PurpleAir founder Adrian Dybwad.

# AirSensor 0.7.5

This release completely refactors the way that aggregation and QC are 
performed. The changes should make the code easier to maintain, faster and
much more flexible for those developing QC algorithms.

* New `pat_aggregate()` and `pat_createAirSensor()` functions.
* New default QC function `PurpleAirQC_hourly_AB_02()`.
* `pat_externalFit()` and `pat_monitorComparison()` updated to use new 
aggregation/QC functions.
* Previous version of `pat_aggregate()` and `pat_createAirSensor()` are now
now available as `pat_aggregate_old` and `pat_createAirSensor_old()`.
* `PurpleAirQC_validationPlot()` renamed to `PurpleAirQC_validationPlot_old()`.
* `pat_outliers()` defaults updated to reflect current default 120 second sample
interval.
* `pat_multiPlot()` now returns a `ggplot2` object which can be manipulated.

# AirSensor 0.7.4

* Renamed `downloadParseTimeseriesData()` to `pat_downloadParseRawData()`.

# AirSensor 0.7.3

* Improved error messaging in `pas_load()`.

# AirSensor 0.7.2

* Updated `pas_upgrade()` to add `sensorManufacturer`, `targetPollutant` and
`technologyType` columns.

# AirSensor 0.7.1

* Updated example `archiveBaseUrl` from `http://smoke.mazamascience.com/data/PurpleAir`
to `http://data.mazamascience.com/PurpleAir/v1`.

# AirSensor 0.7.0

* Renamed `pat_scatterPlot()` to `pat_scatterPlotMatrix()`.

# AirSensor 0.6.17

* Renamed `downloadParseSynopticData()` to `pas_downloadParseRawData()`.
* Renamed `enhanceSynopticData()` to `pas_enhanceData()`.
* New functions used in creating `pas` objects: `pas_addAirDistrict()`,
`pas_addSpatialMetadata()`, `pas_addUniqueIDs()`, `pas_addUniqueIDs()`,
`pas_hasSpatial()`.

# AirSensor 0.6.16

* New `pas_upgrade()` function to upgrade `pas` files created with 
**AirSensor** version 0.5.

# AirSensor 0.6.15

* Fixed bug in `enhanceSynopticData()` when `countryCodes = NULL`. This was
encountered when running `pas_createNew()` with `countryCodes = NULL`.
* Added "PurpleAir Synoptic Data" article.

# AirSensor 0.6.14

* PAS function documentation typo fixes.
* Added "Developer Style Guide" article.
* Updated to use `MazamaCoreUtils::loadDatafile()` function .
* Added more unit testing for `pas_~()` functions.

# AirSensor 0.6.13

* Proper timezone handling in `pat_multiPlot()` and `pat_monitorComparison()`

# AirSensor 0.6.12

* Separated calculation of SoHIndex into `PurpleAirSoH_dailyToIndex()`

# AirSensor 0.6.11

* Fixed bug in `pat_isEmpty()` so that it test for an empty `data` dataframe
rather than an empty `meta` dataframe.
* `pat_trimDate()` now preserves the first day if it begins at local midnight.

# AirSensor 0.6.10

* Improved error message in `downloadParseTimeseriesData()`.

# AirSensor 0.6.9

* Archive directory structure was changed so that monthly pat files are found
in a monthly directory underneath `pat/YYYY/`. The `pat_loadMonth()` function
was modified to search in this location.
* `pat$meta$pm25_A/B` values now come from the raw data _outdoor_ value
` PM2.5 (ATM)` rather than the _indoor_ value `PM2.5 (CF=1)`. See
https://www2.purpleair.com/community/faq#!hc-what-is-the-difference-between-cf-1-and-cf-atm
* Added more logging to `downloadParseTimeseriesData()`.

# AirSensor 0.6.8

* Changed default value to `countryCodes = NULL` in `pas_createNew()` and
`enhanceSynopticData()` so that it is easier to create _pas_ objects for other
countries.
* Removed `countryCode` and `stateCode` arguments from `pas_get~` functions.
Filter should be done with `pas_filter()` before these functions are called.

# AirSensor 0.6.7

* Fixed bug in `enhanceSynopticData()` which failed to add `pwfsl~` columns when
`includePWFSL = FALSE`. (The columns should still exist but with all NA.)
* All `pas_get~()` functions now support subsetting by `countryCode` and US
`stateCode`.

# AirSensor 0.6.6

* Added `days` argument to `pat_loadLatest()`.
* `pat_load()` and `sensor_load()` default to the last 7 days when no dates
are provided.
* New `pat_trimDate()` function trims data to local time full days.
* `pas_isPas()` no longer requires presence of optional `pwfsl_~` columns.

# AirSensor 0.6.5

* Reordered arguments in `pas_filterNear()` to package standard
`longitude, latitude`.

# AirSensor 0.6.4

* Refactored _pas_ objects to include: `deviceID`, `locationID`, 
`deviceDeploymentID`.
* Refactored `pat_createNew() and `pat_load~()` functions so that the first `id` 
argument is used as the `deviceDeploymentID` unique time series identifier.
* The second and third arguments are now `label` and `pas` and provide an 
alternative to specifying `id`.
* Recreated example data with the new functions.

# AirSensor 0.6.3

* Improved examples.

# AirSensor 0.6.2

* Documentation corrections.
* Recreated all `exaple_pat~` data files with so they have the new metadata
fields: `sensorID`, `locationID` and `deviceDeploymentID`.

# AirSensor 0.6.1

Revisited everything at the `pas` object level:

* Error message harmonization.
* Updated package data files.
* Updated documentation.
* Updated tests.
* Renamed `pas_sensorDeploymentID()` to `pas_deviceDeploymentID()`

# AirSensor 0.6.0

Version 0.6 is a backwards *in*compatible release. It replaces `label` identifier
with a proper "sensor-deployment" identifier and relies on the
*MazamaLocationUtils* package to create location identifiers. Becuase of this
change, archives must be regenerated and many functions must be refactored.

New fields in `pat$meta` include:

* `sensorID` -- same as `ID` but more explicit
* `locationID` -- generated with `MazamaLocationUtils::location_createID()`
* `sensorDeploymentID` -- combination of the above

The following functions were added/refactored to use "sensor-deployment"
identifiers:

* `pas_sensorDeploymentID()` creates a unique "sensor-deployment"
identifier. This identifier is used in creation of file names in the archive
database and as the unique time series identifer in _airsensor_ objects.
* `pas_getColumn()`, `past_getLabels()`, `pas_getIDs()`
* `pat_createNew()`
* `pat_load()`, `pat_loadLatest()`, `pat_loadMonth()`
* `pat_createAirSensor()` 

# AirSensor 0.5.18

* Linted for timezones - fixed in Mutliplot and monitor comparison. 

# AirSensor 0.5.17

Minor documentation updates and package rebuild.

# AirSensor 0.5.16

* `sensor_polarPlot()` now uses the second-nearest met station if no data are
found at the nearest station.
* `pat_filterDate()` now issues a warning if requested data range is outside
aviailable date range
* removed print statements from `downloadParseTimeseriesData()`

# AirSensor 0.5.15

Additional functionality for calculating a state of health index to be used as
an overall assessment of sensor functioning.

* renamed `pat_dailyStateOfHealth()` to `pat_dailySoH()`
* new functions `pat_dailySoHPlot()`, `pat_dailySoHIndex_00()`, 
`pat_dailySoHIndexPlot()`

# AirSensor 0.5.14

Modified how `pat_filterDate()` obtains the `timezone` used to interpret the
incoming `startdate` and `enddate`:

1) `startdate` timezone if it is `POSIXct`
2) `timezone` if it is passed in
3) `pat$meta$timezone` otherwise

# AirSensor 0.5.13

* `pat_outliers()` no longer fails in the face of DC signals
* New `pas_getLabels()` functions simplifies the creation of vectors of sensor 
labels (currently used as unique identifiers)

# AirSensor 0.5.12

Refactored `PurpleAirSoH_daily~()` functions to use *dplyr* resulting in code
that runs faster and is easier to understand.

* Changed `~SoH_dailyCorrelation()` to `~SoH_dailyOtherFit()`.
* Changed `timeseriesTbl_multiPlot()` argument `parameterPatter` to `pattern`.

# AirSensor 0.5.11

Fixed bug in datetime axis that caused `SoH` functions to return missing values
after the switch from PST to PDT.

# AirSensor 0.5.10

Removed `logger.error()` statements from the following low-level functions as
they couldn't be turned off and ended up cluttering the log files:

 * `loadDataFile()`
 * `pas_load()`
 * `pat_createNew()`
 * `pat_loadLatest()`
 * `pat_loadMonth()`
 * `sensor_loadLatest()`
 * `sensor_loadMonth()`
 * `sensor_loadYear()`
 
Improved `pat_join()` logic to deal with metadata changes associated 
with the value `pwfsl_closestMonitorID` when _temporary_ monitors come and go
in the PWFSL database.

# AirSensor 0.5.9

New `PurpleAirSoH~` functions calculate daily state-of-health metrics from PAT
data. These metrics can be combined to create multi-metric indices that provide
an overall assessment of the health of a Purple Air sensor.

 * `PurpleAirSoH_dailyCorrelation()`
 * `PurpleAirSoH_dailyPctDC()`
 * `PurpleAirSoH_dailyPctReporting()`
 * `PurpleAirSoH_dailyPctValid()`
 * `pat_dailyStateOfHealth()`

# AirSensor 0.5.8

* Added `sensor_loadYear()` to load annual files (~2-3 MB).
* `sensor_load()` now attempts to load and join annual files before attempting
the slower process of loading and joining monthly files.
* Removed excessive logging in some of the loading functions.

# AirSensor 0.5.7

* `timeseriesTbl_multiPlot()` now supports a `style` paramter which can be set
to `"point"`, `"line"` or `"area"`.
* Improved handling of missing files in data loading functions.
* Added state-of-health functions: `SoH_pctValid()`, `SoH_pctDC()`.
* Harmonized naming of state-of-health metrics.

# AirSensor 0.5.6

* Updated `pat_createNew()` and `downloadParseTimeseriesData()` to support the
`id` parameter.
* Enhanced `PurpleAirQC_aggregationPlot()` to work with any tibble or dataframe
and renamed it to `tbl_multiplot()`.
* Added a state-of-health metrics calculation function: `SoH_pctReporting()`.

# AirSensor 0.5.5

* Now requiring **MazamaCoreUtils** 0.3.10.
* `pat_aggregate()` non longer stops in the face of data with a DC signal on
one of the pm25 channels.
* Added `aggregation_FUN` argument to `pat_createAirSensor()` to allow for
custom aggregation statistics.

# AirSensor 0.5.4

* Added `PurpleAirQC_aggregationPlot()` function.
* Improved `datestamp` handling in `pas_load()`.
* More explicit use of timezones to avoid local/UTC misunderstandings.

# AirSensor 0.5.3

* Added `make.names` argument to `pat_loadMonth()` and `pat_loadLatest()`.

# AirSensor 0.5.2

* Fixed bug in `pat_dygraph()` which referenced `PWFSLSmoke::parseDatetime()`
which is now deprecated in favor of `MazamaCoreUtils::parseDatetime()`.

# AirSensor 0.5.1

* Upudated data loding to allow for loading data from a local archive. New
functions include: `loadDataFile()`, `getArchiveBaseDir()`,
`setArchiveBaseDir()`.
* All `~_load()` and `~_loadLatest()` functions now call `loadDataFile()` 
* `pat_load()` now continues working in the face of missing data files.
under the hood and will work with data archives found at `archiveBaseDir`.
* Removed `pat_calendarPlot()` and `sensor_calendarPlot()` from the packge.
These work-in-progress functions are now found in `local_R/`. We anticipate
including calendar plotting functionality in the **AirMonitorPlots** package.

# AirSensor 0.5.0

Version 0.5.x represents the version of the package that is ready for public 
release.

* Version bump.

# AirSensor 0.4.10

* Fixed bug in `sensor_filterMeta()` where filtering the number of rows in
`sensor$meta` did not also filter columns in `sensor$data`.

# AirSensor 0.4.9

* Updated `sensor_videoFrame()` with new arguments and new defaults for 
`colorPalette` and `colorBins`.
* Updated `pat_sample()` to handle cases where the A or B channel contains
only missing values.

# AirSensor 0.4.8

* `sensor_loadLatest()` no supports a `days` argument and defaults to loading
a 45-day file.

# AirSensor 0.4.7

* Improved `local_executables/` scripts.
* Added `timezone` argument to `downloadParseTimeseriesData()`.
* Improved logging in `local_executables/crontab~` files.
* Fixed implementation of `downloadParseTimeseriesData()` to avoid using day
boundaries when explicit times are specified.

# AirSensor 0.4.6

* `pas_load()` default `retries` was increased from 10 to 30.
* Fixed bug where `pat_createNew()` created time ranges that ended (UTC - local)
hours short of the requested `enddate`.
* Plots generated with `pat_externalFit()`, `pat_internalFit()`, 
`pat_monitorComparison()` and `pat_outliers()` 
now have time axes with sensor local time rather than UTC.
* `pat_externalFit()` and `pat_monitorComparison()` labeling now includes PWFSL 
monitor `siteName`
* Documentation improvements.
* Internal refactoring for consistency.

# AirSensor 0.4.5

* Using `MazamaCoreUtil::stopIfNull()` throughout.

# AirSensor 0.4.4

* Updated requirements: MazamaCoreUtils (>= 0.3.5).
* All time-related functions now specify a timezone explicitly so as to avoid
confusion related to the default `base::Sys.timezone()`.
* `enhanceSynopticData()` now handles presence or absence of `State` column
in raw data obtained from PurpleAir. This was apparently removed some time 
during the summer of 2019.

# AirSensor 0.4.3

* Updated `pat_calendarPlot()` and `sensor_calendarPlot()` to handle discrete
color scales.

# AirSensor 0.4.2

* `pat_createAirSensor()` now uses `pat$meta$label` to populate 
`sensor$meta$siteName`.
* New `sensor_calendarPlot()`.
* Added `aspectRatio` argument to `~calendarPlot()` functions.
* Updated requirements: MazamaCoreUtils (>= 0.3.1), PWFSLSmoke (>= 1.2.100)
* `sensor_load()` no longer stops when monthly files cannot be found. This helps
when asking for the current year's worth of data for a calendar plot.

# AirSensor 0.4.1

* Minor documentation tweaks.
* Updated usage of `MazamaCoreUtils::dateRange()` to reflect change from "end of
enddate" to "start of enddate".

# AirSensor 0.4.0

* New `setArchiveBaseUrl()` and `getArchiveBaseUrl()` functions allow per 
session specification of the location of pre-generated data files.
* Removed `baseUrl` parameter from all data loading functions. Now users must
begin a session with `setArchiveBaseUrl()`.
* Modified behavior of `sensor_load()` to trim data to the requested time
range.
* Fixed bug in `pat_scatterPlot()` that generated an error when the number of
records in the `pat` object was fewer than the `sampleSize` parameter.

# AirSensor 0.3.15

* New `pat_calendarPlot()` tailored to full-year calendar heatmaps.
* Added `pat_distinct()` to remove duplicate data records.
* All `pat_~()` functions now remove duplicate records to guarantee proper
functioning of chained functions.

# AirSensor 0.3.14

* Removed Shiny app related functions.
* Added `pat_loadLatest()` to always get the most recent 7 days.
* Added `pat_loadSensor()` to always get the most recent 7 days.
* Added `timezone` argument to `pat_createNew()`.
* `pat_createNew()` now accepts start and end points not on date boundaries.

# AirSensor 0.3.13

* Removed popups from `AirShiny_leaflet()`
* Fixed `pas_leaflet()` coloring issue.pwd
* Fixed bug in `pat_load()` which didn't trim data to date boundaries when a
local timezone was passed in. 
* Added `pat_calendarPlot()` to plot annual daily heat map. 

# AirSensor 0.3.12

* Renamed `pas_loadLatest()` to `pas_createNew()`.
* Renamed `pat_loadLatest()` to `pat_createNew()`.
* Added "Too Many Requests" error messages to `download~()` functions.
* Added `timezone` parameter to `pas_load()`.
* Included `timezone` information in every internal function call where it can
be specified so that R's default "system local timezone" does not accidentally 
get used.
* When no wind data is provided, use nearest *worldmet* met station in 
`sensor_polarplot()` and `sensor_pollutionRose()`.
* Added `PurpleAirQC_algorithm` to `sensor` objects created by
`pat_createAirSensor()` so that they self-document how they were created.
* Included an `archival` argument to `pas_load()`. When `archival = TRUE` a
`pas` object will be loaded that contains metadata for all PurpleAir sensors
including those that have ceased reporting.
* Removed unneeded `downloadParseSensorList()`.
* Changed argument `name` to `label` in `pat_createNew()` and 
`downloadParseTimeseriesData()`.
* Updated all scripts in `local_executables/` to work with the latest release.

# AirSensor 0.3.11

* Faster geodesic calculations in `pas_staticMap()` and `pas_filterNear()`.
* Include `downloadParseSensorList()` to download a list of archived PurpleAir 
sensors. 

# AirSensor 0.3.10

* Added `sensor_videoFrame()` function to create a single frame map for a
network of sensors. These can be used to create videos showing the evolution
of PM2.5 levels over several days.
* Added `local_executables/createVideo_exec.R` script to generate mp4 videos.
* Added `ylim` argument to `pat_multiPlot()`.

# AirSensor 0.3.9

* Added `sensor_filter()`, `sensor_filterDate()` and `sensor_filterMeta()`.
* Added `local_examples/downloadSpeeds.Rmd` to benchmark data download times
from ThinkSpeak.
* New `Purple Air Failure Modes` vignette.

# AirSensor 0.3.8 

* Added `pat_aggregateOutlierCounts()` to count outliers per aggregation
period. 
* Revamped `pat_aggregate()` to fix warnings and optimize

# AirSensor 0.3.7 

* Added `wind_loadMonth()` to load pre-generated monthly wind data
* Added `wind_load()` to load pre-generated wind data from timestamps
* Updated `sensor_pollutionRose()` to accept new wind data model
* Added `sensor_polarPlot()` to plot bivariate polar plots
* Renamed `airsensor_load~()` to `sensor_load~()`.
* Added `sensor_~` utility functions: `isSensor()`, `isEmpty()`, `extractMeta()`
, `extractData()`.
* Added `example_sensor` dataset for use in documentation examples.
* Added `local_examples/LA_fireworks_2019.R`
* Default required data retentaion rate during hourly aggregation was increased
from 10/30 to 20/30 (`min_count = 20`).
* Improved examples in the documentation.
* Suppressing warnings from `pat_scatterPlot()`.
* Updated "PurpleAir Timeseries" vignette.
* Added `returnAllColumns` option to `PurpleAirQC_~1 functions.
* New `PurpleQC_validationPlot()` function.
* `pat_createPATimeseriesObject()` now retains additional metadata:
`sensorManufacturer`, `targetPollutant`, `technologyType`, `communityRegion`
* Updated all package datasets so they include additional metadata.

# AirSensor 0.3.6

* Fixed `airsensor_load()` so that it includes monitors found in _any_ month
rather that those found in _every_ month.
* Fixed `pat_createPATimeseriesObject()` and `pat_createAirSensor()` so that they 
no longer generate `NaN` or `Inf` values.

# AirSensor 0.3.5

* Added `as_pollutionRose()`
* Added `createMonthlyWind_exec()`
* Include `example_as` as an example "airsensor" object
* `initializeMazamaSpatialUtils()` now only sets up logging if it hasn't 
already been set up.
* Cleanup/refactoring of `local_executables` scripts.
* Modified `pat_loadMonth()` to use the newer `pat_<label>_<monthstamp>.rda` 
naming system.

# AirSensor 0.3.4

* Added `pat_monitorComparison()`.
* Added `local_examples/bikesgv_story.Rmd`.
* Improved error handling in `pas_filter()`.
* Added `"pm25_a"` and `"pm25_b"` plot types to `pat_multiPlot()`.

# AirSensor 0.3.3

* renamed `pas_within()` to `pas_filterNear()`
* renamed `airSensorQC_~` functions to `PurpleAirQC_~`
* refactored scripts in `local_executables/` to be more similar

# AirSensor 0.3.2

* Created an archive of `airsensor` data files with pre-generated, 
hourly-aggregated data suitable for use with the *PWFSLSmoke* package.
* Added `airsensor_load()` to load pre-generated, hourly-aggregated data files
suitable for use with the *PWFSLSmoke* package.

# AirSensor 0.3.1

* Added quality control algorithms: `hourly_AB_00` and `hourly_AB_01`.
* Updated `pat_cerateAirSensor()` to accept arguments that impact the conversion
of `pat` data into aggregated period-averages: `period`, `channel`, `qc_algorithm`,
`min_count`.
* Fixed `pat_aggregate()` so that any `NaN` or `Inf` values are converted to `NA`.

# AirSensor 0.3.0

The *AirSensor* package is now almost feature complete with functions for QC
and aggregation to an houly axis.

* Fixed bug in `pat_aggregate()` that occasionaly returned empty columns of data.
* Added `local_examples/Jons_qc_1.R` with Jon's best take on appropriate QC of
the hourly aggregated data. 
* Updated AirShiny theme and include About section
* Revamped AirShiny UI
* Support for `pat_createAirSensor()` added to barplot 
* Dockerized AirShiny
* Updated Namespace
* Added `global.R` to improve clarity of scope
* Added hex-sticker logo for AirShiny 
* Added `pas_within()` for spatial analysis

# AirSensor 0.2.18

* New `pat_createAirSensor()` function converts from `pat` object to `airsensor`
object that is compatible with the `PWFSLSmoke` package.
* Changed `pat_qc()` argument `humidityMax` --> `max_humidity`.

# AirSensor 0.2.17

* Refactored `pat_aggregate()` to fix an issue with t-test statistics. Also
simplified the function signature to accept just `pat` and `period` arguments.

# AirSensor 0.2.16

* Updated `pat_load()` to default to the most recent week of data
* New `pat_qc()` function applies low level QC
* updated `pat_outliers()` to retain records with missing PM2.5 values when
`replace = TRUE`
* modified `pat_aggregate()` defaults to: `period = "1 hour", quickStats = TRUE`
* removed `pat_aggregate()` `dataThreshold` argument
* added `example_pat_failure_B` dataset with severe A channel errors

# AirSensor 0.2.15

* Fixed default parameters bug in `pat_scatterPlot()`
* New shiny utilities for AirShiny

# AirSensor 0.2.14 

* Remove out-of-time-range records in `pat_loadLatest()`
* Corrected default date calculation in `createMonthlyPAT_exec.R`

# AirSensor 0.2.13 

* Added ShinyAirSensor directory for alpha web-apps
* Added `threePlot` web-app
* Added `dailyAveragePlot` web-app

# AirSensor 0.2.12

* renamed internal `sample()` to `.sample()` to avoid confusion with
`base::sample()`
* simplified end-user parameters
* Added ShinyAirSensor
* `utils-plot.R` added to support general functions
* `utils-gen.R` added to support general functions

# AirSensor 0.2.11

* `local_examples/07_pat_archive.R` demonstrates how to efficiently work with
pre-generated `pat` files from the archive
* new `pat_loadMonth()` loads pre-generated "pat" objects from a data archive
* simplified `pat_aggregate()` -- it now always returns all statistics
* removed unused `plotList` parameter from `pat_multiPlot()`
* `pat_join()` can now accept either individual `pat` objects or a list of 
`pat` objects
* new `local_executables/createMonthlyPAT_exec.R` script for populating an
archive with `pat` data files

# AirSensor 0.2.10

* renamed `pat_load()` to `pat_loadLatest()`
* removed `subset` and `weights` parameters from `pat_internalFit()`
* removed unused `param` parameter from `pat_join()`

# AirSensor 0.2.9

* `ast_createAirSensor()` converts `ast` objects into "airsensor" objects that
are compatible with the *PWFSLSmoke* package
* `initializeMazamaSpatialUtils()` now imports all datasets need to create
`pas` objects
* updated `example_pas` data file has additional fields introduced by the 
0.2.8 version of `enhanceSynopticData()`
* removed first attempt `pat_timeAverage()` function
* new `pat_aggregate()` function performs temporal aggregation

# AirSensor 0.2.8

* improvements in `enhanceSynopticData()` now handle changing order of json
properties and validate locations before adding spatial metadata
* new `pat_createASTimeseries()` function handles conversion of 
Purple Air-specific "pat" objects into sensor-generic "ast" objects.
* new `ast_createAirSensor()` objects converts "ast" objects into a "as" data
type that is compatible with the "ws_monitor" data type used in the 
*PWFSLSmoke* package

# AirSensor 0.2.7

* corrected the algorithm for `pat_sample(forGraphics = TRUE)`
* consistent support for named palettes in `pas_leaflet()` and `pas_staticMap()`
* `enhanceSynopticMetadata()` adds the following columns to a `pas` object:
  - `airDistrict` -- CARB air district
  - `sensorManufacturer = "Purple Air"`
  - `targetPollutant = "PM"`
  - `technologyType = "consumer-grade"`
  - `communityRegion` -- (where known)
  
# AirSensor 0.2.6 

* added `example_pat_failure` dataset
* added `createASTimeseriesObject()`
* improved labeling in all plots
* new `pas_staticMap()` function with customizable base maps and color schemes
* removed `pas_esriMap()` because the ESRI map service we were using started
requiring tokens on April 25, 2019

# AirSensor 0.2.5

* changed parameter name from `param` to `parameter` in `pas_leaflet()`
* changed `pat_sample()` outlier detection window size to `n = 23` to match
`pat_outliers()`


# AirSensor 0.2.4

* uniform parameter validation in all `pat~` functions
* improved defaults for `pat_sample()` function
* minor improvements to `pat~` plot functions

# AirSensor 0.2.3

* `pat_sample()` included to sample `pat` datasets
* `pat_dygraphs()` included to plot JavaScript based "dygraphs"

# AirSensor 0.2.2

* `pat_multiPlot()` time axis now in sensor local time
* `pat_multiPlot()` has a new `pm25_over` plottype
* much improved `pat_scatterPlot()`

# AirSensor 0.2.1

* minor documentation cleanup
* graphical options and other improvements for `pas_esriMap()`
* added `local_examples/example_02_pas-filtering.R`
* added `pas_filterArea()`
* new utility functions `pas_isPas()`, `pas_isEmpty()`

# AirSensor 0.2.0

* renamed package to *AirSensor*
* renamed `pat_internalData()` to `pat_scatterPlot()` with improved functionality
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
* improved handling of date ranges in `pat_loadLatest()` -- all requests are assumed
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

