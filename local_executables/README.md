# Sensor Data Ingest

Scripts in this repository do all the work of converting raw data from Purple
Air sensors into .rda files ready for use with the **AirSensor** R 
package

The `purpleair_createDailyPAS_exec.R script designed to be run shortly after 
midnight in a cron job. It will create a datestamped file that will contain a 
record of all sensors installed and hopefully generating data on that date.

## Installation Instructions for an Operational Site

**TODO**

## Post-Installation Checks

**TODO**

***
***

Now for the gory details.

## Files ##

**TODO**

## Output Directories ##

**TODO**

## Processing Logs ##

As each script is run, either at the command line or from a cron job, it will 
generate logging output in the output directory along with the final `.rda` 
file. Log files should contain the name of the processing script. Four 
different levels of logging are provided:

 * `ERROR` -- Something went wrong, sometimes resulting in no generation of an output file.
 * `INFO` -- Summary information on data processed along with any warnings generated.
 * `DEBUG` -- Detailed processing information to help understand where processing might have gone wrong.
 * `TRACE` -- *Excruciatingly* detailed processing information including URL requests.

Note that scripts run repeatedly in cron jobs will overwrite the logs so it is 
possible for the cronjob user to get an email describing a data processing 
failure (perhaps due to unavailable URLs) that resolved itself in subsequent 
runs.

## Testing ##

**TODO**

## Background Reading ##

A quick refresher on docker commands is available at the 
[docker cheat sheet](https://github.com/wsargent/docker-cheat-sheet).

