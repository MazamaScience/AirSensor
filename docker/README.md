## Create the Docker Image ##

A quick refresher on docker commands is available at the 
[docker cheatsheet](https://github.com/wsargent/docker-cheat-sheet).

A docker image with all required prerequisites can be built with the `Makefile` 
in this directory:

```
make production_build
```

You should then be able to see something like the following:

```
$ docker images
REPOSITORY                    TAG         IMAGE ID            CREATED             SIZE
...
mazamascience/airsensor       1.0.7       796ae3ba57e5        2 minutes ago       3.74GB
mazamascience/airsensor       latest      796ae3ba57e5        2 minutes ago       3.74GB
...
```

> It is best practice to create versioned images and tag the most recent one 
with "latest".

Spatial data required by the **MazamaSpatialUtils** package already exists in 
the docker image in `/home/mazama/data/Spatial`.

## Test the Docker Image ##

Having built the docker image we can now test it. The following output was 
obtained use the provided `example_pas` synoptic dataset:

```
docker run --rm -ti mazamascience/airsensor R --vanilla
...
library(AirSensor)

data(example_pas)
pas <- example_pas

> names(example_pas)
 [1] "ID"                               "label"                           
 [3] "DEVICE_LOCATIONTYPE"              "THINGSPEAK_PRIMARY_ID"           
 [5] "THINGSPEAK_PRIMARY_ID_READ_KEY"   "THINGSPEAK_SECONDARY_ID"         
 [7] "THINGSPEAK_SECONDARY_ID_READ_KEY" "latitude"                        
 [9] "longitude"                        "pm25"                            
[11] "lastSeenDate"                     "sensorType"                      
[13] "flag_hidden"                      "isOwner"                         
[15] "humidity"                         "temperature"                     
[17] "pressure"                         "age"                             
[19] "parentID"                         "flag_highValue"                  
[21] "flag_attenuation_hardware"        "Ozone1"                          
[23] "pm25_current"                     "pm25_10min"                      
[25] "pm25_30min"                       "pm25_1hr"                        
[27] "pm25_6hr"                         "pm25_1day"                       
[29] "pm25_1week"                       "statsLastModifiedDate"           
[31] "statsLastModifiedInterval"        "countryCode"                     
[33] "stateCode"                        "timezone"                        
[35] "deviceID"                         "locationID"                      
[37] "deviceDeploymentID"               "airDistrict"                     
[39] "pwfsl_closestDistance"            "pwfsl_closestMonitorID"          
[41] "sensorManufacturer"               "targetPollutant"                 
[43] "technologyType"                   "communityRegion"                 

pas %>% 
  pas_filter(stateCode == 'CA') %>% 
  pas_filter(pwfsl_closestDistance < 100) %>% 
  dplyr::pull(pwfsl_closestDistance) %>% 
  round() %>% 
  table()

 1  2  4  6  7 10 11 12 13 15 16 18 19 21 38 40 41 46 47 53 57 58 60 73 74 
 2  8  2  6  6  2  2  2  4  4  2  2  4  4  4  4  2  2  4  4  2  2  2  4  2 
```

## Publish the Docker Image ##

```
make production_publish
```


## Download the Docker Image ##

A recent image can also be obtained from DockerHub with:

```
docker pull mazamascience/airsensor:1.0.7
```

