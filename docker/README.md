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
REPOSITORY                          TAG         IMAGE ID            CREATED             SIZE
...
mazamascience/mazamapurpleair       0.1.7       796ae3ba57e5        14 minutes ago      2.62GB
mazamascience/mazamapurpleair       latest      796ae3ba57e5        14 minutes ago      2.62GB
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
docker run --rm -ti mazamascience/mazamapurpleair R --vanilla
...
library(MazamaPurpleAir)

data(example_pas)
pas <- example_pas

names(pas)

 [1] "ID"                               "parentID"                        
 [3] "label"                            "DEVICE_LOCATIONTYPE"             
 [5] "THINGSPEAK_PRIMARY_ID"            "THINGSPEAK_PRIMARY_ID_READ_KEY"  
 [7] "THINGSPEAK_SECONDARY_ID"          "THINGSPEAK_SECONDARY_ID_READ_KEY"
 [9] "latitude"                         "longitude"                       
[11] "pm25"                             "lastSeenDate"                    
[13] "sensorType"                       "flag_hidden"                     
[15] "flag_highValue"                   "isOwner"                         
[17] "flag_attenuation_hardware"        "temperature"                     
[19] "humidity"                         "pressure"                        
[21] "age"                              "pm25_current"                    
[23] "pm25_10min"                       "pm25_30min"                      
[25] "pm25_1hr"                         "pm25_6hr"                        
[27] "pm25_1day"                        "pm25_1week"                      
[29] "statsLastModifiedDate"            "statsLastModifiedInterval"       
[31] "countryCode"                      "stateCode"                       
[33] "timezone"                         "pwfsl_closestDistance"           
[35] "pwfsl_closestMonitorID"          

pas %>% 
  filter(stateCode == 'CA') %>% 
  filter(pwfsl_closestDistance < 100) %>% 
  pull(pwfsl_closestDistance) %>% 
  round() %>% 
  table()

 1  2  4  6  7 10 11 12 13 15 16 18 19 21 38 40 41 46 47 53 57 58 60 73 74 
 2  8  2  6  6  2  2  2  4  4  2  2  4  4  4  4  2  2  4  4  2  2  2  4  2 
```

## Publish the Docker Image ##

```
docker login
...
docker push mazamascience/mazamapurpleair:0.1.7
```


## Download the Docker Image ##

A recent image can also be obtained from DockerHub with:

```
docker pull mazamascience/mazamapurple:0.1.7
```

