_This is a verbatim copy of the similarly named .pdf file in this directory. 
This document was a personal communication on May 8, 2020 and is reproduced here
as a `.md` file for easy reading._

----

# PurpleAir CF=ATM vs CF=1

This document describes the meaning and labeling of CF=ATM and CF=1 fields in 
the PurpleAir sensors. It will discuss a bug that caused the labels for these 
two sets of particulate readings to be swapped and what PurpleAir has done about 
that.

## Background

Plantower laser counters used in the PurpleAir sensors output two sets of 
particulate measurements with different “correction factors”. These correction 
factors were developed by the manufacturer, Plantower, and the methodology of 
the development of these correction factors is unknown. While there are unknowns, 
the manufacturer has provided direction in the operational manual for the PMS5003, 
stating that the CF=1 is applicable for a standard environment (indoors / chamber) 
while the CF=ATM is applicable under atmospheric environment (outdoors / am202bient).

The PMS5003 operational manual can be accessed here:
http://www.aqmd.gov/docs/default-source/aq-spec/resources-page/plantower-pms5003-manual_v2-3.pdf?sfvrsn=2

## History

When PurpleAir started, we had no detailed manual for the PMS laser counters and 
as such no clear understanding of the meaning of, or the differences between these 
two sets of data.

We knew that the one set read lower at higher concentrations and since the sensors 
were already known to read high, these numbers were chosen to be used to display 
on the map.

## What Happened?

PurpleAir mistakenly labeled the lower readings CF=1 and the higher set of 
numbers CF=ATM. All three readings for PM1, PM2.5 and PM10 were swapped. This 
bug was present from day one and did not change during the firmware updates all 
the way up to version 4.11.

## How we are fixing it

### ThingSpeak Data

On October 20th 2019, PurpleAir updated the download tool 
(​www.purpleair.com/sensorlist​), fixing the labels so any files downloaded after 
that date will have the correct column headers. There will not be any changes to 
thingspeak data fields uploaded by the PurpleAir firmware. All past downloaded 
data is still valid if you use the new column headers.
   
### JSON data

Firmware version 5.0 will be an update that corrects the labels in the json 
produced by the sensor and uploaded to PurpleAir servers as well as third party 
providers such as custom servers and Azure. JSON data uploaded to Weather 
Underground does not change.

### SD Card Data

In firmware V5.00, the same data is written to the same columns. The only change 
is in the CSV headers which are updated with the correct labels. All past data 
is still valid assuming you use the new headers.

### PurpleAir Map

The PurpleAir map has always used the lower of the two numbers (CF=ATM). PurpleAir 
updated the map on 21 November 2019 to selectively use CF=ATM or CF=1 values for 
outdoor and indoor sensors respectively. The PurpleAir map will interpret the 
older firmware’s messages correctly, swapping the fields as needed.

## Summary of changes

- Thingspeak: The fields uploaded to thingspeak do not change in that the same 
data is uploaded to the same fields after version 5 firmware. The labels in the 
download tool (​www.purpleair.com/sensorlist​) were corrected on the 20th October 
2019.
- SD Data: Column order does not change. The same data is written to the same 
columns. Only the headers were updated to swap CF=ATM and CF=1 labels.
- Custom data providers using JSON: CF=ATM and CF=1 labels will be swapped in f
irmware V5.00 and up. All firmware prior to V5.00 will have the CF=ATM and CF=1 
labels swapped in any JSON.
- Weather Underground data is not affected and always had CF=ATM data uploaded 
to the service.
How to determine what data is what
Since CF=ATM values only differ from CF=1 values at higher concentrations, you 
will not see any difference between the values till the concentrations reach 
around 50ug/m3. ​**The CF=ATM values will always be the lower of the two, so it 
would be safe to assume that if one field is lower in value, that field is the 
CF=ATM value.**
 
##Summary:

| Plantower PMS Sensor Bytes Data | PA JSON <= 4.11 | PA JSON >= 5.0 | Thingspeak | Weather Underground
---- | ---- | ---- | ---- | ---- | ----
4,5 | PM1.0 CF=1 | pm1_0_atm | pm1_0_cf_1 | field1 - primary data |
6,7 | PM2.5 CF=1 | pm2_5_atm | pm2_5_cf_1 | field2 - primary data |
8,9 | PM10 CF=1 | pm10_0_atm | pm10_0_cf_1 | field3 - primary data | 
10,11 | PM1.0 CF=ATM | pm1_0_cf_1 | pm1_0_atm | field7 - secondary data | Uploaded as PM1.0
12,13 | PM2.5 CF=ATM | pm2_5_cf_1 | pm2_5_atm | field8 - primary data | Uploaded as PM2.5
14,15 | PM10 CF=ATM | pm10_0_cf_1 | pm10_0_atm | field8 - secondary data | Uploaded as PM10


## Questions? Contacting PurpleAir

We hope this document helped to explain the differences and labeling of CF=ATM 
and CF=1 values.

If you have any questions or concerns about this or other topics, please contact 
PurpleAir by email at ​contact@purpleair.com​, by phone on 1-800-474-0696 or on 
our web site at www.purpleair.com/contact​.

