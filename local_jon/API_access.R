#
# See: https://api.purpleair.com
#

# ----- Setup ------------------------------------------------------------------

# Set this manually
READ_KEY <- ""


# ----- Get sensors via bbox ---------------------------------------------------

# > library(MazamaSpatialUtils)
# > setSpatialDataDir("~/Data/Spatial")
# > loadSpatialData("USCensusStates")
# > ia <- subset(USCensusStates, stateCode == "IA")
# > bbox(ia)
# min       max
# x -96.6397 -90.14006
# y  40.3755  43.50120

# URL built with using the API page:
# GET https://api.purpleair.com/v1/sensors?fields=longitude%2Clatitude&nwlng=-96.6397&nwlat=43.5012&selng=-90.14006&selat=40.3755 HTTP/1.1

url <- "https://api.purpleair.com/v1/sensors?fields=longitude%2Clatitude&nwlng=-96.6397&nwlat=43.5012&selng=-90.14006&selat=40.3755"

r <- httr::GET(
  url,
  httr::add_headers("X-API-Key" = READ_KEY)
)

# Handle the response
status_code <- httr::status_code(r)
content <- httr::content(r, encoding="UTF-8")

# > names(content)
# [1] "api_version"     "time_stamp"      "data_time_stamp" "max_age"         "fields"          "data"

# api_version --
#   The version of API that generated the response presented as V<frontend>-<backend>.
#
# time_stamp:
#  The servers UTC (Unix) time stamp from when the response was generated in 
#  SECONDS since 1 January 1970. All time stamps use this same format.
#
# data_time_stamp:
#  The servers UTC (Unix) time stamp from when the data was loaded from the 
#  database in SECONDS since 1 January 1970.
#
# max_age:
#  Filter results to only include sensors modified or updated within the last 
#  number of seconds. Using a value of 0 will match sensors of any age.

# From:  https://stackoverflow.com/questions/4227223/convert-a-list-to-a-data-frame
df <- data.frame(
  matrix(unlist(content$data), nrow = length(content$data), byrow = TRUE), 
  stringsAsFactors = FALSE
)
names(df) <- unlist(content$fields)




