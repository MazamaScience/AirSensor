
## NOTE: Originally I was downloading data from the link below, 
## now I see otherwise. 

# Lines of code for deciding which stations seems to have more reliable data:


lon <- 149.19
lat <- -35.30
year <- 2020

lon <- pat_downer$meta$longitude
lat <- pat_downer$meta$latitude

closestSite <- worldmet::getMeta(lon = lon, lat = lat, n = 1, 
                                 plot = FALSE)[1,]
siteCode <- paste0(closestSite$USAF, "-", closestSite$WBAN)
siteData <- worldmet::importNOAA(code = siteCode, year = year, 
                                 parallel = FALSE)
windData <- dplyr::select(siteData, c("date", "wd", "ws"))




# original wind data formatting:
wind2020_orig <- read.csv("/Users/kayleigh/Data/Australia_on_fire/2020.csv", header = TRUE)
wind2019_orig <- read.csv("/Users/kayleigh/Data/Australia_on_fire/2019.csv", header = TRUE)

wind2019 <- 
  wind2019_orig %>%
  dplyr::select(date = DATE, wd = HourlyWindDirection, ws = HourlyWindSpeed)

wind2019$date <- MazamaCoreUtils::parseDatetime(wind2019$date, timezone = "Australia/Canberra")


wind2020 <- 
  wind2020_orig %>%
  dplyr::select(date = DATE, wd = HourlyWindDirection, ws = HourlyWindSpeed)

wind2020$date <- MazamaCoreUtils::parseDatetime(wind2020$date, timezone = "Australia/Canberra")

canberra_airportLat <- wind2020_orig$LATITUDE
canberra_airportLon <- wind2020_orig$LONGITUDE

wind <- dplyr::bind_rows(wind2019, wind2020)
min_date <- MazamaCoreUtils::parseDatetime(20191201, timezone = "Australia/Canberra")
wind_filt <- dplyr::filter(wind, date >= min_date)
wind_filt <- unique(wind_filt)


canberra_wind <- dplyr::filter(wind_filt, lubridate::minute(wind_filt$date) == 0)

filename <- c("canberra_wind.rda")
filepath <- file.path("/Users/kayleigh/Data/Australia_on_fire/", filename)
save(list = "canberra_wind", file = filepath)


# search site to search manually: https://www.ncei.noaa.gov/access/search/dataset-search
# Example API URL from the site:
# https://www.ncei.noaa.gov/access/services/data/v1?dataset=global-marine&dataTypes=WIND_DIR,WIND_SPEED&stations=AUCE&startDate=2016-01-01&endDate=2016-01-02&boundingBox=90,-180,-90,180



