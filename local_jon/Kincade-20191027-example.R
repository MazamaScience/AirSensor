#
# Can we ingest only the data within a bbox?
#
# This script grabs chunks of code from inside a variety of other functions
# to demonstrate the possibility of reading in only the data within a bbox.

library(MazamaSpatialUtils)
setSpatialDataDir("~/Data/Spatial")
loadSpatialData("USCensusStates")
loadSpatialData("USCensusCounties")

library(MazamaSatelliteUtils)
setSatelliteDataDir("~/Data/Satellite")

# ----- Open a netcdf file -----------------------------------------------------

# ncFile <- system.file(
#   "extdata",
#   "OR_ABI-L2-AODC-M6_G16_s20192491826095_e20192491828468_c20192491835127.nc",
#   package = "MazamaSatelliteUtils"
# )

files <- goesaodc_downloadAOD("G17", 2019102714, timezone = "America/Los_Angeles")
files <- goesaodc_listFiles("G17", 2019102714, timezone = "America/Los_Angeles")

nc <- goesaodc_openFile(basename(files[1]))

# ----- Get grid data ----------------------------------------------------------

# Get satID from netCDF, will be either "G16" or "G17"
satID <- ncdf4::ncatt_get(nc, varid = 0, attname = "platform_ID")$value

# Choose which gridFile to load based on satID
if ( satID == "G16") {
  gridFile <- "goesEastGrid.rda"
} else if ( satID == "G17" ) {
  gridFile <- "goesWestGrid.rda"
}

# Assemble the correct filepath based on satID and Data directory
filePath <- file.path(getSatelliteDataDir(), gridFile)

# Test for grid existence and if found, load it. Stop with appropriate message
# if missing
if ( file.exists(filePath) ) {
  goesGrid <- get(load(filePath))
} else {
  stop("Grid file not found. Run 'installGoesGrids()' first")
}  

# ----- Set up our bbox --------------------------------------------------------

# bbox_oregon <- c(-124.56624, -116.46350, 41.99179, 46.29203)
# 
# lonLo <- bbox_oregon[1]
# lonHi <- bbox_oregon[2]
# latLo <- bbox_oregon[3]
# latHi <- bbox_oregon[4]

ca <- subset(USCensusStates, stateCode == "CA")
bbox <- bbox(ca)

lonLo <- bbox[1]
lonHi <- bbox[2]
latLo <- bbox[3]
latHi <- bbox[4]

# Kincade fire
lonLo <- -124
lonHi <- -120
latLo <- 36
latHi <- 39

# ----- Name things what they are to ease understanding ------------------------

# Matrices of the same dimensions as AOD and DQF
lonMatrix <- goesGrid$longitude
latMatrix <- goesGrid$latitude

# Create a matrix of logicals identifying grid cells within bbox_oregon
gridMask <-
  lonMatrix >= lonLo &
  lonMatrix <= lonHi &
  latMatrix >= latLo &
  latMatrix <= latHi

# > class(gridMask)
# [1] "matrix"
# > summary(as.logical(gridMask))
# Mode   FALSE    TRUE    NA's 
# logical 3678696   24142   47162 

gridMask[is.na(gridMask)] <- FALSE

# image(gridMask[200:300,200:300])

# ----- Find the i,j bounding box in curvilinear grid space --------------------

# Find the first row in each column inside the bbox
iLos <- apply(gridMask, 2, function(x) { min(which(x)) })
iLo <- min(iLos) # lots of Inf but that's OK

# Last row
iHis <- apply(gridMask, 2, function(x) { max(which(x)) })
iHi <- max(iHis) # lots of -Inf but that's OK

# First column
jLos <- apply(gridMask, 1, function(x) { min(which(x)) })
jLo <- min(jLos) # lots of Inf but that's OK

# Last column
jHis <- apply(gridMask, 1, function(x) { max(which(x)) })
jHi <- max(jHis) # lots of -Inf but that's OK

# Convert to the variables we pass to ncvar_get()
start_x <- iLo
count_x <- iHi - iLo + 1

start_y <- jLo
count_y <- jHi - jLo + 1

# ----- Code borrowed from goesaodc_createTibble() -----------------------------

varList <- list()

# Get subset lons and lats from the original grid file
varList[["lon"]] <- as.numeric( lonMatrix[iLo:iHi,jLo:jHi] )
varList[["lat"]] <- as.numeric( latMatrix[iLo:iHi,jLo:jHi] )

# Get AOD using start and count arguments
varList[["AOD"]] <- as.numeric(ncdf4::ncvar_get(
  nc, 
  varid = "AOD",
  start = c(start_x, start_y),
  count = c(count_x, count_y),
  verbose = FALSE,
  signedbyte = TRUE,
  collapse_degen = TRUE,
  raw_datavals = FALSE
))

# Get DQF using start and count arguments
varList[["DQF"]] <- as.numeric(ncdf4::ncvar_get(
  nc, 
  varid = "DQF",
  start = c(start_x, start_y),
  count = c(count_x, count_y),
  verbose = FALSE,
  signedbyte = TRUE,
  collapse_degen = TRUE,
  raw_datavals = FALSE
))

# Create a tibble
tbl <-
  tibble::as_tibble(varList) %>%
  tidyr::drop_na()

# ----- Code borrrowed from goesaodc_createSpatialPoints() ---------------------

spatialPoints <- sp::SpatialPointsDataFrame(
  coords = dplyr::select(tbl, c(.data$lon, .data$lat)),
  data = dplyr::select(tbl, -c(.data$lon, .data$lat))
)

# use 10^AOD
spatialPoints@data$AOD <- 10^spatialPoints@data$AOD

# ----- The proof is in the pudding --------------------------------------------

maps::map('state', 'california', xlim = c(lonLo, lonHi), ylim = c(latLo, latHi))
usr <- par('usr')
graphics::rect(usr[1], usr[3], usr[2], usr[4], col = "gray80")
goesaodc_plotSpatialPoints(spatialPoints, add = TRUE, 
                           cex = 0.5, n = 5e5,
                           breaks = c(0,5,10,20,40,Inf),
                           colBins = 5,
                           paletteName = "YlOrRd")
maps::map('county', 'california', col = "white", add = TRUE)
maps::map('state', 'california', lwd = 1.5, add = TRUE)

