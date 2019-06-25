FROM rocker/shiny:3.6.0

LABEL maintainer="hans@mazamascience.com" \
      maintainer="jon@mazamascience.com"
 
LABEL build_date="2019-06-25"
LABEL version="1.1"

LABEL description="AirShiny Docker Image"

# Install general libraries 
RUN apt-get update && apt-get install libcurl4-openssl-dev libv8-3.14-dev -y 

# Install AirSensor library Dependencies
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    xtail \
    wget \
    libudunits2-dev \ 
    libssl-dev \
    libprotobuf-dev \
    libjq-dev \
    libxml2-dev \
    libv8-dev \
    protobuf-compiler \
    libgdal-dev \
    libfreetype6-dev \
    libmagick++-dev \
    cargo \
    libx11-dev \
    mesa-common-dev \
    libglu1-mesa-dev

# Create directories for shiny
RUN mkdir -p /var/lib/shiny-server/bookmarks/shiny

# Create directories for AirSensor
RUN mkdir -p /home/mazama/data/Spatial

# Add CARB spatial data required by AirSensor
RUN wget -nv http://mazamascience.com/RData/Spatial/CA_AirBasins_01.RData \
  -O /home/mazama/data/Spatial/CA_AirBasins_01.RData

# Download and install R dependencies
RUN install2.r --error -r "https://cran.rstudio.com" \
	shiny \
	V8 \ 
	shinyjs \ 
	countrycode \ 
	dplyr \ 
	dygraphs \ 
	geosphere \ 
	GGally \ 
	ggmap \ 
	ggplot2 \
	httr \ 
	jsonlite \ 
	leaflet \ 
	lubridate \ 
	MazamaCoreUtils \ 
	MazamaSpatialUtils \ 
	plyr \ 
	PWFSLSmoke \ 
	RColorBrewer \ 
	rlang \ 
	seismicRoll \ 
	sp \ 
	stringr \ 
	tidyr \ 
	magrittr \ 
	xts \ 
	zoo \ 
	shinythemes

# Copy app directory to image 
COPY app /srv/shiny-server/

# Allow readable files
RUN chmod -R 755 /srv/shiny-server/

# Copy the AirSensor package to image
COPY AirSensor_0.3.1.tar.gz /

# Install the AirSensor package
RUN R CMD INSTALL AirSensor_0.3.1.tar.gz

# Port 
EXPOSE 3838

# Run 
CMD ["/usr/bin/shiny-server.sh"] 
