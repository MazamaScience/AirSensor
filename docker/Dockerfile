FROM mazamascience/pwfslsmoke:1.2.117
MAINTAINER "Jonathan Callahan" jonathan@mazamascience.com

#######################################################################
# For PWFSLSmoke

# Install from CRAN to get all dependent packages
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    ffmpeg \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/ \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
  ## Install extra/updated R packages from CRAN
  && install2.r --error -r "https://cran.rstudio.com" \
    cowplot \
    GGally \
    MazamaCoreUtils \
    MazamaLocationUtils \
    seismicRoll \
    worldmet

################################################################################
# For MazamaSpatialUtils

# Add CARB spatial data required by AirSensor
RUN wget -nv http://data.mazamascience.com/MazamaSpatialUtils/Spatial_0.7/CA_AirBasins_01.rda \
  -O /home/mazama/data/Spatial/CA_AirBasins_01.rda


#######################################################################
# For AirSensor (while the Github repo is private)

WORKDIR /

COPY AirSensor_1.1.1.tar.gz /

RUN R CMD INSTALL AirSensor_1.1.1.tar.gz

