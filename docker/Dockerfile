FROM mazamascience/pwfslsmoke:1.2.9
MAINTAINER "Jonathan Callahan" jonathan@mazamascience.com

#######################################################################
# For PWFSLSmoke

# Install from CRAN to get all dependent packages
RUN apt-get update \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/ \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
  ## Install extra/updated R packages from CRAN
  && install2.r --error -r "https://cran.rstudio.com" \
    dplyr \
    GGally \
    seismicRoll

################################################################################
# For MazamaSpatialUtils

# Add CARB spatial data required by AirSensor
RUN wget -nv http://mazamascience.com/RData/Spatial/CA_AirBasins_01.RData \
  -O /home/mazama/data/Spatial/CA_AirBasins_01.RData


#######################################################################
# For AirSensor (while the Github repo is private)

WORKDIR /

COPY AirSensor_0.3.5.tar.gz /

RUN R CMD INSTALL AirSensor_0.3.5.tar.gz

