FROM mazamascience/pwfslsmoke:1.2.2
MAINTAINER "Jonathan Callahan" jonathan@mazamascience.com

#######################################################################
# For PWFSLSmoke

# Install from CRAN to get all dependent packages
RUN apt-get update \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/ \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
  ## Install R packages from CRAN
  && install2.r --error -r "https://cran.rstudio.com" \
    optparse \
    seismicRoll

#######################################################################
# For MazamaPurpleAir (while the Github repo is private)

WORKDIR /

COPY MazamaPurpleAir_0.1.8.tar.gz /

RUN R CMD INSTALL MazamaPurpleAir_0.1.8.tar.gz

