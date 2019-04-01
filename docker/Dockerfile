FROM mazamascience/spatialutils:0.6.1

#######################################################################
# For PWFSLSmoke

# Install from CRAN to get all dependent packages
RUN apt-get update \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/ \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
  ## Install R packages from CRAN
  && install2.r --error -r "https://cran.rstudio.com" \
    PWFSLSmoke

# Install from Github for the latest updates
RUN installGithub.r \
    mazamascience/pwfslsmoke

#######################################################################
# For MazamaPurpleAir

# Install from local .tar.gz file
RUN R CMD CMD INSTALL MazamaPurpleAir.tar.gz

