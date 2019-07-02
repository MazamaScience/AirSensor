################################################################################
# Makefile for building and running docker containers

# PRODUCTION version -----------------------------------------------------------

AirSensor_tarball:
	cd ..; R CMD build .
	mv ../AirSensor_0.3.5.tar.gz .

production_build:
	docker build --no-cache -t mazamascience/airsensor:0.3.5 -t mazamascience/airsensor:latest .

