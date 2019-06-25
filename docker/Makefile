################################################################################
# Makefile for building and running docker containers

# PRODUCTION version -----------------------------------------------------------

AirSensor_tarball: AirSensor_0.3.1.tar.gz
	cd ..; R CMD build .
	mv ../AirSensor_0.3.1.tar.gz .
 

production_build: AirSensor_tarball
	docker build --no-cache -t mazamascience/airsensor:0.3.1 -t mazamascience/airsensor:latest .

