################################################################################
# Makefile for building and running docker containers

# PRODUCTION version -----------------------------------------------------------

AirSensor_tarball:
	cd ..; R CMD build .
	mv ../AirSensor_0.4.9.tar.gz .

production_build:
	docker build --no-cache -t mazamascience/airsensor:0.4.9 -t mazamascience/airsensor:latest .
