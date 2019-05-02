################################################################################
# Makefile for building and running docker containers

# PRODUCTION version -----------------------------------------------------------
production_build:
	cd ..; R CMD build .
	mv ../AirSensor_0.2.7.tar.gz .
	docker build --no-cache -t mazamascience/airsensor:0.2.7 -t mazamascience/airsensor:latest .

