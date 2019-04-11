################################################################################
# Makefile for building and running docker containers

# PRODUCTION version -----------------------------------------------------------
production_build:
	cd ..; R CMD build .
	mv ../AirSensor_0.1.8.tar.gz .
	docker build --no-cache -t mazamascience/mazamapurpleair:0.1.8 -t mazamascience/mazamapurpleair:latest .

