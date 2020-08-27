################################################################################
# Makefile for building and running docker containers

AirSensor_tarball:
	cd ..; R CMD build --resave-data .
	mv ../AirSensor_0.9.11.tar.gz .

# PRODUCTION version -----------------------------------------------------------

production_build:
	docker build --no-cache -t mazamascience/airsensor:0.9.11 -t mazamascience/airsensor:latest .

production_publish:
	docker login && docker push mazamascience/airsensor:0.9.11


