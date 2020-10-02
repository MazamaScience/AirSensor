################################################################################
# Makefile for building and running docker containers

AirSensor_tarball:
	cd ..; R CMD build --resave-data .
	mv ../AirSensor_1.0.2.tar.gz .

# PRODUCTION version -----------------------------------------------------------

production_build:
	docker build --no-cache -t mazamascience/airsensor:1.0.2 -t mazamascience/airsensor:latest .

production_publish:
	docker login && docker push mazamascience/airsensor:1.0.2


