################################################################################
# Makefile for building and running docker containers

TAG=$(shell git tag --points-at HEAD)
ifeq ($(TAG),)
$(error The latest commit must be tagged in order to build or publish a docker image)
endif

# PRODUCTION version -----------------------------------------------------------

AirSensor_tarball:
	cd ..; R CMD build --resave-data .
	mv ../AirSensor_0.8.6.tar.gz .

production_build:
	docker build --no-cache -t mazamascience/airsensor:${TAG} -t mazamascience/airsensor:latest .

production_publish:
	docker login && docker push mazamascience/airsensor:${TAG}


