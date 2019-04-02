################################################################################
# Makefile for building and running docker containers

# PRODUCTION version -----------------------------------------------------------
production_build:
	cd ..; R CMD build .
	mv ../MazamaPurpleAir_0.1.6.tar.gz .
	docker build --no-cache -t mazamascience/mazamapurpleair:0.1.6 -t mazamascience/mazamapurpleair:latest .

