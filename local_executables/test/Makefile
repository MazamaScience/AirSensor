################################################################################
# Makefile for testing scripts against mazamascience/airsensor:latest
#

# A little Makefile magic from:  https://www.gnu.org/software/make/manual/html_node/File-Name-Functions.html#File-Name-Functions
BASE_PATH:=$(realpath ..)

# And a target to test it
show_base_path:
	echo $(BASE_PATH)

# Clean up generated files
clean:
	-rm -rf $(BASE_PATH)/output
	-rm -rf $(BASE_PATH)/logs
	
setup:
	-mkdir -p $(BASE_PATH)/output
	-mkdir -p $(BASE_PATH)/logs
	-touch $(BASE_PATH)/logs/cron_log.txt

################################################################################
# PAS objects

createPAS: setup
	docker run --rm -v $(BASE_PATH):/app -v $(BASE_PATH)/output:/app/output -v $(BASE_PATH)/logs:/app/logs -w /app mazamascience/airsensor:latest /app/createPAS_exec.R --outputDir=/app/output --logDir=/app/logs >> $(BASE_PATH)/logs/cron_log.txt 2>&1 

createPAS_archival: setup
	docker run --rm -v $(BASE_PATH):/app -v $(BASE_PATH)/output:/app/output -v $(BASE_PATH)/logs:/app/logs -w /app mazamascience/airsensor:latest /app/createPAS_archival_exec.R --outputDir=/app/output --logDir=/app/logs >> $(BASE_PATH)/logs/cron_log.txt 2>&1 


################################################################################
# PAT objects

createLatestPAT: setup
	docker run --rm -v $(BASE_PATH):/app -v $(BASE_PATH)/output:/app/output -v $(BASE_PATH)/logs:/app/logs -w /app mazamascience/airsensor:latest /app/createLatestPAT_exec.R --outputDir=/app/output --logDir=/app/logs --pattern=^SCNP_..$ >> $(BASE_PATH)/logs/cron_log.txt 2>&1 

createMonthlyPAT: setup
	docker run --rm -v $(BASE_PATH):/app -v $(BASE_PATH)/output:/app/output -v $(BASE_PATH)/logs:/app/logs -w /app mazamascience/airsensor:latest /app/createMonthlyPAT_exec.R --outputDir=/app/output --logDir=/app/logs --pattern=^SCNP_..$ >> $(BASE_PATH)/logs/cron_log.txt 2>&1 

createAprPAT: setup
	docker run --rm -v $(BASE_PATH):/app -v $(BASE_PATH)/output:/app/output -v $(BASE_PATH)/logs:/app/logs -w /app mazamascience/airsensor:latest /app/createMonthlyPAT_exec.R --outputDir=/app/output --logDir=/app/logs --datestamp=201904 --pattern=^SCNP_..$ >> $(BASE_PATH)/logs/cron_log.txt 2>&1 

createOctPAT: setup
	docker run --rm -v $(BASE_PATH):/app -v $(BASE_PATH)/output:/app/output -v $(BASE_PATH)/logs:/app/logs -w /app mazamascience/airsensor:latest /app/createMonthlyPAT_exec.R --outputDir=/app/output --logDir=/app/logs --datestamp=201710 --pattern=^SCSB_..$ >> $(BASE_PATH)/logs/cron_log.txt 2>&1 


################################################################################
# AirSensor objects

createLatestAirSensor: setup
	docker run --rm -v $(BASE_PATH):/app -v $(BASE_PATH)/output:/app/output -v $(BASE_PATH)/logs:/app/logs -w /app mazamascience/airsensor:latest /app/createLatestAirSensor_exec.R --outputDir=/app/output --logDir=/app/logs --pattern=^SCNP_..$ >> $(BASE_PATH)/logs/cron_log.txt 2>&1 

createMonthlyAirSensor: setup
	docker run --rm -v $(BASE_PATH):/app -v $(BASE_PATH)/output:/app/output -v $(BASE_PATH)/logs:/app/logs -w /app mazamascience/airsensor:latest /app/createMonthlyAirSensor_exec.R --outputDir=/app/output --logDir=/app/logs --pattern=^SCNP_..$ >> $(BASE_PATH)/logs/cron_log.txt 2>&1 


createAnnualAirSensor: setup
	docker run --rm -v $(BASE_PATH):/app -v $(BASE_PATH)/output:/app/output -v $(BASE_PATH)/logs:/app/logs -w /app mazamascience/airsensor:latest /app/createAnnualAirSensor_exec.R --outputDir=/app/output --logDir=/app/logs >> $(BASE_PATH)/logs/cron_log.txt 2>&1


################################################################################
# Videos

createVideo: setup
	docker run --rm -v $(BASE_PATH):/app -v $(BASE_PATH)/output:/app/output -v $(BASE_PATH)/logs:/app/logs -w /app mazamascience/airsensor:latest /app/createVideo_exec.R --outputDir=/app/output --logDir=/app/logs --communityID=SCSB >> $(BASE_PATH)/logs/cron_log.txt 2>&1 




