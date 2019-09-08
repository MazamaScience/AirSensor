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
	

################################################################################
# PAS objects

createPAS:
	docker run --rm -v $(BASE_PATH):/app -v $(BASE_PATH)/output:/app/output -v $(BASE_PATH)/logs:/app/logs -w /app mazamascience/airsensor:latest /app/createPAS_exec.R --outputDir=/app/output --logDir=/app/logs >> $(BASE_PATH)/logs/cron_log.txt 2>&1 

createPAS_archival:
	docker run --rm -v $(BASE_PATH):/app -v $(BASE_PATH)/output:/app/output -v $(BASE_PATH)/logs:/app/logs -w /app mazamascience/airsensor:latest /app/createPAS_archival_exec.R --outputDir=/app/output --logDir=/app/logs >> $(BASE_PATH)/logs/cron_log.txt 2>&1 


################################################################################
# PAT objects

createLatestPAT:
	docker run --rm -v $(BASE_PATH):/app -v $(BASE_PATH)/output:/app/output -v $(BASE_PATH)/logs:/app/logs -w /app mazamascience/airsensor:latest /app/createLatestPAT_exec.R --outputDir=/app/output --logDir=/app/logs --pattern=^SCNP_..$ >> $(BASE_PATH)/logs/cron_log.txt 2>&1 

createMonthlyPAT:
	docker run --rm -v $(BASE_PATH):/app -v $(BASE_PATH)/output:/app/output -v $(BASE_PATH)/logs:/app/logs -w /app mazamascience/airsensor:latest /app/createMonthlyPAT_exec.R --outputDir=/app/output --logDir=/app/logs --pattern=^SCNP_..$ >> $(BASE_PATH)/logs/cron_log.txt 2>&1 


################################################################################
# AirSensor objects

createLatestAirSensor:
	docker run --rm -v $(BASE_PATH):/app -v $(BASE_PATH)/output:/app/output -v $(BASE_PATH)/logs:/app/logs -w /app mazamascience/airsensor:latest /app/createLatestAirSensor_exec.R --outputDir=/app/output --logDir=/app/logs --pattern=^SCNP_..$ >> $(BASE_PATH)/logs/cron_log.txt 2>&1 

createMonthlyAirSensor:
	docker run --rm -v $(BASE_PATH):/app -v $(BASE_PATH)/output:/app/output -v $(BASE_PATH)/logs:/app/logs -w /app mazamascience/airsensor:latest /app/createMonthlyAirSensor_exec.R --outputDir=/app/output --logDir=/app/logs --pattern=^SCNP_..$ >> $(BASE_PATH)/logs/cron_log.txt 2>&1 


################################################################################
# Videos

createVideo:
	docker run --rm -v $(BASE_PATH):/app -v $(BASE_PATH)/output:/app/output -v $(BASE_PATH)/logs:/app/logs -w /app mazamascience/airsensor:latest /app/createVideo_exec.R --outputDir=/app/output --logDir=/app/logs --communityID=SCSB >> $(BASE_PATH)/logs/cron_log.txt 2>&1 

################################################################################
# Calendar plots

createCalendarPlot:
	docker run --rm -v $(BASE_PATH):/app -v $(BASE_PATH)/output:/app/output -v $(BASE_PATH)/logs:/app/logs -w /app mazamascience/airsensor:latest /app/createCalendarPlot_exec.R --outputDir=/app/output --logDir=/app/logs --pattern=^SCNP_..$ >> $(BASE_PATH)/logs/cron_log.txt 2>&1 



